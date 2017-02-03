#ifndef ARITHMETICEXPRESSIONSOLVER_H_
#define ARITHMETICEXPRESSIONSOLVER_H_

#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <algorithm>
#include <utility>
#include "Sort.h"
#include <math.h>
using namespace std;

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"
#include "ArithmeticExpressionBuilder.h"
class ArithExprSyn : public Synthesizer {
	ArithExpression* expr;
	int depth;
	int numvars;
	set<int> consts;
	vector <ArithType> ops;
	ArithExprBuilder* ab;
	vector< vector<int> > prevInputs;
	vector<int> prevExampleIds;
	vector<bool> prevSetOutputs;
	bool completeBigMap;
	bool completeSmallMap;
	string name;
	timerclass arithtimer;
	bool repeatVars;
	
	vector<Lit> sugLits;
	map <pair<int, int>, bool> validOutsMap;
	vector<bool> isBit;
	bool outIsBit;
public:
#ifdef SWAPPER
	int maxDagSize;
#endif
	
	ArithExprSyn(FloatManager& _fm) :Synthesizer(_fm) {
		expr = NULL;
		depth = 3;
		numvars = 3;
		ops.push_back(Plus);
		ops.push_back(Times);
		consts = set<int>();
		ab = NULL;
		completeBigMap=false;
		completeSmallMap=true;
		outIsBit = false;
		repeatVars = true;
		//We set these params based on name of the uninterp function
		//while registering the function
	}
	~ArithExprSyn(){
		if (ab != NULL){
			delete ab;
		}
	}
	void setupParams(string name) {
		this->name = name;
		string ncopy = name;
		replace(ncopy.begin(), ncopy.end(), '_', ' ');
		stringstream ss(ncopy);
		string s;
		ss >> s;
		Assert(s == "GEN", "invalid name: " + name);
		ss >> s;
		Assert(s == "arithexpr", "invalid name: " + name);
		ops.clear();
		int ind = 0;
		outIsBit = false;
#ifdef SWAPPER
		maxDagSize = -1;
#endif
		while (ss >> s) {
			if (s == "b") {
				isBit.push_back(true);
				ind++;
			}
			else if (s == "i") {
				isBit.push_back(false);
				ind++;
			}
			else if (s == "outb") {
				outIsBit = true;
			}
			else if (s == "outi") {
				outIsBit = false; // this is default
			}
			else if (s == "plus") ops.push_back(Plus);
			else if (s == "times") ops.push_back(Times);
			else if (s == "div") ops.push_back(Div);
			else if (s == "mod") ops.push_back(Mod);
			else if (s == "lt") ops.push_back(Lt);
			else if (s == "and") ops.push_back(And);
			else if (s == "or") ops.push_back(Or);
			else if (s == "neg") ops.push_back(Neg);
			else if (s == "not") ops.push_back(Not);
			else if (s == "xor") ops.push_back(Xor);
			else if (s == "eq") ops.push_back(Eq);
			else if (s[s.length() - 1] == 'd') {
				depth = atoi(s.substr(0, s.length() - 1).c_str());
				Assert(depth >= 1, "invalid depth: " + s);
			}
#ifdef SWAPPER
			else if (s[s.length() - 1] == 'n') {
				maxDagSize = atoi(s.substr(0, s.length() - 1).c_str());
				Assert(maxDagSize >= 1, "invalid maxDagSize: " + s);
				cout << "maxDagSize set to: " << maxDagSize << endl;
			}
#endif
			else if (s[s.length() - 1] == 'v') {
				numvars = atoi(s.substr(0, s.length() - 1).c_str());
				Assert(numvars >= 1, "invalid numvars: " + s);
			}
			else if (s[0] == 'c') {
				consts.insert(atoi(s.substr(1, s.length() - 1).c_str()));
			}
			else if (s == "norepeat") {
				repeatVars = false;
			}
			else {
				Assert(false, "Cannot parse name of arithexpr: token=" + s);
			}
			
		}
#ifdef SWAPPER
		Assert((ops.size() == 11 || ops.size() == 6) && maxDagSize > 0, "there should be 11 ops and maxDag constraint");
#endif
		Assert(isBit.size() == numvars, "Inconsistent evaluation numvars != spec for isBit");
		if (ops.empty()) {
			ops.push_back(Plus);
			ops.push_back(Times);
		}
		setupBuilder();
	}
	void addConst(int c){
		consts.insert(c);
	}
	void setOps(vector<ArithType> opsinp){
		ops = opsinp;
	}
	void setDepth(int d){
		depth = d;
	}
	
	void setNumVariables(int nv){
		numvars = nv;
	}
	void setupBuilder(){
		cout << "Repeat vars: " << repeatVars << endl;
		ab = new ArithExprBuilder(numvars, consts, ops, depth, repeatVars, isBit, outIsBit, this);
	}
	
	virtual void finalize() {
		//Called after inout matrix is final but before it is destroyed
	}
	
	void addConflicts(vector<int>& conflictIds, InputMatrix& im) {
		for (auto& conflictId : conflictIds) {
			addSingleConflict(conflictId,im);
		}
	}
	
	void addConflicts(vector< int > &conflictIds, vector<bool>& setOutputs, InputMatrix& im){
		for(int i = 0; i < conflictIds.size(); i++){
			//cout<<"("<<im.getVal(conflictId, tupidin)<<","<<im.getVal(conflictId, attrin)<<") ";
			if (setOutputs[i]) {
				addSingleConflict(conflictIds[i],im);
			}
		}
		set<int> conflictOnlyInputs;
		// add all false lits
		// optimization: only add those false lits that the search algorithm tried
		for (auto it = validOutsMap.begin(); it != validOutsMap.end(); it++) {
			if (it->second == false) {
				Lit l = getLit(im.valueid(it->first.first, numvars), it->first.second);
#ifdef PRINTDEBUG
				cout << "Pushing (" << it->first.first << ", " << it->first.second << ") Lit: " << toInt(l)  << endl;
#endif
				conflict.push(~l);
				conflictOnlyInputs.insert(it->first.first);
			}
		}
		
		for(auto& conflictId: conflictOnlyInputs) {
			addOnlyInputsConflict(conflictId,im);
		}
		
	}
	
	void addSingleConflict(const int &conflictId, InputMatrix& im ){
		for(int j=0;j<=numvars;j++){
			conflict.push(getLit(im.valueid(conflictId, j)));
		}
	}
	
	void addOnlyInputsConflict(const int &conflictId, InputMatrix& im) {
		for(int j=0;j<numvars;j++){
			conflict.push(getLit(im.valueid(conflictId, j)));
		}
	}
	
	bool isValidOutput(int instanceId, int val) {
		pair<int, int> p(instanceId, val);
		auto it = validOutsMap.find(p);
		if (it != validOutsMap.end()) {
			return it->second;
		}
		if (val < 0 || val > 31) {// TODO: don't hard code these numbers
			//validOutsMap[p] = false;
			return false;
		}
		Lit v = getLit(inout->valueid(instanceId, numvars), val); // TODO: this lit can be invalid?
		if (solver->value(v) == l_False) {
#ifdef PRINTDEBUG
			cout << "Not valid output: (" << instanceId << ", " << val << ") Lit: " << toInt(v) << endl;
#endif
			validOutsMap[p] = false;
			return false;
		}
		validOutsMap[p] = true;
		return true;
	}
	
	bool tryExpressions(int d) {
		validOutsMap.clear();
		//Try all expressions bounded by depth d
		InputMatrix& im = *inout;
		int nI = inout->getNumInstances();
		map<string, int> inputsMap; // track input string to idx to get rid of repeated inputs
		vector < vector < int > > inputs;
		vector < int > outputs;
		vector<int> exampleIds;
		vector<bool> setOutputs;
		
		//vector<vector<int>> onlyInputs;
		//vector<int> onlyInputsIds;
		
		for (int i = 0; i < nI; ++i) {
			stringstream ss;
			int out = im.getVal(i, numvars);
			vector<int> vals;
			bool foundEmptyVariable = false;
			for (int j = 0; j < numvars; j++) {
				int v = im.getVal(i, j);
				if (v == EMPTY) {
					foundEmptyVariable = true;
					break;
				}
				ss << v << "#";
				vals.push_back(v);
			}
			if (foundEmptyVariable) {
				continue;
			}
			if (out == EMPTY) {
				inputs.push_back(vals);
				exampleIds.push_back(i);
				setOutputs.push_back(false);
				continue;
			}
			string s = ss.str();
			if (inputsMap.find(s) != inputsMap.end()) {
				// there exists an example with the same inputs
				int idx = inputsMap[s];
				if (im.getVal(idx, numvars) != out) {
					// different outputs - conflict
#ifdef PRINTDEBUG
					cout << "Same inputs different outputs CONFLICT" << endl;
#endif
					vector<int> conflicts;
					conflicts.push_back(idx);
					conflicts.push_back(i);
					addConflicts(conflicts, im);
					return false;
				}
				// Otherwise, there is no need to add this repeated example
			}
			else {
				exampleIds.push_back(i);
				inputs.push_back(vals);
				outputs.push_back(out);
				setOutputs.push_back(true);
				inputsMap[s] = i;
			}
		}
		int nExamples = (int)inputs.size();
#ifdef PRINTDEBUG
		int j = 0;
		cout << "Synthesis" << endl;
		for (int io = 0; io < nExamples; io++) {
			cout << exampleIds[io] << " ";
			cout << "Inputs: ";
			for (int inp : inputs[io]) {
				cout << inp << " ";
			}
			if (setOutputs[io]) {
				cout << " Output: " << outputs[j++] << endl;
			} else {
				cout << endl;
			}
		}
#endif
		
		//Evaluate previous expr on each example and return true if they all satisfy
		if (expr != NULL) {
			//Assert(!outIsBit || expr->isBoolean, "Expr should be bit if the output type is bit");
			if (nExamples == 0) return true;
#ifdef PRINTDEBUG
			cout << "Prev expr: " << expr->getSig() << endl;
#endif
			bool allSatisfied = true;
			int j = 0;
			vector<int> allOutputs;
			for (int io = 0; io < nExamples; io++) {
				int outv = expr->evaluate(inputs[io]);
				if (setOutputs[io] && outv != outputs[j]) {
#ifdef PRINTDEBUG
					cout << "Failed Input: ";
					for (int inp : inputs[io]) {
						cout << inp << " ";
					}
					cout << endl;
#endif
					allSatisfied = false;
					break;
				}
				if (setOutputs[io]) j++;
				if (!setOutputs[io] && !isValidOutput(exampleIds[io], outv)) {
#ifdef PRINTDEBUG
					cout << "Failed Partial Input: ";
					for (int inp : inputs[io]) {
						cout << inp << " ";
					}
					cout << endl;
#endif
					allSatisfied = false;
					break;
				}
				allOutputs.push_back(outv);
			}
			if (allSatisfied) {
				addSuggestions(exampleIds, setOutputs, allOutputs);
				return true;
			}
		}
		else {
			Assert(nExamples == 0, "expr == NULL Can only happen in the case when nExamples == 0.");
			expr = (ab->getExpression(1, inputs, outputs, exampleIds, setOutputs));
			//TODO: this may not work if out type is bit and all level 1 expressions are int variables
			if (expr == NULL) {
				//none of the first level expressions work e.g. all of them are ints and we need a bit output
				Assert(find(isBit.begin(),isBit.end(),true) == isBit.end() && outIsBit,"this should be the case when there are no bit inputs and we need a bit output")
				expr = (ab->getExpression(2, inputs, outputs, exampleIds, setOutputs));
				Assert(expr != NULL, "there should be a two node example for boolean output!");
				//expr = ab->getFirstExpression();
				
			}
			return true;
		}
		
		if (prevInputs.size() > 0) {
			if (isSubset(inputs,prevInputs,exampleIds, prevExampleIds)) {
#ifdef PRINTDEBUG
				cout << "Inputs is subset of prev inputs" << endl;
#endif
				if (sameSetOutputs(exampleIds, setOutputs, prevExampleIds, prevSetOutputs)) {
#ifdef PRINTDEBUG
					cout << "Same subset of outputs" << endl;
#endif
					// We can just use the same small map
					ArithExpression* aePrev = ab->getExpressionFromSmallMap(outputs, prevExampleIds, generateBigNotUnSetOutputs(exampleIds, setOutputs, prevExampleIds));
					if (aePrev != NULL) {
#ifdef PRINTDEBUG
						cout << "Found expr in existing small map" << endl;
#endif
						// we are done
						expr = aePrev;
						return true;
					} else {
						// First check if the small map is complete
						if (completeSmallMap) {
							// Now check if the big map is complete
							if (completeBigMap) {
								// Conflict
#ifdef PRINTDEBUG
								cout << "Expr not found -- conflict" << endl;
#endif
								addConflicts(exampleIds, setOutputs, im);
								return false;
							} else {
								// We need to generate the big map again
							}
						} else {
							// First complete the small map
#ifdef PRINTDEBUG
							cout << "Generating small map from big map" << endl;
#endif
							ArithExpression* aePrev1 = ab->generateSmallMap(outputs, prevExampleIds, generateBigSetOutputs(exampleIds, setOutputs, prevExampleIds), generateBigNotUnSetOutputs(exampleIds, setOutputs, prevExampleIds));
							if (aePrev1 != NULL) {
#ifdef PRINTDEBUG
								cout << "Found expr in small map" << endl;
#endif
								// we are done
								expr = aePrev1;
								completeSmallMap = false;
								return true;
							} else {
								completeSmallMap = true;
								if (completeBigMap) {
									// Conflict
#ifdef PRINTDEBUG
									cout << "Expr not found -- conflict" << endl;
#endif
									addConflicts(exampleIds, setOutputs, im);
									return false;
								} else {
									// we need to generate the big map again
								}
							}
						}
					}
				} else {
#ifdef PRINTDEBUG
					cout << "Different subset of outputs - generating small map from big map" << endl;
#endif
					prevSetOutputs = generateBigSetOutputs(exampleIds, setOutputs, prevExampleIds);
					ArithExpression* aePrev = ab->generateSmallMap(outputs, prevExampleIds, prevSetOutputs, generateBigNotUnSetOutputs(exampleIds, setOutputs, prevExampleIds));
					if (aePrev != NULL) {
#ifdef PRINTDEBUG
						cout << "Found expr in small map" << endl;
#endif
						// we are done
						expr = aePrev;
						completeSmallMap = false;
						return true;
					} else {
						completeSmallMap = true;
						if (completeBigMap) {
							// Conflict
#ifdef PRINTDEBUG
							cout << "Expr not found -- conflict" << endl;
#endif
							addConflicts(exampleIds, setOutputs, im);
							return false;
						} else {
							// we need to generate the big map again
						}
					}
				}
			}
		}
		
		Assert(d >= 1, "depth should be at least 1");
		Assert(ab != NULL, "need Arithmetic Builder Instantiated by now");
		
		ab->clearSetMap();
		// Generate both the big map and the small map again
		prevInputs = inputs;
		prevExampleIds = exampleIds;
		prevSetOutputs = setOutputs;
		
		completeBigMap = false;
		completeSmallMap = true;
		for (int cDepth = 1; cDepth <= d; cDepth++) {
			
			ArithExpression* currExpr = ab->getExpression(cDepth, inputs, outputs, exampleIds, setOutputs); // TODO: should include onlyInputs
			
			if (currExpr != NULL) {
				expr = currExpr;
#ifdef PRINTDEBUG
				cout << "Used " << nExamples << " examples. ArithExpr: " << expr->getSig() << endl;
#endif
				return true;
			}
			
		}
#ifdef PRINTDEBUG
		cout << "CONFLICT" << endl;
#endif
		//if control comes here i.e.
		//if no expressions work, TODO: try greedy set cover of examples as conflict
		//for now, return all examples as the conflict
		addConflicts(exampleIds, setOutputs, im);//TODO:Can do smarter // TODO: should include onlyInputs
		completeBigMap = true;
		return false;
	}
	
	void addSuggestions(const vector<int> &inputIds, const vector<bool> &setOutputs, const vector<int> &allOutputs) {
		Assert(inputIds.size() == setOutputs.size(), "Examples Ids and set outputs are not of the same size");
		Assert(inputIds.size() == allOutputs.size(), "Examples Ids and outputs are not of the same size");
		
		for (int i = 0; i < inputIds.size(); i++) {
			if (!setOutputs[i]) {
				Lit v = getLit(inout->valueid(inputIds[i], numvars), allOutputs[i]);
				sugLits.push_back(v);
				//cout << "Suggesting (" << inputIds[i] << ", " << numvars << ") = " << outputs[i] << " Lit: " << toInt(v) << endl;
			}
		}
	}
	
	// Check if exampleIds is a subset of prevExampleIds
	// assumes that exampleIds and prevExamplesIds are sorted in increasing order
	bool isSubset(const vector<vector<int>>& inputs, const vector<vector<int>>& prevInputs, const vector<int>& exampleIds, const vector<int>& prevExampleIds) {
		
		int i = 0, j = 0;
		int n = exampleIds.size(), m = prevExampleIds.size();
		while (i < n && j < m) {
			if (exampleIds[i] == prevExampleIds[j]) {
				if (inputs[i] != prevInputs[j]) return false;
				i++;
				j++;
			}
			else if (exampleIds[i] < prevExampleIds[j]) {
				// not a subset
				return false;
			}
			else {
				j++;
			}
		}
		if (i < n) return false;
		return true;
	}
	
	// assumes that we already checked that exampleIds is a subset of prevExampleIds
	bool sameSetOutputs(const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<int>& prevExampleIds, const vector<bool>& prevSetOutputs) {
		int i = 0, j = 0;
		for(; j < prevExampleIds.size() && i < exampleIds.size(); j++) {
			if (exampleIds[i] == prevExampleIds[j]) {
				if (setOutputs[i] != prevSetOutputs[j]) return false;
				i++;
			}
		}
		return true;
	}
	
	vector<bool> generateBigNotUnSetOutputs(const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<int>& prevExampleIds) {
		vector<bool> res;
		int i = 0, j = 0;
		for(; j < prevExampleIds.size() && i < exampleIds.size(); j++) {
			if (exampleIds[i] == prevExampleIds[j]) {
				res.push_back(setOutputs[i]);
				i++;
			} else {
				res.push_back(true);
			}
		}
		for (; j < prevExampleIds.size(); j++) {
			res.push_back(true);
		}
		Assert(res.size() == prevExampleIds.size(), "size mismatch");
		return res;
	}
	
	vector<bool> generateBigSetOutputs(const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<int>& prevExampleIds) {
		vector<bool> res;
		int i = 0, j = 0;
		for(; j < prevExampleIds.size() && i < exampleIds.size(); j++) {
			if (exampleIds[i] == prevExampleIds[j]) {
				res.push_back(setOutputs[i]);
				i++;
			} else {
				res.push_back(false);
			}
		}
		for (; j < prevExampleIds.size(); j++) {
			res.push_back(false);
		}
		Assert(res.size() == prevExampleIds.size(), "size mismatch");
		return res;
		
	}
	
	
	//In[0] = tupleid, In[1] = attr , In[2] = output (bit)
	virtual bool synthesis(vec<Lit>& suggestions) {
		//x_index = 0, y_index = 1, z_index = 2,...
		//0 -> numvars-1 : indices for variables
		//numvars : index for output
		
		sugLits.clear();
#ifdef PRINTDEBUG
		arithtimer.restart();
#endif
		conflict.clear();
		bool b = tryExpressions(depth);
		suggestions.clear();
		for (int i = 0; i < sugLits.size(); i++) {
			suggestions.push(sugLits[i]);
		}
#ifdef PRINTDEBUG
		arithtimer.stop().print("ArithTime");
#endif
		return b;
		
	}
	
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
		
		//params correspond to the variables
		//generate the arithmetic expression
		bool_node* bnode = expr->getDag(dopt, params, isBit);
		if (outIsBit && !expr->isBoolean){
			//expr should output bit if output needs to be bit
			bool_node* eq = new EQ_node();
			eq->mother = bnode;
			eq->father = dopt->getCnode(1);
			eq->addToParents();
			bnode = dopt->optAdd(eq);
		}
		return bnode;
	}
	virtual void newInstance() {
		
	}
	
	virtual void print(ostream& out) {
		//cout<<"Giving to FE: "<<expr->getSig()<<" || "<< expr->getFEOut()<<endl;
		out<<expr->getFEOut();
		//out << "( SIMTH_SYNTH ( "<< simfn <<" , "<< theta << " ) )"; //IN_0 and IN_1 are inputs
		//Just text when printing, frontend language
		//Can be any valid Sketch code e.g. an uninterp function
		
	}
};

#endif
