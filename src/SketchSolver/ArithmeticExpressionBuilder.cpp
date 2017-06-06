#include "ArithmeticExpressionBuilder.h"
#include "ArithmeticExpressionSolver.h"


void getOutputsSubset(const vector<int>& outputs, const vector<bool>& setOutputs, vector<int>& outputsSubset) {
	Assert(outputs.size() == setOutputs.size(), "Outputs and setOutputs should have the same size" + to_string(outputs.size()) + " " + to_string(setOutputs.size()) + " " + to_string(outputsSubset.size()));
	for (int i = 0; i < outputs.size(); i++) {
		if (setOutputs[i]) {
			outputsSubset.push_back(outputs[i]);
		}
	}
}

ArithExpression* ArithExprBuilder::addToMaps(ArithType op, ArithExpression* ae1, ArithExpression* ae2, int d, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs) {
	ArithType lop = ae1->getOp();
	ArithType rop;
	string fsig;
	string msig = ae1->getSig();
	if (ae2 != NULL) {
		rop = ae2->getOp();
		fsig = ae2->getSig();
	}
	else {
		Assert(ArithExpression::isUnary(op), "op should be unary");
	}
	//TODO: Check if the expression is "simplifiable" or
	// "canonicalizable" -> reject those
	
	/*if (ae1->isCommutative(op)){
	 //(a+b)+(c+d) - make sure a<=b<=c<=d (sig) - issue with depth? nope.
	 // + can be replaced with *
	 if(lop == op && rop == op){
	 string b = ae1->getFather()->getSig();
	 string c = ae2->getMother()->getSig();
	 if (b>c) {
	 return NULL;
	 }
	 }
	 //(a+b)+c - make sure a<=b<=c (sig) - issue with depth? -> check c->depth != maxdepth-1
	 if(lop == op && ae2->getDepth() < maxDepth-1 ){
	 string b = ae1->getFather()->getSig();
	 string c = ae2->getSig();
	 if(b>c){
	 return NULL;
	 }
	 }
	 //a+(b+c) - make sure a<=b<=c (sig) - issue with depth? -> check a->depth != maxdepth-1
	 if(rop == op && ae1->getDepth() < maxDepth-1 ){
	 string a = ae1->getSig();
	 string b = ae2->getMother()->getSig();
	 if(a>b){
	 return NULL;
	 }
	 }
	 }
	 if (op == Plus){
	 //(a*b)+(c*b) -> symmetries?
	 if(lop == rop && lop == Times){
	 string a = ae1->getMother()->getSig();
	 string b = ae1->getFather()->getSig();
	 string c = ae2->getMother()->getSig();
	 string d = ae2->getFather()->getSig();
	 if (a == c || a == d || b == c || b ==d) {
	 return NULL;
	 }
	 }
	 //(a%b)+(c%b)
	 //(a/b)+(c/b)
	 if(lop == rop && (lop == Mod || lop == Div) && ae1->getFather()->getSig() == ae2->getFather()->getSig() ){
	 return NULL;
	 }
	 //a+a if 2 \in consts and Times \in ops
	 if(ae1->getSig() == ae2->getSig() && ops.find(Times) != ops.end() && consts.find(2) != consts.end()) return NULL;
	 }
	 if (op == Div){
	 // x / x (if C1 in consts)
	 if(ae1->getSig() == ae2->getSig()){
	 return NULL;
	 }
	 // (a/b)/c = a/(b*c) "canonical" OR a/(b/c) = (a*c)/b
	 if (lop == Div || rop == Div) {
	 return NULL;
	 }
	 // x/C1
	 if(rop == Const && ae2->val == 1) return NULL;
	 
	 //(a%b) / b = a%b
	 if(lop == Mod && ae1->getFather()->getSig() == ae2->getSig()){
	 return NULL;
	 }
	 
	 //1/x is 0 or 1
	 if(lop == Const && (ae1->val ==1 || ae1->val == 0)){
	 return NULL;
	 }
	 
	 }
	 if(op == Mod){
	 // x % x (if C0 in consts)
	 if(ae1->getSig() == ae2->getSig()){
	 return NULL;
	 }
	 // (a%b)%c = a%c "canonical"
	 if (lop == Mod) {
	 return NULL;
	 }
	 // x%C1 or C0 (note x%C1 == 0 but if we needed 0, we should just add it)
	 if(rop == Const && (ae2->val == 1 || ae2->val == 0)) return NULL;
	 
	 //const % x?
	 if(lop == Const && (ae1->val == 1 || ae1->val == 0)) return NULL;
	 
	 //(a/b) % b = a
	 if(lop == Div && ae1->getFather()->getSig() == ae2->getSig()){
	 return NULL;
	 }
	 }
	 if (op == Times){
	 //(a/b) * c not canon
	 if (lop == Div || rop == Div) return NULL;
	 //a * 0 or a* 1
	 if(lop == Const && (ae1->val == 1 || ae1->val == 0)) return NULL;
	 if(rop == Const && (ae2->val == 1 || ae2->val == 0)) return NULL;
	 }*/
	
	
	/*if (ArithExpression::isCommutative(op)) {
		if (msig > fsig) {
	 return NULL;
		}
	 }*/
#ifdef SWAPPER
	set<ArithExpression*> dOps = ae1->dagOps;
	if (ae2 != NULL) {
		dOps.insert(ae2->dagOps.begin(), ae2->dagOps.end());
	}
	if (syn->maxDagSize > 0 && dOps.size() >= syn->maxDagSize) {
		/*if (syn->printdebug) {
		 cout << "Ignoring large DAG expression: opl=" << ae1->getSig()<<"opr= "<<((ae2!= NULL) ? ae2->getSig() : "NULL") << endl;
		 }*/
		//Note, we don't even build this node in memory or hash this node
		return NULL;
		//This is a bigger pattern than needed
	}
#endif
	string checkSig;
	if (ae2 != NULL) {
		checkSig = "(" + msig + ArithExpression::AT2str(op) + fsig + ")";
	}
	else {
		checkSig = "(" + ArithExpression::AT2str(op) + msig + ")";
	}
	//cout<<"Adding expression: "<<checkSig<<endl;
	
	ArithExpression* ae;
	if (ASigMap.find(checkSig) == ASigMap.end()) {
		//cannot find sig
		ae = new ArithExpression(op, ae1, ae2, !repeatVars);
		ASigMap[checkSig] = ae;
	}
	else {
		ae = ASigMap[checkSig];
	}
	if (!repeatVars && ae->repeated) {
		return NULL;
	}
	
	vector<int> outputs;
	bool hasNoUndefOutputs = ae->getOutputs(exprOutMap, outputs);
	if (!hasNoUndefOutputs) {
		return NULL;
	}
	
	vector<int> outputsSubset;
	getOutputsSubset(outputs, setOutputs, outputsSubset);
	Assert(inputs.size() == outputs.size(), "outputs not correctly evaluated");
	
	if ((!outIsBit || ae->isBoolean) && outputsSubset == neededOutputs && checkUnsetOutputs(exampleIds, setOutputs, outputs)) {
		ASetMap[d].insert(ae);
		syn->addSuggestions(exampleIds, setOutputs, outputs);
		return ae;
	}
	if (repeatVars) {
		auto it = outputsABigMap.find(outputs);
		if (it != outputsABigMap.end()) {
			ArithExpression* aeold = it->second;
			if (false && ae->isBoolean && !aeold->isBoolean) {//NOT DOING THIS, NOT SOUND
				//if this new expression outputs bit
				//and the old one doesn't (but they both output same values modulo current examples)
				
#ifdef PRINTDEBUG
				cout << "Replacing " << aeold->getSig() << " with " << ae->getSig() << " due to bit/int output difference" << endl;
#endif
				int dold = aeold->getDepth();
				int numErased = ASetMap[dold].erase(aeold);
				Assert(numErased == 1, "this expression should have been removed from AsetMap D1: numErased= " + to_string(numErased) + aeold->getSig());
				
				if (dold < d) {
					aeold->isDead = true; // for marking this node unusable for the iteration,
					//we will reset it after every synthesis call
				}
				exprOutMap.erase(aeold);
				exprOutMap[ae] = outputs;
				it->second = ae;
				ASetMap[d].insert(ae);
			}
#ifdef SWAPPER
			else if (ae->isBoolean == aeold->isBoolean && ae->dagOps.size() < aeold->dagOps.size() && syn->maxDagSize > 0) {
				//found a smaller and equivalent DAG pattern
#ifdef PRINTDEBUG
				cout << "Replacing " << aeold->getSig() << " with " << ae->getSig() << " due to dag size constraint" << endl;
#endif
				int dold = aeold->getDepth();
				int numErased = ASetMap[dold].erase(aeold);
				Assert(numErased == 1, "this expression should have been removed from AsetMap D1: numErased= " + to_string(numErased) + aeold->getSig());
				
				if (dold < d) {
					aeold->isDead = true; // for marking this node unusable for the iteration,
					//we will reset it after every synthesis call
				}
				exprOutMap.erase(aeold);
				exprOutMap[ae] = outputs;
				it->second = ae;
				ASetMap[d].insert(ae);
			}
#endif
			return NULL;
		}
		else {
			outputsABigMap[outputs] = ae;
			auto it = outputsASmallMap.find(outputsSubset);
			if (it != outputsASmallMap.end()) {
				it->second.push_back(outputs);
			} else {
				vector<vector<int>> fullOutputs;
				fullOutputs.push_back(outputs);
				outputsASmallMap[outputsSubset] = fullOutputs;
			}
			if (d < maxDepth) {
				ASetMap[d].insert(ae);
				exprOutMap[ae] = outputs;
			}
		}
	}
	else {
		// TODO: currently not doing the above optimization when no repeat vars
		if (d < maxDepth) {
			ASetMap[d].insert(ae);
			exprOutMap[ae] = outputs;
		}
		return NULL;
	}
	return NULL;
	
}

// all setOutputs[i] is false if it is partial output. otherwise true
ArithExpression* ArithExprBuilder::getExpressionFromSmallMap(const vector<int>& outputs, const vector<int>& exampleIds, const vector<bool>& notUnsetOutputs) {
	auto it = outputsASmallMap.find(outputs);
	if (it != outputsASmallMap.end()) {
		auto& emap = it->second; // vector of full outputs
		for (int i = 0; i < emap.size(); i++) {
			// check if unset outputs are ok
			if (checkUnsetOutputs(exampleIds, notUnsetOutputs, emap[i])) {
				syn->addSuggestions(exampleIds, notUnsetOutputs, emap[i]);
				return outputsABigMap[emap[i]];
			}
		}
		return NULL;
	}
	else {
		return NULL;
	}
}

ArithExpression* ArithExprBuilder::generateSmallMap(const vector<int>& needOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<bool>& notUnsetOutputs) {
	outputsASmallMap.clear();
	
	for (auto it = outputsABigMap.begin(); it != outputsABigMap.end(); it++) {
		auto& outputs = it->first;
		vector<int> outputsSubset;
		getOutputsSubset(outputs, setOutputs, outputsSubset);
		ArithExpression* ae = it->second;
		if ((!outIsBit || ae->isBoolean) && outputsSubset == needOutputs && checkUnsetOutputs(exampleIds, notUnsetOutputs, outputs)) {
			syn->addSuggestions(exampleIds, notUnsetOutputs, outputs);
			return ae;
		} else {
			auto it = outputsASmallMap.find(outputsSubset);
			if (it != outputsASmallMap.end()) {
				it->second.push_back(outputs);
			} else {
				vector<vector<int>> fullOutputs;
				fullOutputs.push_back(outputs);
				outputsASmallMap[outputsSubset] = fullOutputs;
			}
		}
	}
	return NULL;
}


ArithExpression* ArithExprBuilder::addToMapsD1(ArithType op, int cival, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs) {
	string checkSig;
	bool isBool = false;
	if (op == Variable) {
		checkSig = ArithExpression::AT2str(op) + to_string(cival);
		isBool = isBit[cival];
	}
	else if (op == Const) {
		checkSig = to_string(cival);
		isBool = (cival == 0 || cival == 1);
	}
	else {
		Assert(false, "depth 1 can only be var or const");
	}
	ArithExpression* ae;
	if (ASigMap.find(checkSig) == ASigMap.end()) {
		//cannot find sig
		ae = new ArithExpression(op, cival,isBool);
		ASigMap[checkSig] = ae;
	}
	else {
		ae = ASigMap[checkSig];
	}
	vector<int> outputs;
	bool hasNoUndefOutputs = ae->getOutputs(inputs, outputs);
	if (!hasNoUndefOutputs) {
		return NULL;
	}
	
	vector<int> outputsSubset;
	getOutputsSubset(outputs, setOutputs, outputsSubset);
	Assert(inputs.size() == outputs.size(), "outputs not correctly evaluated");
	if ((!outIsBit || ae->isBoolean) && outputsSubset == neededOutputs && checkUnsetOutputs(exampleIds, setOutputs, outputs)) {
		ASetMap[1].insert(ae);
		syn->addSuggestions(exampleIds, setOutputs, outputs);
		return ae;
	}
	auto it = outputsABigMap.find(outputs);
	if (it != outputsABigMap.end()) {
		ArithExpression* aeold = it->second;
		if (false && ae->isBoolean && !aeold->isBoolean) {//NOT DOING THIS, NOT SOUND
			//if this new expression is out output bit type
			//and the old one isn't
			
#ifdef PRINTDEBUG
			cout << "Replacing " << aeold->getSig() << " with " << ae->getSig() << " due to bit/int output difference" << endl;
#endif
			int dold = aeold->getDepth();
			
			int numErased = ASetMap[dold].erase(aeold);
			Assert(numErased == 1, "this expression should have been removed from AsetMap D1: numErased= " + to_string(numErased) + aeold->getSig() );
			Assert(dold == 1, "depth should be 1 for the older node");
			//aeold->isDead = true; // for marking this node unusable for the iteration, need to reset it after ecery synthesis call
			exprOutMap.erase(aeold);
			exprOutMap[ae] = outputs;
			it->second = ae;
			ASetMap[1].insert(ae);
		}
		else {
			return NULL;
		}
	}
	else {
		outputsABigMap[outputs] = ae;
		auto it = outputsASmallMap.find(outputsSubset);
		if (it != outputsASmallMap.end()) {
			it->second.push_back(outputs);
		} else {
			vector<vector<int>> fullOutputs;
			fullOutputs.push_back(outputs);
			outputsASmallMap[outputsSubset] = fullOutputs;
		}
		ASetMap[1].insert(ae);
		exprOutMap[ae] = outputs;
	}
	return NULL;
}

bool ArithExprBuilder::checkUnsetOutputs(const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<int>& outputs) {
	Assert(exampleIds.size() == setOutputs.size(), "Examples Ids and set outputs are not of the same size");
	Assert(exampleIds.size() == outputs.size(), "Examples Ids and outputs are not of the same size");
	for (int i = 0; i < exampleIds.size(); i++) {
		if (!setOutputs[i]) {
			if (!syn->isValidOutput(exampleIds[i], outputs[i])) {
				return false;
			}
		}
	}
	return true;
}
