#pragma once

#include <fstream>
#include <iostream>
#include <map>

#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "Util.h"
#include "BooleanToCNF.h"
#include "MiniSATSolver.h"
#include "Predicate.h"

class vstate{
public:
    int nodeid;
    int level;
    vstate(int _nodeid, int _level): nodeid(_nodeid), level(_level) {}
};

class Predicate;

class IClause {
    vector<Predicate*> predicates;
    vector<int> vals;
public:
    int creationTime;

    IClause(int time) {
        creationTime = time;
    }

    void add(Predicate* p, int val) {
        predicates.push_back(p);
        vals.push_back(val);
    }

    Predicate* getPredicate(int idx) {
        return predicates[idx];
    }

    int getVal(int idx) {
        return vals[idx];
    }

    int size() {
        return vals.size();
    }

    string print() {
        stringstream s;
        for (int i = 0; i < vals.size(); i++) {
            s << "(" << predicates[i]->print() << ":" << vals[i] << ") ";
        }
        return s.str();
    }
};



class Interface {
    vector<int> nodeVals; // node id -> val
    set<int> inputNodeIds; // node ids that have been set by the SAT solver
    vec<vstate> stack;
    const int EMPTY = INT32_MIN;
    BooleanDAG* dag;
    map<int, Predicate*> nodeToPredicates;
    int ncontrols;
    int curStackLevel = 0;

public:
    SolverHelper* satSolver;  
    map<int, map<int, int>> varsMapping; // node id -> val -> Lit 
    vector<vector<IClause*>> clauseLevels;
    vector<set<Predicate*>> levelPredicates;

    
    Interface(BooleanDAG* dag_): dag(dag_) {
        nodeVals.resize(dag->size(), EMPTY);
        MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
        satSolver = new SolverHelper(*ms);
        set<Predicate*> s;
        levelPredicates.push_back(s);

        vector<bool_node*>& ctrlNodes = dag->getNodesByType(bool_node::CTRL);
        int ctr = 0;
        for (int i = 0; i < ctrlNodes.size(); i++) {
            if (ctrlNodes[i]->getOtype() == OutType::FLOAT || ctrlNodes[i]->getOtype() == OutType::BOOL) {
                ctr++;
            }
        }
        ncontrols = ctr;
        // if ncontrols = 0, make it 1 just so numerical opt does not break
        if (ncontrols == 0) {
            ncontrols = 1;
        }
    }
    ~Interface() {
        delete &satSolver->getMng();
        delete satSolver;
        for (int i = 0; i < clauseLevels.size(); i++) {
            for (int j = 0; j < clauseLevels[i].size(); j++) {
                delete clauseLevels[i][j];
            }
        }
        for (int i = 0; i < levelPredicates.size(); i++) {
            for (auto it = levelPredicates[i].begin(); it != levelPredicates[i].end(); it++) {
                delete *it;
            }
        }
    }
    
    void add(Tvalue& tv, bool_node& node, SolverHelper& dir) {
        Assert(node.getOtype() != OutType::FLOAT, "This should not happen");
        if (Util::isAbsolute(&node)) { // if the node is part of abs(x) do not add to the interface. 
            return;
        }
        if (node.type == bool_node::LT) { 
            Predicate* p = new BasicPredicate(dag, node.id);
            nodeToPredicates[node.id] = p;
            levelPredicates[0].insert(p);
        }
        if (node.type == bool_node::AND || node.type == bool_node::OR) {
            return;
            /*Predicate* p1 = nodeToPredicates[node.mother->id];
            Predicate* p2 = nodeToPredicates[node.father->id];
            cout << node.lprint() << endl;
            Predicate* dp = new DiffPredicate(p1, p2, ncontrols);
            cout << p1->print() << endl;
            cout << p2->print() << endl;
            cout << dp->print() << endl;
            //levelPredicates[0].insert(dp);*/
        } 
        
        if (tv.isBvect()) {
            map<int, int> m;
            m[1] = tv.getId(); m[0] = -tv.getId();
            varsMapping[node.id] = m;
        } else {
            const gvvec& vec = tv.num_ranges;
            map<int, int> m;
            for (gvvec::const_iterator ci = vec.begin(); ci != vec.end(); ++ci) {
                m[ci->value] = ci->guard;
            }
            varsMapping[node.id] = m;
        }
    }

    void add(Predicate* p, int level) {
        if (level < levelPredicates.size()) {
            levelPredicates[level].insert(p);
        } else {
            Assert(level == levelPredicates.size(), "Some thing is wrong with levels"); 
            set<Predicate*> s;
            levelPredicates.push_back(s);
            levelPredicates[level].insert(p);
        }
    }
    
    int size() {
        return varsMapping.size();
    }
    
    pair<int, int> backtrack(int level) {
        int i;
        int j = 0;
        pair<int, int> res;
        for (i = stack.size() - 1; i >= 0; --i) {
            vstate& ms = stack[i];
            if (ms.level > level) {
                res = make_pair(ms.nodeid, nodeVals[ms.nodeid]);
                nodeVals[ms.nodeid] = EMPTY;
                inputNodeIds.erase(ms.nodeid);
                ++j;
            } else {
                break;
            }
        }
        stack.shrink(j);
        curStackLevel = level;
        return res;
    }
    
    int getLit(int nodeid, int val) {
        return varsMapping[nodeid][val];
    }

    
    void setInput(int nodeid, int val, int level) {
        Assert(!hasValue(nodeid), "Node already has a value");
        Assert(level == curStackLevel || level == curStackLevel + 1, "Wrong stack level");

        nodeVals[nodeid] = val;
        stack.push(vstate(nodeid, level));
        inputNodeIds.insert(nodeid);
        curStackLevel = level;
        /*int lit = getLit(nodeid, val);
        cout << "Trying: " << lit << endl;
        if (satSolver->tryAssignment(lit)) {
            nodeVals[nodeid] = val;
            stack.push(vstate(nodeid, 1));
            inputNodeIds.insert(nodeid);
            return true;
        } 
        return false;*/
    }

    void setInput(int nodeid, int val) {
        setInput(nodeid, val, curStackLevel + 1);
    }

    bool clearLastLevel() {
        pair<int, int> last = backtrack(curStackLevel - 1);
        if (curStackLevel == -1) {
            curStackLevel = 0;
            return false;
        } else {
            setInput(last.first, !last.second, curStackLevel);
            return true;
        }
        //satSolver->getMng().cancelLastDecisionLevel();
    }

    void restartInputs() {
        // clean inputs in the interface
        backtrack(0);
        // clear assignments in the sat solver
        //satSolver->getMng().reset();
    }


    void addClause(IClause* clause, int level) {
        if (level < numLevels()) {
            clauseLevels[level].push_back(clause);
        } else { 
            Assert(level == numLevels(), "This is not possible");
            vector<IClause*> clauses;
            clauses.push_back(clause);
            clauseLevels.push_back(clauses);
        }
        for (int i = 0; i < clause->size(); i++) {
            add(clause->getPredicate(i), level + 1);
        }
    }

    void removeOldClauses() {
        vector<IClause*> removedClauses;
        for (auto it = clauseLevels[0].begin(); it != clauseLevels[0].end(); it++) {
            IClause* c = *it;
            if (c->creationTime < GradUtil::counter - 5) {
                removedClauses.push_back(c);
            }
        }
        for (int i = 0; i < removedClauses.size(); i++) {
            clauseLevels[0].erase(remove(clauseLevels[0].begin(), clauseLevels[0].end(), removedClauses[i]), clauseLevels[0].end());
        }
        // TODO: need to also remove predicates from levelPredicates
    }
    
    int getValue(int nodeid) {
        return nodeVals[nodeid];
    }
    
    const set<int>& getInputConstraints() {
        return inputNodeIds;
    }
    
    int numSet() {
        return inputNodeIds.size();
    }

    int numLevels() {
        return clauseLevels.size();
    }

    int numClauses(int level) {
        Assert(level < numLevels(), "dquwhe");
        return clauseLevels[level].size();
    }

    int totalClauses() {
        int sum = 0;
        for (int i = 0; i < numLevels(); i++) {
            sum += numClauses(i);
        }
        return sum;
    }
    
    bool hasValue(int nodeid) {
        return nodeVals[nodeid] != EMPTY;
    }
    
    bool isInput(int nodeid) {
        return varsMapping.find(nodeid) != varsMapping.end();
    }    

    void printInputs() {
        cout << "Inputs: ";
        for (auto it = inputNodeIds.begin(); it != inputNodeIds.end(); it++) {
            cout << "(" << *it << "," << nodeVals[*it] << "); ";
        }
        cout << endl;

        cout << "Clauses:" << endl;
        for (int i = 0; i < clauseLevels.size(); i++) {
            cout << "Level: " << i << endl;
            for (int j = 0; j < clauseLevels[i].size(); j++) {
                cout << clauseLevels[i][j]->print() << endl;
            }
        }
    }
};
