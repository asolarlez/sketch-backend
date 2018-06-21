#pragma once

#include <fstream>
#include <iostream>
#include <map>

#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "Util.h"
#include "BooleanToCNF.h"
#include "MiniSATSolver.h"


class vstate{
public:
    int nodeid;
    int level;
    vstate(int _nodeid, int _level): nodeid(_nodeid), level(_level) {}
};

class Interface {
    vector<int> nodeVals; // node id -> val
    set<int> inputNodeIds; // node ids that have been set by the SAT solver
    vec<vstate> stack;
    const int EMPTY = INT32_MIN;


public:
    SolverHelper* satSolver;  
    map<int, map<int, int>> varsMapping; // node id -> val -> Lit 
  
    
    Interface(int dagSize) {
        nodeVals.resize(dagSize, EMPTY);
        MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
        satSolver = new SolverHelper(*ms);
    }
    ~Interface() {
        delete &satSolver->getMng();
        delete satSolver;
    }
    
    void add(Tvalue& tv, bool_node& node, SolverHelper& dir) {
        Assert(node.getOtype() != OutType::FLOAT, "This should not happen");
        if (Util::isAbsolute(&node)) { // if the node is part of abs(x) do not add to the interface. 
            return;
        }
        if (tv.isBvect()) {
            map<int, int> m;
            m[1] = tv.getId(); m[0] = -tv.getId();
            cout << node.id << " " << m[1] << " " << m[0] << endl;
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
    
    int size() {
        return varsMapping.size();
    }
    
    void backtrack(int level) {
        int i;
        int j = 0;
        for (i = stack.size() - 1; i >= 0; --i) {
            vstate& ms = stack[i];
            if (ms.level > level) {
                nodeVals[ms.nodeid] = EMPTY;
                inputNodeIds.erase(ms.nodeid);
                ++j;
            } else {
                break;
            }
        }
        stack.shrink(j);
    }

    void popLast() {
        int stack_size = stack.size();
        vstate& ms = stack[stack_size - 1];
        nodeVals[ms.nodeid] = EMPTY;
        inputNodeIds.erase(ms.nodeid);
        stack.shrink(1);
    }
    
    int getLit(int nodeid, int val) {
        return varsMapping[nodeid][val];
    }

    bool tryInput(int nodeid, int val) {
        if (hasValue(nodeid)) {
            return false;
        } else {
            nodeVals[nodeid] = val;
            stack.push(vstate(nodeid, 1));
            inputNodeIds.insert(nodeid);
            return true;
        }
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

    void restartInputs() {
        // clean inputs in the interface
        backtrack(0);
        // clear assignments in the sat solver
        //satSolver->getMng().reset();
    }

    void clearLastInput() {
        popLast();
        //satSolver->getMng().cancelLastDecisionLevel();
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
    }
};
