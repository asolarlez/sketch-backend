#pragma once

#include <fstream>
#include <iostream>
#include <map>

#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "Util.h"
#include "BooleanToCNF.h"

class NodeValPair {
public:
    int nodeid;
    int valTrue;
    int valFalse;
    NodeValPair(int nodeid_, int valTrue_, int valFalse_): nodeid(nodeid_), valTrue(valTrue_), valFalse(valFalse_) { }
};


class vstate{
public:
    int nodeid;
    int level;
    vstate(int _nodeid, int _level): nodeid(_nodeid), level(_level) {}
};

class Interface {
    map<int, map<int, Lit>> reverseVarsMapping; // node id -> val -> Lit (TODO: we should have one mapping for each instance)
                                                //                      (or have an interface for each instance)
    vector<int> nodeVals; // node id -> val
    set<int> inputNodeIds; // node ids that have been set by the SAT solver
    set<int> assertedNodeIds; // nodes ids for values that are set just by propagating asserts
    int counter;
    vec<vstate> stack;
    const int EMPTY = INT32_MIN;

public:
    map<int, NodeValPair*> varsMapping; // input id -> (node, val)
    
    
    Interface(int dagSize) {
        counter = 0;
        nodeVals.resize(dagSize, EMPTY);
    }
    
    void add(Tvalue& tv, bool_node& node, SolverHelper& dir) {
        Assert(node.getOtype() != OutType::FLOAT, "This should not happen");
        if (node.type != bool_node::LT) {
            return; // Ignore all nodes other than LT (TODO: remove this)
        }
        if (tv.isBvect()) {
            varsMapping[counter] = new NodeValPair(node.id, 1, 0);
            map<int, Lit> m;
            m[1] = lfromInt(tv.getId()); m[0] = lfromInt(-tv.getId());
            reverseVarsMapping[node.id] = m;
            dir.addNumSynSolvClause(counter, tv.getId());
            counter++;
        } else {
            const gvvec& vec = tv.num_ranges;
            map<int, Lit> m;
            for (gvvec::const_iterator ci = vec.begin(); ci != vec.end(); ++ci) {
                varsMapping[counter] = new NodeValPair(node.id, ci->value, EMPTY);
                m[ci->value] = lfromInt(ci->guard);
                dir.addNumSynSolvClause(counter, ci->guard);
                counter++;
            }
            reverseVarsMapping[node.id] = m;
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
    
    Lit getLit(int nodeid, int val) {
        return reverseVarsMapping[nodeid][val];
    }
    
    // Caller should maintain the invariant that a node's value will not be rewritten
    // This is method is critical for performace
    void pushInput(int inputid, int val, int dlevel) {
        Assert(val == 0 || val == 1, "val should be a binary");
        int nodeid = varsMapping[inputid]->nodeid;
        int nodeval = val == 1 ? varsMapping[inputid]->valTrue : varsMapping[inputid]->valFalse;
        if (nodeval != EMPTY) {
            nodeVals[nodeid] = nodeval;
            stack.push(vstate(nodeid, dlevel));
            inputNodeIds.insert(nodeid);
        }
    }
    
    int getValue(int nodeid) {
        return nodeVals[nodeid];
    }
    
    int getNodeId(int inputid) {
        return varsMapping[inputid]->nodeid;
    }
    
    
    const set<int>& getInputConstraints() {
        return inputNodeIds;
    }
    
    const set<int>& getAssertedInputConstraints() {
        return assertedNodeIds;
    }
    
    void resetInputConstraints() {
        assertedNodeIds.insert(inputNodeIds.begin(), inputNodeIds.end());
        inputNodeIds.clear();
    }
    int numSet() {
        return inputNodeIds.size();
    }
    
    bool hasValue(int nodeid) {
        return nodeVals[nodeid] != EMPTY;
    }
    
    
    
};
