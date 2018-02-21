#pragma once

#include <fstream>
#include <iostream>
#include <map>

#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "Tvalue.h"
#include "Util.h"

class NodeValPair {
    int nodeid;
    int valTrue;
    int valFalse;
public:
    NodeValPair(int nodeid_, int valTrue_, int valFalse_): nodeid(nodeid_), valTrue(valTrue_), valFalse(valFalse_) { }
};




class Interface {
    map<int, NodeValPair*> varsMapping;
    int counter;

public:
    Interface() {
        counter = 0;
    }
    
    void add(Tvalue& tv, bool_node& node, SolverHelper& dir) {
        Assert(node.getOtype() != OutType::FLOAT, "This should not happen");
        if (tv.isBvect()) {
            dir.addNumSynSolvClause(counter, tv.getId());
            varsMapping[counter] = new NodeValPair(node.id, 1, 0);
            counter++;
        } else {
            const gvvec& vec = tv.num_ranges;
            for (gvvec::const_iterator ci = vec.begin(); ci != vec.end(); ++ci) {
                dir.addNumSynSolvClause(counter, ci->guard);
                varsMapping[counter] = new NodeValPair(node.id, ci->value, EMPTY);
                counter++;
            }
        }
    }
};
