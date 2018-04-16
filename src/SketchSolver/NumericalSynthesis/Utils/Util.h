#pragma once
#include <map>
#include <vector>
#include <gsl/gsl_vector.h>
#include "BooleanNodes.h"

class Util {
public:
	static map<int, int> getNodeToValMap(map<int, int>& inputMap, vector<int>& inputs) {
		map<int, int> res;
		for(int i = 0; i < inputs.size(); i++) {
			if (inputs[i] == 0 || inputs[i] == 1) {
				res[inputMap[i]] = inputs[i];
			}
		}
		return res;
	}
    
    static bool isSqrt(bool_node* n) {
        if (n->type != bool_node::UFUN) {
            return false;
        }
        UFUN_node* un = (UFUN_node*) n;
        if (un->get_ufname() == "sqrt_math") {
            return true;
        }
        return false;
    }
    
    static bool hasArraccChild(bool_node* n) {
        FastSet<bool_node>& children = n->children;
        for(child_iter it = children.begin(); it != children.end(); ++it) {
            if ((*it)->type == bool_node::ARRACC && (*it)->mother == n) {
                return true;
            }
        }
        return false;
    }
    
    static bool hasAssertChild(bool_node& n) {
        FastSet<bool_node>& children = n.children;
        if (children.size() > 1) return false;
        for(child_iter it = children.begin(); it != children.end(); ++it) {
            if ((*it)->type == bool_node::ASSERT) {
                return true;
            }
        }
        return false;
    }
    
    static bool hasNotAssertChild(bool_node& n) {
        FastSet<bool_node>& children = n.children;
        if (children.size() > 1) return false;
        for(child_iter it = children.begin(); it != children.end(); ++it) {
            if ((*it)->type == bool_node::NOT) {
                FastSet<bool_node>& grandChildren = (*it)->children;
                if (grandChildren.size() > 1) return false;
                for (child_iter it1 = grandChildren.begin(); it1 != grandChildren.end(); ++it1) {
                    if ((*it1)->type == bool_node::ASSERT) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    
    static bool hasAssertChild(bool_node* n) {
        return hasAssertChild(*n);
    }
    
    static bool hasNotAssertChild(bool_node* n) {
        return hasNotAssertChild(*n);
    }
    
    static set<int> getRelevantNodes(bool_node* n) {
        set<int> ids;
        set<int> visitedIds;
        vector<bool_node*> toVisit;
        toVisit.push_back(n);
        
        while(toVisit.size() > 0) {
            bool_node* node = toVisit.back();
            toVisit.pop_back();
            if (visitedIds.find(node->id) == visitedIds.end()) {
                visitedIds.insert(node->id);
                ids.insert(node->id);
                const vector<bool_node*>& parents = node->parents();
                for (int i = 0; i < parents.size(); i++) {
                    toVisit.push_back(parents[i]);
                }
            }
        }
        return ids;
    }
    
    static string print(const gsl_vector* v) {
        stringstream s;
        for (int i = 0; i < v->size; i++) {
            s << gsl_vector_get(v, i) << ", ";
        }
        return s.str();
    }
    
    static string print(const set<int>& v) {
        stringstream s;
        for (auto it = v.begin(); it != v.end(); it++) {
            s << *it << ", ";
        }
        return s.str();
    }
};
