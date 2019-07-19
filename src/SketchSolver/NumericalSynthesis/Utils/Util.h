#pragma once
#include <map>
#include <vector>
#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#else
#include "FakeGSL.h"
#endif
#include "BooleanNodes.h"
#include "CommandLineArgs.h"


class Util {
public:
	static map<int, int> getNodeToValMap(map<int, int>& inputMap, vector<int>& inputs) {
		map<int, int> res;
		for(auto i = 0; i < inputs.size(); i++) {
			if (inputs[i] == 0 || inputs[i] == 1) {
				res[inputMap[i]] = inputs[i];
			}
		}
		return res;
	}

    static bool isAbsolute(bool_node* n) {
        if (n->type != bool_node::LT) {
            return false;
        }
        bool_node* v;
        if (n->mother->type == bool_node::CONST && ((CONST_node*) (n->mother))->getFval() == 0) {
            v = n->father;
        } else if (n->father->type == bool_node::CONST && ((CONST_node*) (n->father))->getFval() == 0) {
            v = n->mother;
        } else {
            return false;
        }
        FastSet<bool_node>& children = n->children;
        if (children.size() > 1) return false;
        for(child_iter it = children.begin(); it != children.end(); ++it) {
            if ((*it)->type == bool_node::ARRACC) {
                ARRACC_node* ac = (ARRACC_node*)(*it);
                if (ac->mother != n) {
                    return false;
                }
                bool_node* m = ac->multi_mother[0];
                bool_node* f = ac->multi_mother[1];
                if (m != v && f != v) {
                    return false;
                }
                if (m == v) {
                    if (f->type != bool_node::NEG || f->mother != v) {
                        return false;
                    }
                }
                if (f == v) {
                    if (m->type != bool_node::NEG || m->mother != v) {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }
        cout << "Absolute node: " << n->lprint() << endl;
        return true;
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
                for (auto i = 0; i < parents.size(); i++) {
                    toVisit.push_back(parents[i]);
                }
            }
        }
        return ids;
    }
    
    static string print(const gsl_vector* v) {
        stringstream s;
        for (int i = 0; i < v->size; i++) {
            s << gsl_vector_get(v, i) << ";";
        }
        return s.str();
    }
    static string print(const gsl_vector* v, string delimiter) {
        stringstream s;
        for (int i = 0; i < v->size; i++) {
            s << gsl_vector_get(v, i) << delimiter;
        }
        return s.str();
    }
    
    static string print(double* arr, int len) {
        stringstream s;
        for (int i = 0; i < len; i++) {
            s << arr[i] << ";";
        }
        return s.str();
    }

    static string print(const vector<int>& v) {
        stringstream s;
        for (auto it = v.begin(); it != v.end(); it++) {
            s << *it << ";";
        }
        return s.str();
    }

    static string print(const vector<double>& v) {
        stringstream s;
        for (auto it = v.begin(); it != v.end(); it++) {
            s << *it << ";";
        }
        return s.str();
    }

    static string print(const set<int>& v) {
        stringstream s;
        for (auto it = v.begin(); it != v.end(); it++) {
            s << *it << ";";
        }
        return s.str();
    }

    static double getMin(const vector<double>& vals) {
        double minval = 1e30;
        for (int i = 0; i < vals.size(); i++) {
            if (vals[i] < minval) {
                minval = vals[i];
            }
        }
        return minval;
    }

    static double norm(const gsl_vector* v) {
        return gsl_blas_dnrm2(v);
    }

    static bool sameDir(const gsl_vector* v1, const gsl_vector* v2) {
        double dp = 0.0;
        gsl_blas_ddot(v1, v2, &dp);
        dp = dp/(norm(v1) * norm(v2));
        //cout << "dot product: " << dp << endl;
        if (dp > 0.9) return true;
        return false;
    }

    static string benchName() {
        string s = PARAMS->inputFname;
        int x1 = s.rfind(".sk");
        int x2 = s.rfind("/tmp/");
        return s.substr(x2+5, x1-x2 - 5);

    }

    static vector<string> split(string &text, string sep) {
        vector<string> tokens;
        size_t start = 0, end = 0;
        while ((end = text.find(sep, start)) != string::npos) {
            tokens.push_back(text.substr(start, end - start));
            start = end + 1;
        }
        tokens.push_back(text.substr(start));
        return tokens;
    }

};
