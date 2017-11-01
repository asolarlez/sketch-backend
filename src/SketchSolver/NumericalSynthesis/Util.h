#pragma once
#include <map>
#include <vector>

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
};
