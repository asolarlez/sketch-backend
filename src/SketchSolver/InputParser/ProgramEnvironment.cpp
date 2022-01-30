//
// Created by kliment on 12/20/21.
//

#include "ProgramEnvironment.h"
#include "SketchFunction.h"

void findPureFuns(const map<string, BooleanDAG *> &functionMap, set<string> &pureFuns) {

    for (auto it = functionMap.begin(); it != functionMap.end(); ++it) {
        auto ctrlvec = it->second->getNodesByType(bool_node::CTRL);
        if (ctrlvec.size() == 0) {
            pureFuns.insert(it->first);
            continue;
        }
        if (ctrlvec.size() == 1 && ctrlvec[0]->get_name() == "#PC") {
            pureFuns.insert(it->first);
        }
    }

    set<string> other;
    do{
        other = pureFuns;
        for (auto it = pureFuns.begin(); it != pureFuns.end(); ++it) {
            assert(functionMap.find(*it) != functionMap.end());
            BooleanDAG* bd = functionMap.at(*it);

            auto ufvec = bd->getNodesByType(bool_node::UFUN);
            for (auto ufit = ufvec.begin(); ufit != ufvec.end(); ++ufit ) {

                UFUN_node* ufn = dynamic_cast<UFUN_node*>(*ufit);
                if (ufn == NULL) { continue;  }
                if (other.count(ufn->get_ufname()) == 0) {
                    //calling a non-pure function means you are not pure either.
                    other.erase(*it);
                    break;
                }
            }
        }
        swap(other, pureFuns);
    } while (other.size() != pureFuns.size());

}

FunctionMap &boolean_dag_map_to_function_map(map<string, BooleanDAG *> &boolean_dag_map, ProgramEnvironment *the_env)
{
    auto* ret = new FunctionMap();

    for(const auto& it: boolean_dag_map)
    {
        ret->insert(it.first, new SketchFunction(it.second, nullptr, the_env));
    }

    return *ret;
};
