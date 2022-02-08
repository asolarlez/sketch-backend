//
// Created by kliment on 2/8/22.
//

#include "VarStore.h"

bool VarStore::has_original_name(const string &original_name) const {
    return original_name_to_name.find(original_name) != original_name_to_name.end();
}
