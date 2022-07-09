//
// Created by kliment on 7/12/21.
//

#include "CounterexampleFinder.h"
#include "File.h"

CounterexampleFinder::Result
CounterexampleFinder::fromFile(const File *file, FloatManager &floats, vector<bool_node *> &inputNodes)  {
    AssertDebug(file != nullptr, "file shouldn't be nullptr.");
#ifdef CHECK_FILE_INVARIANT
    assert(check_file_invariant(file));
#endif
    for(int i = 0;i<file->size();i++) {
        bool ok = parseLine(file->at(i));
        assert(ok);
        if (PARAMS->verbosity > 12) {
            inputs->printContent(cout);
        }
        bool rv = this->run(*inputs);
        if (rv) {
#ifdef CHECK_FILE_INVARIANT
            assert(file->get_used(i) == 0);
            file->set_used(i);
#endif
            return FOUND;
        }
    }
    return UNSAT;
}
