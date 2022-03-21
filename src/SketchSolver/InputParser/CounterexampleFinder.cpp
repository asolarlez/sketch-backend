//
// Created by kliment on 7/12/21.
//

#include "CounterexampleFinder.h"
#include "File.h"

CounterexampleFinder::Result
CounterexampleFinder::fromFile(File *file, FloatManager &floats, vector<bool_node *> &inputNodes)  {
    AssertDebug(file != nullptr, "file shouldn't be nullptr.");
//		ifstream file;
//		file.open(fname);
    bool ok = true;
//
//		if (!file.is_open() || file.fail()) {
//			Assert(false, "File " << fname << " could not be opened!! file.is_open() = " << file.is_open() <<" file.fail() = " << file.fail());
//			return UNSAT;
//		}

#ifdef CHECK_FILE_INVARIANT
    {
            assert(check_file_invariant(file));
        }
#endif
//        cout << "FILE PASSES OK (in FromFile)!!" << endl;

    for(int i = 0;i<file->size();i++) {
//			try {
        ok = parseLine(file->at(i));
        assert(ok);
//			}
//			catch (BasicError& e) {
//				cerr << "Error parsing file " << fname << endl;
//				throw e;
//			}

        if (!ok) {
//				file.close();
            return MOREBITS;
        }
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
//		file.close();
    return UNSAT;
}
