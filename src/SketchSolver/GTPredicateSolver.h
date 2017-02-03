#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <utility>
#include "Sort.h"
#include <math.h>
using namespace std;

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"



class GtpSyn : public Synthesizer {
	int theta;
public:
	GtpSyn(FloatManager& _fm) :Synthesizer(_fm) {
		
	}
	virtual bool synthesis(vec<Lit>& suggestions) {
		conflict.clear();
		int gtmin = 1000000;
		int gtid = -1;
		int ltmax = -10000000;
		int ltid = -1;
		InputMatrix& im = *inout;
		int inpt = 0;
		int outpt = 1;
		for (int i = 0; i < inout->getNumInstances(); ++i) {
			int out = im.getVal(i, outpt);
			int in = im.getVal(i, inpt);
			if (out == EMPTY || in == EMPTY) {
				continue;
			}
			if (out == 1) {
				if (in < gtmin) { gtmin = in; gtid = i; }
			}
			else {
				if (in > ltmax) { ltmax = in; ltid = i; }
			}
		}
		//im.print();
		//cout << ltmax << "-" << gtmin << endl;
		if (ltmax < gtmin) {
			theta = (ltmax + gtmin) / 2;
			return true;
		}
		else {
			if (gtid == -1) {
				theta = ltmax + 1;
				return true;
			}
			if (ltid == -1) {
				theta = gtmin - 1;
				return true;
			}
			conflict.push(getLit(im.valueid(gtid, inpt)));
			conflict.push(getLit(im.valueid(gtid, outpt)));
			conflict.push(getLit(im.valueid(ltid, inpt)));
			conflict.push(getLit(im.valueid(ltid, outpt)));
			return false;
		}
	}
	virtual void newInstance() {
		
	}
	
	virtual void finalize() {
		
	}
	
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
		return dopt->addGT(params[0], dopt->getCnode(theta));
	}
	
	virtual void print(ostream& out) {
		out << "( " << theta << "< IN_0" << ")";
	}
};
