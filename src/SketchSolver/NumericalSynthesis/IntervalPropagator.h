#pragma once

#include <vector>
#include "BasicError.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "Interval.h"

#define IP_DEBUG 1
class ientry {
public:
	Interval* interval;
	int prior;
	int nodeid;
	bool_node* reason; // nodeId responsible for the update
	
	ientry(Interval* _interval, int _prior, int _nodeid, bool_node* _reason): interval(_interval), prior(_prior), nodeid(_nodeid), reason(_reason) {}
};

class IntervalTracker {
	//Stores all the Ranges in the order in which they are added.
	//maps idx to a range and the prior index for the range of that variable.
	vector<ientry> intervals;
	//Maps every node to the latest index within ranges.
	vector<int> varmap;
	// Works as a stack. Every time a new level is seen, this stack records the id
	// level (first) and the idx where Ranges for that level start (second).
	// since levels increase monotonically, they will increase monotonically in this array.
	vector<pair<int, int> > levelmap;
	int highestlevel;
	
public:
	IntervalTracker(int nNodes) {
		highestlevel = 0;
		for (int i = 0; i < nNodes; i++) {
			varmap.push_back(-1);
		}
	}
	
	int size() {
		return intervals.size();
	}
	
	void setInterval(int nodeid, int level, Interval* interval, bool_node* reason) {
		int newid = intervals.size();
		if (level != highestlevel) {
			Assert(level > highestlevel, "This should not be possible");
			highestlevel = level;
			levelmap.push_back(make_pair(level, newid));
		}
		int oldid = varmap[nodeid];
		intervals.push_back(ientry(interval, oldid, nodeid, reason));
		varmap[nodeid] = newid;
	}
	
	Interval* getInterval(int nodeid) {
		int idx = varmap[nodeid];
		if (idx < 0) {
			return FULL_INTERVAL;
		} else {
			return intervals[idx].interval;
		}
	}
	
	Interval* getInterval(int nodeid, int& maxidx) {
		if (maxidx < 0) { maxidx = intervals.size();  }
		int idx = varmap[nodeid];
		while (true) {
			if (idx < 0) { return FULL_INTERVAL; }
			if (idx < maxidx) {
				Assert(intervals[idx].nodeid == nodeid, "Something is messed up");
				maxidx = idx;
				return intervals[idx].interval;
			}
			idx = intervals[idx].prior;
		}
	}
	
	
	bool_node* getCause(int nodeid, int& pos) {
		if (pos < 0) { pos = intervals.size(); }
		int idx = varmap[nodeid];
		while(true){
			if (idx < 0) { return NULL; }
			if (idx < pos) {
				pos = idx;
				return intervals[idx].reason;
			}
			idx = intervals[idx].prior;
		}
	}
	
	void popIntervals(int level) {
		if (level <= highestlevel) {
			int lastidx = intervals.size();
			int nelems = 0;
			for (int i = levelmap.size() - 1; i >= 0; --i) {
				int lvv = levelmap[i].first;
				highestlevel = lvv;
				if (lvv <= level) {
					break;
				}
				nelems++;
				lastidx = levelmap[i].second;
			}
			for (int i = 0; i < nelems; i++) {
				levelmap.pop_back();
			}
			if (levelmap.size() == 0) {
				highestlevel = 0;
			}
			for (int i = intervals.size() - 1; i >= lastidx; --i) {
				ientry& te = intervals[i];
				varmap[te.nodeid] = te.prior;
			}
			nelems = intervals.size() - lastidx;
			for (int i = 0; i < nelems; i++) {
				intervals.pop_back();
			}
		}
	}
	
	
	void print() {
		cout << "Printing IntervalTracker" << endl;
		cout << "Levels: ";
		for (int i = 0; i < levelmap.size(); i++) {
			cout << "(" << levelmap[i].first << "," << levelmap[i].second << "); ";
		}
		cout << endl;
		cout << "Intervals: " << endl;
		for (int i = intervals.size() - 1; i >= 0; i--) {
			cout << "idx: " << i << " nodeid: " << intervals[i].nodeid << " interval: " << intervals[i].interval->print() << " prior: " << intervals[i].prior << " reason: " << (intervals[i].reason != NULL ? intervals[i].reason->lprint() : "NULL") << endl;
		}
	}
};

class IntervalPropagator {
	vector<bool_node*> nodesToPropagate; // list of nodes that need to be propagated (acts as a trail)
	int qhead; // Next position in the trail
	IntervalTracker itracker;
	BooleanDAG& bdag;
	
	vector<bool> seen;
	
public:
	vector<bool_node*> conflictNodes;
	
	IntervalPropagator(BooleanDAG& _bdag): bdag(_bdag), itracker(_bdag.size()) {
		qhead = 0;
	}
	
	Interval* getInterval(bool_node& node) {
		return itracker.getInterval(node.id);
	}
	Interval* getInterval(bool_node* node) {
		return itracker.getInterval(node->id);
	}
	bool setInterval(bool_node& node, float low, float high, int level) {
		Assert(nodesToPropagate.size() == 0, "All nodes should be propagated");
		Assert(qhead == 0, "All nodes should be propagated");
		conflictNodes.clear();
		int nodeid = node.id;
		Interval* ointerval = getInterval(node);
		Interval* interval = new Interval(low, high);
		interval = Interval::i_intersect(interval, ointerval);
		if (!checkAndAdd(interval, ointerval, &node, level, NULL)) return false;
		bool success = propagate(level);
#if IP_DEBUG
		printCurState();
#endif
		return success;
	}
	
	void cancelUntil(int level){
		nodesToPropagate.clear(); // TODO: is this correct?
		qhead = 0;
		int oldsize = itracker.size();
		itracker.popIntervals(level);
	}
	
private:
	bool propagate(int level) {
		while (qhead < nodesToPropagate.size()) {
			bool_node* node = nodesToPropagate[qhead++];
			// propagate nodes and all of its children
			if (!propagateNode(node, level)) return false;
			FastSet<bool_node> children = node->children;
			for(child_iter it = children.begin(); it != children.end(); ++it) {
				if ((*it) != NULL && !propagateNode((*it), level)) {
					return false;
				}
			}
		}
		nodesToPropagate.clear();
		qhead = 0;
		return true;
	}
	
	bool propagateNode(bool_node* node, int level) {
		// First, do foward propagation on the node
		if (!forwardPropagate(node, level)) {
			return false;
		}
		
		// Backward propagate to all the parents
		vector<bool_node*> parents = node->parents();
		for (int i = 0; i < parents.size(); i++) {
			if (parents[i] !=NULL && !backwardPropagate(node, parents[i], level)) {
				return false;
			}
		}
		return true;
	}
	
	bool checkAndAdd(Interval* interval, Interval* ointerval, bool_node* node, int level, bool_node* reason) {
		if (interval == EMPTY_INTERVAL) {
			itracker.setInterval(node->id, level, interval, reason);
			generateConflict(node);
			return false;
		}
		if (!Interval::sameInterval(interval, ointerval)) {
			itracker.setInterval(node->id, level, interval, reason);
			nodesToPropagate.push_back(node);
		}
#if IP_DEBUG
		//cout << interval->print() << endl;
#endif
		return true;
	}
	
	bool forwardPropagate(bool_node* node, int level) {
		Assert(node != NULL, "Node should not be null");
		Interval* minterval = node->mother != NULL ? getInterval(node->mother) : NULL;
		Interval* finterval = node->father != NULL ? getInterval(node->father) : NULL;
		Interval* ointerval = getInterval(node);
		Interval* interval;
#if IP_DEBUG
		//cout << "FP: " << node->lprint() << endl;
#endif
		bool_node::Type type = node->type;
		switch(type) {
			case bool_node::SRC: {
				return true;
			}
			case bool_node::CTRL: {
				return true;
			}
			case bool_node::DST: {
				// ignore
				return true;
			}
			case bool_node::ASSERT: {
				interval = Interval::i_copy(minterval);
				break;
			}
			case bool_node::PLUS: {
				interval = Interval::i_plus(minterval, finterval);
				break;
			}
			case bool_node::TIMES: {
				interval = Interval::i_times(minterval, finterval);
				break;
			}
			case bool_node::ARRACC: {
				int size = ((ARRACC_node*)node)->multi_mother.size();
				if (minterval == EMPTY_INTERVAL) {
					Assert(false, "Not possible");
				}
				int lidx = 0;
				int hidx = size - 1;
				if (minterval != FULL_INTERVAL) {
					int low = (int) minterval->getLow();
					if (low >= 0 && low < size) {
						lidx = low;
					}
					int high = (int) minterval->getHigh();
					if (high >= 0 && high < size) {
						hidx = high;
					}
				}
				vector<Interval*> intervals;
				for (int i = lidx; i <= hidx; i++) {
					intervals.push_back(getInterval(((ARRACC_node*)node)->multi_mother[i]));
				}
				interval = Interval::i_union(intervals);
				break;
			}
			case bool_node::DIV: {
				interval = Interval::i_div(minterval, finterval);
				break;
			}
			case bool_node::MOD: {
				Assert(false, "NYI: mod");
				return false;
			}
			case bool_node::NEG: {
				interval = Interval::i_neg(minterval);
				break;
			}
			case bool_node::CONST: {
				return true;
			}
			case bool_node::LT: {
				interval = Interval::i_lt(minterval, finterval);
				break;
			}
			case bool_node::EQ: {
				interval = Interval::i_equal(minterval, finterval);
				break;
			}
			case bool_node::ARRASS: {
				Assert(false, "NYI: arrass");
				return false;
			}
			case bool_node::UFUN: {
				minterval = getInterval(((UFUN_node*)node)->multi_mother[0]);
				const string& name = ((UFUN_node*)node)->get_ufname();
				if (name == "_cast_int_float_math") {
					interval = Interval::i_cast_int_float(minterval);
				} else if (name == "arctan_math") {
					interval = Interval::i_arctan(minterval);
				} else if (name == "sin_math") {
					interval = Interval::i_sin(minterval);
				} else if (name == "cos_math") {
					interval = Interval::i_cos(minterval);
				} else if (name == "tan_math") {
					interval = Interval::i_tan(minterval);
				} else if (name == "sqrt_math") {
					interval = Interval::i_sqrt(minterval);
				} else {
						Assert(false, "NYI");
				}
				break;
			}
			case bool_node::TUPLE_R: {
				if (node->mother->type == bool_node::UFUN) {
					Assert(((UFUN_node*)(node->mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
					interval = Interval::i_copy(minterval);
				} else {
					Assert(false, "NYI");
				}
				break;
			}
			case bool_node::TUPLE_CREATE: {
				return true;
			}
			default: {
				Assert(false, "NYI");
				return false;
			}
		}
		interval = Interval::i_intersect(interval, ointerval);
		return checkAndAdd(interval, ointerval, node, level, node);
	}
	
	bool backwardPropagate(bool_node* node, bool_node* parent, int level) {
		Assert(node != NULL, "Node should not be null");
		Assert(parent != NULL, "Parent should not be null");
		bool_node* other = node->mother == parent ? node->father : node->mother;
		Interval* pinterval = getInterval(parent);
		Interval* ointerval = other != NULL? getInterval(other) : NULL;
		Interval* ninterval = getInterval(node);
#if IP_DEBUG
		//cout << "BP: " << node->lprint()  << " TO " << parent->lprint() << endl;
#endif
		bool parentIsMother = parent == node->mother;
		Interval* interval;

		bool_node::Type type = node->type;
		switch(type) {
			case bool_node::SRC: {
				Assert(false, "Source node cannot be a child");
				return false;
			}
			case bool_node::CTRL: {
				Assert(false, "CTRL node cannot be a child");
				return false;
			}
			case bool_node::DST: {
				Assert(false, "Not possible");
				return false;
			}
			case bool_node::ASSERT: {
				interval = Interval::i_copy(ninterval);
				break;
			}
			case bool_node::PLUS: {
				interval = Interval::i_minus(ninterval, ointerval);
				break;
			}
			case bool_node::TIMES: {
				if (!(ointerval->getLow() == 0 && ointerval->getHigh() == 0)) {
					interval = Interval::i_div(ninterval, ointerval);
				} else {
					interval = FULL_INTERVAL;
				}
				break;
			}
			case bool_node::ARRACC: {
				if (parentIsMother) {
					// Eliminate any indices in parent that cannot lead to values in node.
					int size = ((ARRACC_node*) node)->multi_mother.size();
					int lidx = -1;
					int hidx = -1;
					for (int i = 0; i < size; i++) {
						Interval* ointerval = getInterval(((ARRACC_node*) node)->multi_mother[i]);
						Interval* tinterval = Interval::i_equal(ointerval, ninterval);
						if (tinterval != EMPTY_INTERVAL && tinterval->getHigh() == 1) {
							if (lidx == -1) lidx = i;
							hidx = i;
						}
					}
					if (lidx == -1 || hidx == -1) {
						// all branches cannot satisfy -- conflict
						interval = EMPTY_INTERVAL;
					} else {
						interval = new Interval(lidx, hidx);
					}
				} else {
					Interval* minterval = getInterval(node->mother);
					if (minterval->getLow() == minterval->getHigh()) {
						int idx = (int) minterval->getLow();
						if (parent == ((ARRACC_node*) node)->multi_mother[idx]) {
							interval = Interval::i_copy(ninterval);
						} else {
							//we cannot decide anything
							interval = FULL_INTERVAL;
						}
					} else {
						// we cannot decide anything
						interval = FULL_INTERVAL;
					}
				}
				
				break;
			}
			case bool_node::DIV: {
				if (parentIsMother) {
					interval = Interval::i_times(ninterval, ointerval);
				} else {
					if (!(ninterval->getLow() == 0 && ninterval->getHigh() == 0)) { // if n is not zero
						interval = Interval::i_div(ointerval, ninterval);
					} else {
						interval = FULL_INTERVAL;
					}
				}
				break;
			}
			case bool_node::MOD: {
				Assert(false, "NYI: mod");
				return false;
			}
			case bool_node::NEG: {
				interval = Interval::i_neg(ninterval);
				break;
			}
			case bool_node::CONST: {
				Assert(false, "CONST node cannot be a child");
				return false;
			}
			case bool_node::LT: {
				if (ninterval->getLow() == ninterval->getHigh()) {
					float val = ninterval->getLow();
					Assert(val == 0 || val == 1, "Something is wrong LT");
					if ((val == 0 && parentIsMother) || (val == 1 && !parentIsMother)) {
						interval = new Interval(ointerval->getLow(), Interval::MAXVAL);
					} else {
						interval = new Interval(Interval::MINVAL, ointerval->getHigh());
					}
				} else {
					// cannot decide anything
					interval = FULL_INTERVAL;
				}
				break;
			}
			case bool_node::EQ: {
				if (ninterval->getLow() == ninterval->getHigh()) {
					if (ninterval->getLow() == 0) {
						// cannot decide anything
						interval = FULL_INTERVAL;
					} else {
						Assert(ninterval->getLow() == 1, "Something is wrong EQ");
						interval = Interval::i_copy(ointerval);
					}
				} else {
					// cannot decide anything
					interval = FULL_INTERVAL;
				}
				break;
			}
			case bool_node::ARRASS: {
				Assert(false, "NYI: arrass");
				return false;
			}
			case bool_node::UFUN: {
				if (parent != ((UFUN_node*)node)->multi_mother[0]) return true;
				pinterval = getInterval(((UFUN_node*)node)->multi_mother[0]);
				const string& name = ((UFUN_node*)node)->get_ufname();
				if (name == "_cast_int_float_math") {
					interval = Interval::i_cast_float_int(ninterval);
				} else if (name == "arctan_math") {
					interval = Interval::i_tan(ninterval);
				} else if (name == "sin_math") {
					interval = Interval::i_arcsin(ninterval);
				} else if (name == "cos_math") {
					interval = Interval::i_arccos(ninterval);
				} else if (name == "tan_math") {
					interval = Interval::i_arctan(ninterval);
				} else if (name == "sqrt_math") {
					interval = Interval::i_invsqrt(ninterval);
				} else {
					Assert(false, "NYI");
				}
				break;
			}
			case bool_node::TUPLE_R: {
				if (node->mother->type == bool_node::UFUN) {
					Assert(((UFUN_node*)(node->mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
					interval = Interval::i_copy(ninterval);
				} else {
					Assert(false, "NYI");
				}
				break;
			}
			case bool_node::TUPLE_CREATE: {
				return true;
			}
			default: {
				Assert(false, "NYI");
				return false;
			}
		}
		interval = Interval::i_intersect(interval, pinterval);
		return checkAndAdd(interval, pinterval, parent, level, node);
	}
	
	// Collects all node ids that contributed to making the range of this node EMPTY
	void generateConflict(bool_node* node) {
#if IP_DEBUG
		//itracker.print();
#endif
		conflictNodes.clear();
		seen.assign(itracker.size(), false);
		analyzeNodeClause(node, -1);
	}
	
	// Analyzes the node and its parents
	void analyzeNodeClause(bool_node* node, int lev) {
		analyzeNode(node, lev);
		vector<bool_node*> parents = node->parents();
		for (int i = 0; i < parents.size(); i++) {
			if (parents[i] != NULL){
				analyzeNode(parents[i], lev);
			}
		}
	}
	
	void analyzeNode(bool_node* node, int lev) {
		int newlev= lev;
		int nodeid = node->id;
#if IP_DEBUG
		//cout << "Doing " << node->lprint() << endl;
#endif
		Interval* interval = itracker.getInterval(nodeid, newlev);
#if IP_DEBUG
		//cout << " Level: " << lev << " NewLevel: " << newlev << endl;
#endif
		if (interval == FULL_INTERVAL) return;

		if (seen[newlev] == 0) {
			seen[newlev] = 1;
			bool_node* ic = itracker.getCause(nodeid, lev);
			Assert(lev == newlev, "Not possible");
#if IP_DEBUG
			//cout << "Node: " << node->lprint() << endl;
			//cout << "Cause: " << (ic != NULL ? ic->lprint() : "null") << endl;
#endif
			if (ic == NULL) {
				// Then, it is one of the interface nodes or constants or asserts
				conflictNodes.push_back(node);
				analyzeNode(node, newlev);
			}else{
				analyzeNodeClause(ic, lev);
			}
		} else {
#if IP_DEBUG
			//cout << "Already seen: " << node->lprint() << endl;
#endif
		}
	}
	
	void printCurState() {
		for (int i = 0; i < bdag.size(); i++) {
			bool_node* n = bdag[i];
			cout << n->lprint() << " Range: " << getInterval(n)->print() << endl;
		}
	}
};
