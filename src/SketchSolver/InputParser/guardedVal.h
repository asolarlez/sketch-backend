#ifndef __GUARDEDVAL_H
#define __GUARDEDVAL_H

class guardedVal{
public:
	int value;
	int guard;
	int idx;
	guardedVal():value(0), guard(0), idx(-1){}
	guardedVal(int g, int v):value(v), guard(g), idx(-1){}
	guardedVal(int g, int v, int i):value(v), guard(g), idx(i){}
	guardedVal(const guardedVal& gv): value(gv.value), guard(gv.guard), idx(gv.idx){}
	inline bool operator==(const guardedVal& gv) const{ return gv.value == value && gv.guard == guard && gv.idx == idx; }
	inline bool operator!=(const guardedVal& gv) const{ return gv.value != value || gv.guard != guard || gv.idx != idx; }
	inline guardedVal& operator=(const guardedVal& gv){ value = gv.value; guard=gv.guard; idx = gv.idx; return *this; }
	friend ostream &operator<<(ostream& out, const guardedVal& gv){
		out<<"("<<gv.guard<<", "<<gv.value;
		if(gv.idx>=0){
			out<<", "<<gv.idx<<")";
		}else{
			out<<")";
		}
		return out;
	}
};
#endif 
