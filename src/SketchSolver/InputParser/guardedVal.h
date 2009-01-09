#ifndef __GUARDEDVAL_H
#define __GUARDEDVAL_H

class guardedVal{
public:
	int value;
	int guard;
	guardedVal():value(0), guard(0){}
	guardedVal(int g, int v):value(v), guard(g){}
	guardedVal(const guardedVal& gv): value(gv.value), guard(gv.guard){}
	inline bool operator==(const guardedVal& gv) const{ return gv.value == value && gv.guard == guard; }
	inline bool operator!=(const guardedVal& gv) const{ return gv.value != value || gv.guard != guard; }
	inline guardedVal& operator=(const guardedVal& gv){ value = gv.value; guard=gv.guard; return *this; }
	friend ostream &operator<<(ostream& out, const guardedVal& gv){
		out<<"("<<gv.guard<<", "<<gv.value<<")";
		return out;
	}
};
#endif 
