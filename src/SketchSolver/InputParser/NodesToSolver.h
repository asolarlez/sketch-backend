#ifndef NODESTOSOLVER_H
#define NODESTOSOLVER_H

#include "BooleanDAG.h"
#include "BooleanToCNF.h"

#ifndef INTEGERBOUND
#define INTEGERBOUND 8192
#endif


class transfValue{
	int _id;
public:
	typedef enum {BIT, BVECT, SPARSE} Type;
	Type type;
	int size;
	bool neg;
	vector<int> num_ranges;	
	transfValue(const transfValue& tv): 
	type(tv.type), _id(tv._id), size(tv.size), num_ranges(tv.num_ranges),neg(tv.neg){
		Assert( _id > 0 , "This can't happen ccjkjkh");
	};
	transfValue(Type p_type, int p_id, int p_size): type(p_type), _id(p_id), size(p_size),neg(false){
		if( _id < 0){ neg = !neg; _id = -_id; }
	};
	transfValue(Type p_type, int p_id, int p_size, bool p_neg): type(p_type), _id(p_id), size(p_size),neg(p_neg){
		if( _id < 0){ neg = !neg; _id = -_id; }
	};
	transfValue& operator=(const transfValue& tv){
		type = (tv.type);
		_id = (tv._id);
		size = (tv.size);
		num_ranges = (tv.num_ranges);
		neg = tv.neg;
	}
	
	int id() const{
		return neg? _id: -_id;	
	}
	
	int id(int idx) const{
		return neg? (_id+idx) : -(_id+idx);
	}
	
	void bitAdjust(bool bit){
		neg = bit;
	}
	
	void intAdjust(int adj){
		for(int i=0; i<num_ranges.size(); ++i){ num_ranges[i] = num_ranges[i]*adj; }
	}
	
	transfValue makeBVect(varDir& dir) const{
		switch(type){
			case BVECT: Dout( cout<<"Converting from BitVector to BitVector"<<endl ); return *this;
			case BIT: 	Dout( cout<<"Converting from Bit to BitVector"<<endl ); return transfValue(BVECT, _id, 1, neg);
			case SPARSE:{
				Dout( cout<<"Converting from Sparse to BitVector"<<endl );
				vector<vector<int> > bit;
				vector<int> nr(num_ranges);
				bool more = false;
				do{
					bit.push_back(vector<int>());
					vector<int>& current = bit[bit.size()-1];
					current.push_back(0);
					more = false;
					for(int i=0; i<num_ranges.size(); ++i){
						int& val = nr[i];
						if( val > 0){ 
							more = true; 
							if( (val & 1) != 0 ){
								current.push_back( id(i) );
							}
						};
					}			
				}while(more);	
				transfValue tv(BVECT, 0, bit.size());
				if( bit.size() == 0){
					tv._id = dir.YES;
					tv.neg = false;
					tv.size = 1;
					return tv;
				}
				tv._id = dir.newAnonymousVar();
				for(int i=1; i<bit.size(); ++i){	
					dir.newAnonymousVar();
				}
				for(int i=1; i<bit.size(); ++i){
					vector<int>& current = bit[i];
					current[0] = tv._id + i;
					dir.mng.addBigOrClause( &current[0], current.size()-1);
				}
				return tv;
			}
		}
	}
	transfValue makeSparse(varDir& dir) const{
		switch(type){
			case BVECT:{
				 Dout( cout<<"Converting from BitVector to Sparse"<<endl ); 
				 transfValue tv (SPARSE, 0, 2);
				 vector<int>& tmp = tv.num_ranges;
				 vector<int> ids(size);
				 for(int i=0; i<size; ++i){
				 	ids[i] = id(i);
				 }
				varRange vr = getSwitchVars(dir.mng,dir, ids, size, tmp, dir.YES);
				tv._id = vr.varID;
				tv.size = vr.range;
				return tv;
			}
			case BIT:{	
				Dout( cout<<"Converting from Bit to Sparse"<<endl ); 
				transfValue tv (SPARSE, 0, 2);
				tv.num_ranges.push_back(0);
				tv.num_ranges.push_back(1);
				tv._id = dir.newAnonymousVar();
				dir.newAnonymousVar();
				dir.mng.addEqualsClause( tv._id, -id());
				dir.mng.addEqualsClause( tv._id+1, id());
				return tv;
			}
			case SPARSE:
				Dout( cout<<"Converting from Sparse to Sparse"<<endl );	return *this;
		}
	}
};


class NodesToSolver : public NodeVisitor{
	SATSolver& mng;
	varDir& dir;
	const string& outname;
	map<bool_node*, int>& node_values; // -1=false, 1=true, 0=unknown
	map<bool_node*, vector<int> >& num_ranges;
	map<bool_node*, int>& node_ids;
	
	template<typename THEOP>
	void processArith(SATSolver& mng, varDir& dir,arith_node& node, map<bool_node*, int>& node_ids, map<bool_node*, vector<int> >& num_ranges);
	template<typename THEOP>
	int doArithExpr(SATSolver& mng, int quant1, int quant2, int id1, int id2, THEOP comp);
	template<typename COMP>
	void processComparissons(SATSolver& mng, varDir& dir,arith_node& node, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges, int YES);
	
	const int YES;
	const string& IN;
	const string& CTRL;
	
vector<int> scratchpad;
vector<int> tmprange;
vector<int> unirange;
	public:
	NodesToSolver(SATSolver& p_mng, varDir& p_dir, const string& p_outname, 
				 map<bool_node*, int>& p_node_values,
				map<bool_node*, vector<int> >& p_num_ranges,
				map<bool_node*, int>& p_node_ids, const int p_YES, const string& p_IN, const string& p_CTRL):
		mng(p_mng), dir(p_dir), 
		outname(p_outname), node_values(p_node_values), 
		num_ranges(p_num_ranges), node_ids(p_node_ids), 
		YES(p_YES), IN(p_IN), CTRL(p_CTRL), scratchpad(100),tmprange(2), unirange(1) {
				tmprange[0] = 0;
	tmprange[1] = 1;
	unirange[0] = 1;			
			 };

	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( PT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( GT_node& node );
	virtual void visit( GE_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( LE_node& node );
	virtual void visit( EQ_node& node ){ };
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	
	virtual void doNonBoolArrAcc(SATSolver& mng, varDir& dir,arith_node& node, map<bool_node*, int>& node_ids, map<bool_node*, vector<int> >& num_ranges);
	virtual bool checkParentsChanged(SATSolver& mng, bool_node& node, bool more);
};





#endif
