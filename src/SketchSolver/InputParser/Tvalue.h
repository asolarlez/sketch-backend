#ifndef TVALUE_H
#define TVALUE_H

#include <vector>
using namespace std;

class Tvalue{
	int _id;
	int _size;
	bool neg;
public:
	typedef enum {BIT, BVECT, SPARSE} Type;
	Type type;
	vector<int> num_ranges;	
	
		
	bool operator!=(const Tvalue& tv) const{
		if( tv._id != _id || tv.neg != neg){
			return true;	
		}
		if( tv._size != _size){
			return true;	
		}
		for(int i=0; i<_size; ++i){
			if( num_ranges[i] != tv.num_ranges[i] ){ return true; }	
		}
		return false;
	}
	
	Tvalue(): type(BIT), _id(0), _size(-1), neg(true){}

	Tvalue(const Tvalue& tv): 
	type(tv.type), _id(tv._id), _size(tv._size), num_ranges(tv.num_ranges),neg(tv.neg){
//		cout<<" copying neg="<<neg<<"  tvneg"<<tv.neg<<" id "<<_id<<endl;
		Assert( _id >= 0 , "This can't happen  zxpoiup "<<_id<<endl);
	};
	Tvalue(Type p_type, int p_id, int p__size): type(p_type), _id(p_id), _size(p__size),neg(true){
		if( _id < 0){ neg = !neg; _id = -_id; }
	};
	Tvalue(Type p_type, int p_id, int p__size, bool p_neg): type(p_type), _id(p_id), _size(p__size),neg(p_neg){
		if( _id < 0){ neg = !neg; _id = -_id; }
	};
	Tvalue(int p_id): type(BIT), _id(p_id), _size(0),neg(true){
		if( _id < 0){ neg = !neg; _id = -_id; }
	};
	Tvalue& operator=(const Tvalue& tv){
		type = (tv.type);
		_id = (tv._id);
		_size = (tv._size);
		num_ranges = (tv.num_ranges);
		neg = tv.neg;
	}
	Tvalue& operator=(int p_id){
		type = BIT;
		_id = p_id;
		neg = true;
		if( _id < 0){ neg = !neg; _id = -_id; }
		_size = 0;
		num_ranges.clear();
	}
	
	void setID(int id){
		_id = id;	
		Assert( _id != 0 , "This can't happen lcx kfiug");
		neg = true;
		if( _id < 0){ neg = !neg; _id = -_id; }
	}
	
	void setSparse(){
		type = SPARSE;
		_size = num_ranges.size();	
	}
	
	int operator[](int idx)const{
		Assert( type == SPARSE , "This should be sparse ");
		return num_ranges[idx];	
	}
	
	int size()const{
		return _size;
	}
	
	bool isNULL() const{
		return _id == 0;
	}
	
	int id() const{
		Assert( _id != 0 , "This can't happen poqiu  "<<_id<<endl);
		return neg? _id: -_id;	
	}
	
	int id(int idx) const{
		Assert( _id != 0 , "This can't happen klj dh "<<_id<<endl);
		return neg? (_id+idx) : -(_id+idx);
	}
	
	void bitAdjust(bool bit){
		Assert( type != SPARSE , "can't bitadjust a sparse.");
		if( !bit ) neg = !neg;
	}
	
	void intAdjust(int adj){
		Assert( type == SPARSE , "This is bad; uiego");
		for(int i=0; i<num_ranges.size(); ++i){ num_ranges[i] = num_ranges[i]*adj; }
	}
	
	bool isSparse() const{
		return type == SPARSE;	
	}	
	
	Tvalue makeBVect(varDir& dir) const{
		Assert( _id != 0 , "This can't happen wo ej "<<_id<<endl);
		switch(type){
			case BVECT: Dout( cout<<"Converting from BitVector to BitVector"<<endl ); return *this;
			case BIT: 	Dout( cout<<"Converting from Bit to BitVector"<<endl ); return Tvalue(BVECT, _id, 1, neg);
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
				Tvalue tv(BVECT, 0, bit.size());
				if( bit.size() == 0){
					tv._id = dir.YES;
					tv.neg = false;
					tv._size = 1;
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
	
	void inPlaceMakeSparse(varDir& dir){
		Assert( _id != 0 , "This can't happen oria um "<<_id<<endl);
		switch(type){
			case BVECT:{
				 Dout( cout<<"Converting from BitVector to Sparse"<<endl ); 
				 vector<int>& tmp = num_ranges;
				 vector<int> ids(_size);
				 for(int i=0; i<_size; ++i){
				 	ids[i] = id(i);
				 }
				type = SPARSE;
				varRange vr = getSwitchVars(dir.mng,dir, ids, _size, tmp, dir.YES);
				_id = vr.varID;
				_size = vr.range;
				neg = true;
				type = SPARSE;
				return ;
			}
			case BIT:{
				Dout( cout<<"Converting "<<*this<<" from Bit to Sparse"<<endl ); 
				if( _id == dir.YES ){
					if( neg ){
						num_ranges.push_back(1);	
						_size = 1;						
					}else{
						num_ranges.push_back(0);
						_size = 1;
						neg = true;
					}
				}else{
					num_ranges.push_back(0);
					num_ranges.push_back(1);
					int tmp = dir.newAnonymousVar();
					dir.newAnonymousVar();
					dir.mng.addEqualsClause( tmp, -id());
					dir.mng.addEqualsClause( tmp+1, id());
					_id = tmp;
					_size = 2;
					neg = true;
				}
				type = SPARSE;
				return;
			}
			case SPARSE:
				Dout( cout<<"Converting from Sparse to Sparse"<<endl );	return;
		}	
	}
	
	Tvalue makeSparse(varDir& dir) const{
		Tvalue tv(*this);
		tv.inPlaceMakeSparse(dir);
		return tv;
	}
	
	void ipMakeSparseCondAdjust(bool cond, int quant, varDir& dir){
		if( !isSparse() ){			
			if( cond ){
				Assert( quant == 1 || quant == 0, "If cond, then quant must either be 1 or 0");
				bitAdjust( quant == 1 );	
				inPlaceMakeSparse(dir);
			}else{
				inPlaceMakeSparse(dir);
				if( quant != 1){
					intAdjust(quant);	
				}
			}
		}else{
			if( quant != 1){
				intAdjust(quant);	
			}
		}
		Dout( cout<<"quant ="<<quant<<" new val ="<<*this<<endl );
	}
	
	friend ostream& operator<<(ostream& out, Tvalue tv){
		out<<"{";
		out<< (tv.neg?tv._id : -tv._id);
		if( tv.type == BVECT) out<< "   size="<<tv._size;
		if( tv.type == SPARSE ){
			out<<" [ ";
			for(int i=0; i<tv._size; ++i){
				out<<tv.num_ranges[i]<<", ";
			}	
			out<<" ] ";
		}
		out<<"}";
		return out;
	}
};









#endif
