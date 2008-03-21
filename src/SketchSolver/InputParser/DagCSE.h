#ifndef DAGCSE_H_
#define DAGCSE_H_

#include "BooleanDAG.h"
#include "timerclass.h"

#define Dtime(...) /*nothing*/



class DagCSE : public NodeVisitor
{	
	BooleanDAG& dag;	
	map<string,  bool_node*> cse_map;
	
	inline bool hasCSE(string& str){
		Dtime(maptimer.restart();)
		bool tmp = cse_map.find(str) != cse_map.end(); 		
		Dtime(maptimer.stop();)
		return tmp;
	}
	
	inline  bool_node*& operator[](const string& str){
		Dtime(maptimer.restart();)
		 bool_node*& tmp = cse_map[str];
		Dtime(maptimer.stop();)
		return tmp;	
	}  
public:
	string last;
	 bool_node* lastNode;
	string ccode;
	Dtime(timerclass stimer;)
	Dtime(timerclass maptimer;)
	DagCSE(BooleanDAG& p_dag);
	virtual ~DagCSE();	
	
	void eliminateCSE();
	
	inline void setCSE(bool_node* node){
		node->accept(*this);
		(*this)[this->ccode] = node;
	}

	inline  bool_node* computeCSE( bool_node* node){
		node->accept(*this);
		//cout<<node<<" ccode = "<<ccode<<endl;
		
		if(this->ccode == last){ 			
			return lastNode; 
		}
		
		map<string,  bool_node*>::iterator it = cse_map.lower_bound(this->ccode);
		if(it != cse_map.end() && it->first == this->ccode ){
			last = this->ccode;
			lastNode = it->second; 
			return it->second;
		}else{
			last = this->ccode;
			lastNode = node; 
			cse_map.insert(it, make_pair(this->ccode, node));
			return node;
		}
	}


	inline void clear(){
		cse_map.clear();
		last = "";
		lastNode = NULL;
	}

	void setStr(int id1, char op, int id2);
	
	virtual void visit(  AND_node& node );
	virtual void visit(  OR_node& node );
	virtual void visit(  XOR_node& node );
	virtual void visit(  SRC_node& node );
	virtual void visit(  DST_node& node );
	virtual void visit(  NOT_node& node );	
	virtual void visit(  CTRL_node& node );
	virtual void visit(  PLUS_node& node );
	virtual void visit(  TIMES_node& node );
	virtual void visit(  UFUN_node& node );
	virtual void visit(  ARRACC_node& node );
	virtual void visit(  DIV_node& node );
	virtual void visit(  MOD_node& node );
	virtual void visit(  CONST_node& node );
	virtual void visit(  NEG_node& node);
	virtual void visit(  GT_node& node );
	virtual void visit(  GE_node& node );
	virtual void visit(  LT_node& node );
	virtual void visit(  LE_node& node );
	virtual void visit(  EQ_node& node );
	virtual void visit(  ARRASS_node& node );
	virtual void visit(  ACTRL_node& node );
	virtual void visit(  ASSERT_node &node);	
};

#endif /*DAGCSE_H_*/
