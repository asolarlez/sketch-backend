#ifndef DAGCSE_H_
#define DAGCSE_H_

#include "BooleanDAG.h"
#include "timerclass.h"
#include "StringHTable.h"


#define Dtime(...) /*nothing*/

/* Class for Structural Hashing of DAG*/
class DagCSE : public NodeVisitor
{	
	BooleanDAG& dag;	
	StringHTable2<bool_node*> cse_map;
	vector<char> tmpbuf;
	/*
	inline bool hasCSE(string& str){
		Dtime(maptimer.restart();)
		bool tmp = cse_map.find(str) != cse_map.end(); 		
		Dtime(maptimer.stop();)
		return tmp;
	}*/
	/*
	inline  bool_node* operator[](const string& str)const{
		Dtime(maptimer.restart();)
		 bool_node* tmp;
		cse_map.get(str.c_str(), tmp);
		Dtime(maptimer.stop();)
		return tmp;	
	} 
	*/
public:

	string ccode;
	Dtime(timerclass stimer;)
	Dtime(timerclass maptimer;)
	DagCSE(BooleanDAG& p_dag);
	virtual ~DagCSE();	
	int cheapStr(int id1, char op, int id2);
	void eliminateCSE();
	/*
	inline void setCSE(bool_node* node){
		node->accept(*this);
		(*this)[this->ccode] = node;
	}
	*/

	bool_node* quickcse(int idmom, int idpop, bool_node::Type t);

	inline  bool_node* computeCSE( bool_node* node){
		node->accept(*this);
		//cout<<node<<" ccode = "<<ccode<<endl;
		
		bool_node* rv;
		cse_map.condAdd(this->ccode.c_str(), this->ccode.size(), node, rv);
		return rv;
	}


	inline void clear(){
		cse_map.clear();
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
	virtual void visit(  LT_node& node );
	virtual void visit(  EQ_node& node );
	virtual void visit(  ARRASS_node& node );
	virtual void visit(  ACTRL_node& node );
	virtual void visit(  ASSERT_node &node);	
	virtual void visit( ARR_R_node &node);
	virtual void visit( ARR_W_node &node);
	virtual void visit( ARR_CREATE_node &node);
    virtual void visit( TUPLE_CREATE_node &node);
    virtual void visit( TUPLE_R_node &node);
};

#endif /*DAGCSE_H_*/
