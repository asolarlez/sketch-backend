#ifndef NODEVISITOR_H_
#define NODEVISITOR_H_

class AND_node;
class OR_node;
class XOR_node;
class SRC_node;
class DST_node;
class NOT_node;
class CTRL_node;
class PLUS_node;
class TIMES_node;
class UFUN_node;
class ARRACC_node;
class DIV_node;
class MOD_node;
class NEG_node;
class CONST_node;
class LT_node;
class EQ_node;
class ARRASS_node;
class ACTRL_node;
class ASSERT_node;
class ARR_R_node;
class ARR_W_node;
class ARR_CREATE_node;
class TUPLE_CREATE_node;
class TUPLE_R_node;
class arith_node;
struct bool_node;
class BooleanDAG;

class NodeVisitor{
	
	protected:
	virtual void visitArith(arith_node& node );
	virtual void visitBool(bool_node& node );
	bool_node* rvalue;
	BooleanDAG* tmpdag;
	public:
	
	
	NodeVisitor();
	~NodeVisitor();
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ASSERT_node &node);
    
	virtual void visit( ARR_R_node &node);
	virtual void visit( ARR_W_node &node);
	virtual void visit( ARR_CREATE_node &node);
    
	virtual void visit( TUPLE_CREATE_node &node);
	virtual void visit( TUPLE_R_node &node);
    
	virtual void process(BooleanDAG& bdag);
};


#endif /*NODEVISITOR_H_*/
