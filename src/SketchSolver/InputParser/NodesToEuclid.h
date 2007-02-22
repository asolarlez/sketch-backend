#ifndef NODESTOEUCLID_H_
#define NODESTOEUCLID_H_

#include "BooleanDAG.h"
#include <string>
#include <iostream>
#include <sstream>

using namespace std;

class NodesToEuclid : public  NodeVisitor
{
	ostream& out;
	string prefix;
public:
	NodesToEuclid(ostream& p_out, const string& p_prefix):out(p_out), prefix(p_prefix){};
	virtual ~NodesToEuclid();
	
	
	string mother_name(bool_node& node){
		stringstream str;
		str<<prefix<<node.mother->get_name();		
		return str.str();
	}
	
	string father_name(bool_node& node){
		stringstream str;
		str<<prefix<<node.father->get_name();
		return str.str();
	}
	
	virtual void visit( AND_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" & "<<father_name(node)<<"; "<<endl;
	}
	virtual void visit( OR_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" | "<<father_name(node)<<"; "<<endl;
	}
	virtual void visit( XOR_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" ^ "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( SRC_node& node ){
		out<<"input "<<prefix<<node.get_name()<<endl;	
	}
	virtual void visit( DST_node& node ){
		out<<"output "<<prefix<<node.get_name()<<" := "<<mother_name(node)<<"; "<<endl;		
	}
	virtual void visit( NOT_node& node ){
		out<<prefix<<node.get_name()<<" := !"<<mother_name(node)<<"; "<<endl;
	}
	virtual void visit( NEG_node& node ){
		out<<prefix<<node.get_name()<<" := -"<<mother_name(node)<<"; "<<endl;
	}
	
	virtual void visit( CONST_node& node ){
		out<<prefix<<node.get_name()<<" := "<< node.getVal() <<"; "<<endl;
	}
	
	virtual void visit( CTRL_node& node ){
		out<<"control "<<prefix<<node.get_name()<<endl;	
	}
	virtual void visit( PLUS_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" +_16 "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( TIMES_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" *_16 "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( ARRACC_node& node ){
		vector<bool_node*>& mmother = node.multi_mother;
		
		Assert( mmother.size() == 2, " NYI; Can't produce Euclid file for this benchmark." );
		
		out<<prefix<<node.get_name()<<" := ";
		out<<" case ";		
		out<<mother_name(node)<<":";
		
		out<<prefix<<mmother[1]->get_name();
		
		out<<"; default : ";
		out<<prefix<<mmother[0]->get_name();		
		out<<"; esac; "<<endl;
		
	}
	virtual void visit( DIV_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" /_16 "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( MOD_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" %_16 "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( GT_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" > "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( GE_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" >= "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( LT_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" < "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( LE_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" <= "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( EQ_node& node ){
		out<<prefix<<node.get_name()<<" := "<<mother_name(node)<<" = "<<father_name(node)<<"; "<<endl;		
	}
	virtual void visit( ARRASS_node& node ){}
	virtual void visit( ACTRL_node& node ){}
	virtual void visit( ASSERT_node &node ){
		out<<" assert "<<mother_name(node)<<"; "<<endl;		
	}
	virtual void visit (UFUN_node &node){
		Assert(false, "NYI");	
	}
	
};

#endif /*NODESTOEUCLID_H_*/
