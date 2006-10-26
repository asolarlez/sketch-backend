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
		if(node.mother != NULL){
			if( node.mother_sgn==1 ){
				str<<prefix<<node.mother->get_name();
			}else{
				str<<"!"<<prefix<<node.mother->get_name();
			}
		}else{
			str<<node.mother_sgn;
		}
		return str.str();
	}
	
	string mother_name(arith_node& node){
		stringstream str;
		if(node.mother != NULL){
			if( node.mother_sgn==1 ){
				str<<prefix<<node.mother->get_name();
			}else{
				str<<"!"<<prefix<<node.mother->get_name();
			}
		}else{
			str<<node.mother_quant;
		}
		return str.str();
	}
	
	string father_name(bool_node& node){
		stringstream str;
		if(node.father != NULL){
			if( node.father_sgn==1 ){
				str<<prefix<<node.father->get_name();
			}else{
				str<<"!"<<prefix<<node.father->get_name();
			}
		}else{
			str<<node.father_sgn;
		}
		return str.str();
	}
	
	string father_name(arith_node& node){
		stringstream str;
		if(node.father != NULL){
			if( node.father_sgn==1 ){
				str<<prefix<<node.father->get_name();
			}else{
				str<<"!"<<prefix<<node.father->get_name();
			}
		}else{
			str<<node.father_quant;
		}
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
	virtual void visit( PT_node& node ){}
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
		vector<int>::iterator signs = node.multi_mother_sgn.begin();
		Assert( mmother.size() == 2, " NYI; Can't produce Euclid file for this benchmark." );
		
		out<<prefix<<node.get_name()<<" := ";
		out<<" case ";
		
		if(node.mother_sgn==0){
			out<<"!("<<prefix<<node.mother->get_name()<<")";	
		}else{
			out<<prefix<<node.mother->get_name();
		}
		
		out<<":";
		
		if( mmother[1]==NULL){
			out<<node.multi_mother_sgn[1];
		}else{
			if( node.multi_mother_sgn[1] == 0){
				out<<"!("<<prefix<<mmother[1]->get_name()<<")";
			}else{
				out<<prefix<<mmother[1]->get_name();
			}
		}
		out<<"; default : ";
		if( mmother[0]==NULL){
			out<<node.multi_mother_sgn[0];
		}else{
			if( node.multi_mother_sgn[0] == 0){
				out<<"!("<<prefix<<mmother[0]->get_name()<<")";
			}else{
				out<<prefix<<mmother[0]->get_name();
			}
		}
		
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
	
};

#endif /*NODESTOEUCLID_H_*/
