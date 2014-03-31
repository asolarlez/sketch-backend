#ifndef NODESTOEUCLID_H_
#define NODESTOEUCLID_H_

#include "BooleanDAG.h"
#include <string>
#include <iostream>
#include <sstream>
#include <typeinfo>

using namespace std;

class NodesToEuclid : public  NodeVisitor
{
	ostream& out;
	string prefix;
	
	vector<string> outputs;
public:



	NodesToEuclid(ostream& p_out, const string& p_prefix):out(p_out), prefix(p_prefix){
	
		
	};
	
	
	void process(BooleanDAG& bdag){
		out<<"MODEL "<<prefix<<endl;
		out<<"CONTROL "<<endl;
		out<<"EXTVAR"<<endl;
		out<<"STOREVAR"<<endl;
		out<<"VAR"<<endl<<endl;
		vector<string> intinputs;
		out<<"CONST"<<endl;
		{
			Dout( cout<<"BEFORE declaring input names"<<endl );
			vector<bool_node*>& specIn = bdag.getNodesByType(bool_node::SRC);	
			for(int i=0; i<specIn.size(); ++i){
				SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
				int nbits = srcnode->get_nbits();
				if(nbits == 1){
					out<<node_name(*srcnode, true)<<" : TRUTH ; "<<endl;
				}else{
					string name = node_name(*srcnode, true);
					intinputs.push_back(name);
					out<<name<<" : BITVEC[16] ; "<<endl;
				}
			}
		}
		
		out<<"DEFINE"<<endl;
		
		int i=0;
		for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
			try{
			//if(i>1000){ cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl; }
			(*node_it)->accept(*this);
			}catch(BasicError& be){
				throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
	    	}
		}
				
		Assert(outputs.size() > 0, "This can't happen;aoi");
		out<<prefix<<"root := "<<outputs[0];
		for(int i=1; i<outputs.size(); ++i){
				out<<" & "<<outputs[i];
		}
		out<<";"<<endl;
		
		out<<" EXEC "<<endl;
		string precond ="";
		if(intinputs.size() > 0){
			for(int i=0; i<intinputs.size(); ++i){
				if(i != 0){
					precond += " & ";
				}
				precond += intinputs[i];
				precond += ">= 0";				
			}
			out<<"decide( ("<<precond<<") => "<<prefix<<"root);"<<endl;
		}else{
			out<<"decide("<<prefix<<"root);"<<endl;
		}
	}
	
	virtual ~NodesToEuclid();
	
	
	
	string node_name(bool_node& node, bool isBool){
		if( typeid(node) == typeid(CONST_node) ){
			CONST_node& cn = dynamic_cast<CONST_node&>(node);
			if(isBool){
				if(cn.getVal() == 0){
					return "false";	
				}else{
					return "true";
				}
			}else{
				stringstream str;
				str<< cn.getVal() ;		
				return str.str();
			}
		}else{
			stringstream str;
			if(node.getOtype()==OutType::BOOL  && !isBool){
				str<<"( case "<<prefix<<node.get_name()<<"_"<<node.id<<": 1; default : 0; esac )";
			}else{
				str<<prefix<<node.get_name()<<"_"<<node.id;	
			}
			return str.str();
		}
	}
	
	string mother_name(bool_node& node, bool isBool){
		return node_name(*node.mother, isBool);
	}
	
	string father_name(bool_node& node, bool isBool){
		return node_name(*node.father, isBool);
	}
	
	virtual void visit( AND_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, true)<<" & "<<father_name(node, true)<<"; "<<endl;
	}
	virtual void visit( OR_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, true)<<" | "<<father_name(node, true)<<"; "<<endl;
	}
	virtual void visit( XOR_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, true)<<" ^ "<<father_name(node, true)<<"; "<<endl;		
	}
	virtual void visit( SRC_node& node ){
		// out<<"input "<<prefix<<node.get_name()<<endl;
		//Declarations have already been made.	
	}
	
	virtual void visit( DST_node& node ){
		outputs.push_back( mother_name(node, true) );
	}
	
	virtual void visit( NOT_node& node ){
		out<<node_name(node, true)<<" := ~"<<mother_name(node, true)<<"; "<<endl;
	}
	virtual void visit( NEG_node& node ){
		out<<node_name(node, false)<<" := -"<<mother_name(node, false)<<"; "<<endl;
	}
	
	virtual void visit( CONST_node& node ){
		//out<<prefix<<node.get_name()<<" := "<< node.getVal() <<"; "<<endl;
	}
	
	virtual void visit( CTRL_node& node ){
		Assert(false, "There should  be no controls left !!");
		out<<"control "<<prefix<<node.get_name()<<endl;	
	}
	virtual void visit( PLUS_node& node ){
		out<<node_name(node, false)<<" := "<<mother_name(node, false)<<" +_16 "<<father_name(node, false)<<"; "<<endl;		
	}
	virtual void visit( TIMES_node& node ){
		out<<node_name(node, false)<<" := "<<mother_name(node, false)<<" *_16 "<<father_name(node, false)<<"; "<<endl;		
	}
	virtual void visit( ARRACC_node& node ){
		vector<bool_node*>& mmother = node.multi_mother;
		
		Assert( mmother.size() == 2, " NYI; Can't produce Euclid file for this benchmark." );
		
		out<<node_name(node, true)<<" := ";
		out<<" case ";		
		out<<mother_name(node, true)<<":";
		
		out<<node_name(*mmother[1], node.getOtype()==OutType::BOOL);
		out<<"; default : ";
		out<<node_name(*mmother[0], node.getOtype()==OutType::BOOL);
		out<<"; esac; "<<endl;
	}

	virtual void visit( DIV_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, false)<<" /_16 "<<father_name(node, false)<<"; "<<endl;		
	}
	virtual void visit( MOD_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, false)<<" %_16 "<<father_name(node, false)<<"; "<<endl;		
	}	
	virtual void visit( LT_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, false)<<" < "<<father_name(node, false)<<"; "<<endl;		
	}
	virtual void visit( EQ_node& node ){
		out<<node_name(node, true)<<" := "<<mother_name(node, false)<<" = "<<father_name(node, false)<<"; "<<endl;		
	}
	virtual void visit( ARRASS_node& node ){}
	virtual void visit( ACTRL_node& node ){}
	virtual void visit( ASSERT_node &node ){
		outputs.push_back( mother_name(node, true) );
	}
	
	
	
	virtual void visit (UFUN_node &node){
		Assert(false, "NYI");	
	}
	
};

#endif /*NODESTOEUCLID_H_*/
