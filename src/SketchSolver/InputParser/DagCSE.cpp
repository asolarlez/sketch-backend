#include "DagCSE.h"
#include "SATSolver.h"
#include <sstream>

char tmpbuf[10000];
char tbl[32] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
                'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
				'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
				'U', 'V'};

inline void writeInt(char* buf, unsigned val, int& p){
	while(val != 0){
		unsigned t = val & 0x1f;
		buf[p] = tbl[t];
		p = p+1;
		val = val>>5;
	}
}


DagCSE::DagCSE(BooleanDAG& p_dag): dag(p_dag),stimer("string creation"), maptimer("map lookup")
{
}

DagCSE::~DagCSE()
{
	stimer.print();
	maptimer.print();
	
}



void DagCSE::eliminateCSE(){
	int k=0;
	for(int i=0; i<dag.size(); ){
		// Get the code for this node. 		
		dag[i]->accept(*this);
		// look it up in the cse map.		
		(cout<<dag[i]->id<<"  "<<dag[i]->get_name()<<": "<<ccode<<endl) ;
		if( hasCSE(ccode) ){
			// if we do find it, then remove the node and replace it with its cse.			
			bool_node * cse = cse_map[ccode];
			Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<cse->get_name()<<endl );
			dag.replace(i, cse); 
			++i;
		}else{
			// if we don't find it, just add it.
			cse_map[ccode] = dag[i];
			++i;
		}
	}
	dag.removeNullNodes();
	dag.relabel();
	Dout(cout<<" end cse "<<endl);	
}

 void DagCSE::visit( AND_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id; 
 		
 	sprintf(tmpbuf, "%d&%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf;
 	stimer.stop();
 }
 void DagCSE::visit( OR_node& node ){
 	stimer.restart();
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	sprintf(tmpbuf, "%d|%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf; 	
 	stimer.stop();
 }
 void DagCSE::visit( XOR_node& node ){
 	stimer.restart();
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	sprintf(tmpbuf, "%dxor%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf;
 	stimer.stop();
 }
 void DagCSE::visit( SRC_node& node ){
 	stimer.restart();
 	stringstream str;
 	str<<node.id;
 	ccode = str.str();
 	stimer.stop();
 }
 void DagCSE::visit( DST_node& node ){
 	stimer.restart();
  	stringstream str;
 	str<<node.id;
 	ccode = str.str();
 	stimer.stop();
 }
 void DagCSE::visit( NOT_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	
 	sprintf(tmpbuf, "!%d", mid );   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 void DagCSE::visit( CTRL_node& node ){
 	stimer.restart();
 	stringstream str;
 	str<<node.id;
 	ccode = str.str();
 	stimer.stop();
 }
 void DagCSE::visit( PLUS_node& node ){
 	stimer.restart();
 	Assert( node.mother != NULL, "Null mother no longer allowed!!");
 	Assert( node.father != NULL, "Null father no longer allowed!!");

 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%d+%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 void DagCSE::visit( TIMES_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id; 	
	
	sprintf(tmpbuf, "%d*%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 
 void DagCSE::visit( DIV_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%d/%d",mid,fid);   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 void DagCSE::visit( MOD_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%dmod%d",mid,fid);   	
 	ccode = tmpbuf;
 	
 	
 	stimer.stop();
 }
 
 void DagCSE::visit( NEG_node& node ){
 	stimer.restart();

 	int mid = node.mother == NULL? -1: node.mother->id; 	
 	
 	sprintf(tmpbuf, "-%d", mid );   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 
  void DagCSE::visit( CONST_node& node ){
  	stimer.restart();
  	
  	sprintf(tmpbuf, "$%d$", node.getVal() );   	
 	ccode = tmpbuf;
  	
 	stimer.stop();
 }
 
 
 void DagCSE::visit( GT_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	
 	sprintf(tmpbuf, "%d>%d",mid,fid);   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 void DagCSE::visit( GE_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	sprintf(tmpbuf, "%d>=%d",mid,fid);   	
 	ccode = tmpbuf;
 	stimer.stop();
 }
 
 void DagCSE::visit( LT_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%d<%d",mid,fid);   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 
 
 void DagCSE::visit( LE_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%d<=%d",mid,fid);   	
 	ccode = tmpbuf;
 	
 	stimer.stop();
 }
 void DagCSE::visit( EQ_node& node ){
 	stimer.restart();
 	
 	int mid = node.mother == NULL? -1: node.mother->id;
 	int fid = node.father == NULL? -1: node.father->id;
 	
 	sprintf(tmpbuf, "%d==%d",min(mid, fid),max(mid, fid));   	
 	ccode = tmpbuf;
 	 	
 	stimer.stop();
 }


 void DagCSE::visit( UFUN_node& node ){
 	stimer.restart();
 	stringstream str; 	
 	str<<node.get_ufname()<<"(";
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
 		str<<mmid<<",";
 	}
 	str<<")";
 	ccode = str.str();
 	stimer.stop();
 }


 
 void DagCSE::visit( ARRACC_node& node ){
 	stimer.restart();
	int p = 0;
	int mid = node.mother == NULL? -1: node.mother->id;
	writeInt(tmpbuf, mid, p);
	tmpbuf[p] = '|'; p++;
	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
		writeInt(tmpbuf, mmid, p);
		tmpbuf[p] = ','; p++;
 	}
	tmpbuf[p] = 0;
	ccode = tmpbuf;
	/*
 	stringstream str;
 	int mid = node.mother == NULL? -1: node.mother->id;
 	str<<mid<<"|";
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
 		str<<mmid<<",";
 	}
 	ccode = str.str();
	*/
 	stimer.stop();
 }
 
 void DagCSE::visit( ARRASS_node& node ){
 	stimer.restart();
	stringstream str;
 	int mid = node.mother == NULL? -1: node.mother->id;

	sprintf(tmpbuf, "%d=%d?%d:%d",mid,node.quant,node.multi_mother[0]->id, node.multi_mother[1]->id);   	
 	ccode = tmpbuf;

 	stimer.stop();
 }
 void DagCSE::visit( ACTRL_node& node ){
 	stimer.restart();
 	stringstream str;
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
 		str<<mmid<<",";
 	}
 	ccode = str.str();
 	stimer.stop();
 }
 
 
 
 
 void DagCSE::visit( ASSERT_node &node){
 	stimer.restart();
 	stringstream str;
	int mid = node.mother == NULL? -1: node.mother->id;
	str<<"ass"<<mid<<":"<<node.getMsg();
 	ccode = str.str();
 	stimer.stop();
 }
 
 	