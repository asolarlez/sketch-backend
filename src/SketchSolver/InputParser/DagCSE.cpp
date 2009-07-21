#include "DagCSE.h"
#include "SATSolver.h"
#include <sstream>



char tmpbuf[10000];
char tbl[64] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
                'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
				'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
				'U', 'V',
				'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 
                'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 
				'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 
				'@', '#'
};


void DagCSE::setStr(int id1, char op, int id2){
	int p = 0;
	writeInt(tmpbuf, id1, p);
	tmpbuf[p] = op; p++;
	writeInt(tmpbuf, id2, p);
	tmpbuf[p] = 0;
	ccode = tmpbuf;		
	/*
	sprintf(tmpbuf, "%d%c%d",id1, op,id2);   	
 	ccode = tmpbuf;
	*/
}


DagCSE::DagCSE(BooleanDAG& p_dag): dag(p_dag) Dtime(,stimer("string creation"), maptimer("map lookup"))
{
}

DagCSE::~DagCSE()
{
Dtime(stimer.print();)
Dtime(maptimer.print();)
	
}



void DagCSE::eliminateCSE(){
	int k=0;
	for(int i=0; i<dag.size(); ){
		// Get the code for this node. 		
		dag[i]->accept(*this);
		// look it up in the cse map.		
		(cout<<dag[i]->globalId<<"  "<<dag[i]->get_name()<<": "<<ccode<<endl) ;
		if( hasCSE(ccode) ){
			// if we do find it, then remove the node and replace it with its cse.			
			bool_node * cse = (bool_node*)cse_map[ccode];
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

 void DagCSE::visit(  AND_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId; 
 		
	setStr(min(mid, fid), '&' ,max(mid, fid));

 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  OR_node& node ){
 	Dtime(stimer.restart();)
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
	setStr(min(mid, fid), '|' ,max(mid, fid));

 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  XOR_node& node ){
 	Dtime(stimer.restart();)
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	setStr(min(mid, fid), '^' ,max(mid, fid));

 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  SRC_node& node ){
 	Dtime(stimer.restart();)
 	stringstream str;
 	str<<node.globalId;
 	ccode = str.str();
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  DST_node& node ){
 	Dtime(stimer.restart();)
  	stringstream str;
 	str<<node.globalId;
 	ccode = str.str();
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  NOT_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	
 	sprintf(tmpbuf, "!%d", mid );   	
 	ccode = tmpbuf;
 	
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  CTRL_node& node ){
 	Dtime(stimer.restart();)
 	stringstream str;
 	str<<node.globalId;
 	ccode = str.str();
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  PLUS_node& node ){
 	Dtime(stimer.restart();)
 	Assert( node.mother != NULL, "Null mother no longer allowed!!");
 	Assert( node.father != NULL, "Null father no longer allowed!!");

 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
 	setStr(min(mid, fid), '+' ,max(mid, fid));

 	
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  TIMES_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId; 	
	
	setStr(min(mid, fid), '*' ,max(mid, fid));

 	
 	Dtime(stimer.stop();)
 }
 
 void DagCSE::visit(  DIV_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
	setStr(min(mid, fid), '/' ,max(mid, fid));

 	
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  MOD_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
	setStr(mid, '%', fid);
 	
 	Dtime(stimer.stop();)
 }
 
 void DagCSE::visit(  NEG_node& node ){
 	Dtime(stimer.restart();)

 	int mid = node.mother == NULL? -1: node.mother->globalId; 	
 	
 	sprintf(tmpbuf, "-%d", mid );   	
 	ccode = tmpbuf;
 	
 	Dtime(stimer.stop();)
 }
 
  void DagCSE::visit(  CONST_node& node ){
  	Dtime(stimer.restart();)
  	
  	sprintf(tmpbuf, "$%d$", node.getVal() );   	
 	ccode = tmpbuf;
  	
 	Dtime(stimer.stop();)
 }
 
 
 void DagCSE::visit(  GT_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
 	
	setStr(fid, '<', mid);
 	
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  GE_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
	setStr(fid, '{', mid);

 	Dtime(stimer.stop();)
 }
 
 void DagCSE::visit(  LT_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
 	setStr(mid, '<', fid);
 	
 	Dtime(stimer.stop();)
 }
 
 
 void DagCSE::visit(  LE_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
	setStr(mid, '{', fid);
 	
 	Dtime(stimer.stop();)
 }
 void DagCSE::visit(  EQ_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
 	
	setStr(min(mid, fid), '=', max(mid, fid));
 	 	
 	Dtime(stimer.stop();)
 }


 void DagCSE::visit(  UFUN_node& node ){
 	Dtime(stimer.restart();)
 	stringstream str; 	
 	str<<node.get_ufname()<<"("<<node.mother->globalId<<")(";
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
 		str<<mmid<<",";
 	}
	str<<")";

 	ccode = str.str();
 	Dtime(stimer.stop();)
 }


 
 void DagCSE::visit(  ARRACC_node& node ){
 	Dtime(stimer.restart();)
	int p = 0;
	int mid = node.mother == NULL? -1: node.mother->globalId;
	writeInt(tmpbuf, mid, p);
	tmpbuf[p] = '~'; p++;
	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
		writeInt(tmpbuf, mmid, p);
		tmpbuf[p] = ','; p++;
 	}
	tmpbuf[p] = 0;
	ccode = tmpbuf;
	/*
 	stringstream str;
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	str<<mid<<"|";
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
 		str<<mmid<<",";
 	}
 	ccode = str.str();
	*/
 	Dtime(stimer.stop();)
 }
 
 void DagCSE::visit(  ARRASS_node& node ){
 	Dtime(stimer.restart();)
	stringstream str;
 	int mid = node.mother == NULL? -1: node.mother->globalId;

	sprintf(tmpbuf, "%d=%d?%d:%d",mid,node.quant,node.multi_mother[0]->globalId, node.multi_mother[1]->globalId);   	
 	ccode = tmpbuf;

 	Dtime(stimer.stop();)
 }

 void DagCSE::visit(  ACTRL_node& node ){
 	Dtime(stimer.restart();)
 	stringstream str;
 	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
 		str<<mmid<<",";
 	}
 	ccode = str.str();
 	Dtime(stimer.stop();)
 }
 
 
 
 
 void DagCSE::visit(  ASSERT_node &node){
 	Dtime(stimer.restart();)
 	stringstream str;
	int mid = node.mother == NULL? -1: node.mother->globalId;
	setStr(0, 'A', mid);
 	Dtime(stimer.stop();)
 }
 
 	
