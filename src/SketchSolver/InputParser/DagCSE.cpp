#include "DagCSE.h"
#include "SATSolver.h"
#include <sstream>




char tbl[64] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V',
    'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '@', '#'
};


int DagCSE::cheapStr(int id1, char op, int id2){
	int p = 0;
	char* tch = &tmpbuf[0];
	writeInt(tch, id1, p);
	tch[p] = op; p++;
	writeInt(tch, id2, p);
	tch[p] = 0;
	return p;
	/*
     sprintf(tmpbuf, "%d%c%d",id1, op,id2);
     ccode = tmpbuf;
     */
}


void DagCSE::setStr(int id1, char op, int id2){
	int p = 0;
	char* tch = &tmpbuf[0];
	writeInt(tch, id1, p);
	tch[p] = op; p++;
	writeInt(tch, id2, p);
	tch[p] = 0;
	ccode = tch;
	/*
     sprintf(tmpbuf, "%d%c%d",id1, op,id2);
     ccode = tmpbuf;
     */
}


DagCSE::DagCSE(BooleanDAG& p_dag): dag(p_dag), tmpbuf(1000) Dtime(,stimer("string creation"), maptimer("map lookup"))
{
}

DagCSE::~DagCSE()
{
    Dtime(stimer.print();)
    Dtime(maptimer.print();)
	
}





void DagCSE::eliminateCSE(){
	int k=0;
	for(int i=0; i<dag.size(); ++i){
		// Get the code for this node.
		bool_node* node = dag[i];
        
		bool_node* cse = computeCSE(node);
		if(cse != node){
            Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<cse->get_name()<<endl );
			dag.replace(i, cse);
		}
	}
	dag.removeNullNodes();
	dag.relabel();
	Dout(cout<<" end cse "<<endl);
}

bool_node* DagCSE::quickcse(int mid, int fid, bool_node::Type t){
	bool_node* rv;
	switch(t){
		case bool_node::AND:{
            int len = cheapStr(min(mid, fid), '&' ,max(mid, fid));
            if(cse_map.get(&tmpbuf[0], len, rv)){ return rv; }else{ return NULL; }
		}
		case bool_node::OR:{
            int len = cheapStr(min(mid, fid), '|' ,max(mid, fid));
            if(cse_map.get(&tmpbuf[0], len, rv)){ return rv; }else{ return NULL; }
		}
		case bool_node::XOR:{
            int len = cheapStr(min(mid, fid), '^' ,max(mid, fid));
            if(cse_map.get(&tmpbuf[0], len, rv)){ return rv; }else{ return NULL; }
		}
		case bool_node::NOT:
        {
            char* tch = &tmpbuf[0];
            int p = 0;
            tch[p] = '!'; p++;
            writeInt(tch, mid, p);
            tch[p]=0;
            if(cse_map.get(tch, p, rv)){ return rv; }else{ return NULL; }
        }
            
		default:
			return NULL;
	}
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
	int p=0;
	char* tch = &tmpbuf[0];
	tch[p] = '.'; p++;
	writeInt(tch, node.globalId, p);
	tch[p] = '.'; p++;
	tch[p] = 0;
	ccode = tch;
 	Dtime(stimer.stop();)
}
void DagCSE::visit(  DST_node& node ){
 	Dtime(stimer.restart();)
  	int p=0;
	char* tch = &tmpbuf[0];
	tch[p] = '.'; p++;
	writeInt(tch, node.globalId, p);
	tch[p] = '.'; p++;
	tch[p] = 0;
	ccode = tch;
 	Dtime(stimer.stop();)
}
void DagCSE::visit(  NOT_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	char* tch = &tmpbuf[0];
 	int p = 0;
	tch[p] = '!'; p++;
	writeInt(tch, mid, p);
	tch[p]=0;
 	ccode = tch;
 	
 	Dtime(stimer.stop();)
}
void DagCSE::visit(  CTRL_node& node ){
 	Dtime(stimer.restart();)
 	int p=0;
	char* tch = &tmpbuf[0];
	tch[p] = '.'; p++;
	writeInt(tch, node.globalId, p);
	tch[p] = '.'; p++;
	tch[p] = 0;
	ccode = tch;
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


void DagCSE::visit(  ARR_R_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid =  node.mother->globalId;
 	int fid =  node.father->globalId;
 	
	setStr(mid, '~' ,fid);
 	
 	Dtime(stimer.stop();)
}

void DagCSE::visit(  ARR_W_node& node ){
 	Dtime(stimer.restart();)
 	int mid = node.mother == NULL? -1: node.mother->globalId;
	char* tch = &tmpbuf[0];
	int p = 0;
	writeInt(tch, node.multi_mother[0]->globalId, p);
	tch[p] = '['; p++;
	writeInt(tch, mid, p);
	tch[p] = ']'; p++;
	writeInt(tch, node.multi_mother[1]->globalId, p);
 	tch[p] = 0;
 	ccode = tch;
 	Dtime(stimer.stop();)
}


void DagCSE::visit( ARR_CREATE_node& node ){
 	Dtime(stimer.restart();)
    
 	int mid = node.mother == NULL? -1: node.mother->globalId;
    
	int tempsz = node.multi_mother.size()*7 + 10;
    
	if(tempsz > tmpbuf.size()){
		tmpbuf.resize(tempsz);
	}
    
	char* tch = &tmpbuf[0];
	int p = 0;
	
	tch[p] = '{'; p++;
	int mmsize = node.multi_mother.size();
	for(int i=0; i<mmsize; ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
		writeInt(tch, mmid, p);
		tch[p] = ','; p++;
 	}
	tch[p] = '}'; p++;
 	tch[p] = 0;
    
 	ccode = tch;
	Assert(p < tmpbuf.size(), "CANNOT HAPPEN!!!");
 	Dtime(stimer.stop();)
}

void DagCSE::visit( TUPLE_CREATE_node& node){
    
    Dtime(stimer.restart();)
	int p = 0;
	
	int mmsize = node.multi_mother.size();
	if(mmsize * 20 > tmpbuf.size()){ tmpbuf.resize(mmsize * 20  );}
	char* tch = &tmpbuf[0];
	
	tch[p] = '<'; p++;
	for(int i=0; i<mmsize; ++i){
        
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
		writeInt(tch, mmid, p);
		tch[p] = ','; p++;
 	}
    tch[p]='>'; p++;
	tch[p] = 0;
	ccode = tch;
	
 	Dtime(stimer.stop();)
    
}

void DagCSE::visit( TUPLE_R_node& node){
    
    Dtime(stimer.restart();)
 	
 	int mid =  node.mother == NULL? -1: node.mother->globalId;
    
    int idx =  node.idx;
 	
	setStr(mid, '@' ,idx);
   
 	
 	Dtime(stimer.stop();)
}


void DagCSE::visit(  DIV_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
	setStr(mid, '/' ,fid);
    
 	
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
 	char* tch = &tmpbuf[0];
	int p = 0;
	tch[p] = '-'; p++;
	writeInt(tch, mid, p);
	tch[p]=0;
 	ccode = tch;
 	
 	Dtime(stimer.stop();)
}

void DagCSE::visit(  CONST_node& node ){
  	Dtime(stimer.restart();)
  	char* tch = &tmpbuf[0];
	int p = 0;
	tch[p] = '$'; p++;
	if(node.isFloat()){
		p += sprintf(tch+p, "%A", node.getFval());
	}else{
		writeInt(tch, node.getVal(), p);
	}
	tch[p] = '$'; p++;
	tch[p]=0;
 	ccode = tch;
  	
 	Dtime(stimer.stop();)
}




void DagCSE::visit(  LT_node& node ){
 	Dtime(stimer.restart();)
 	
 	int mid = node.mother == NULL? -1: node.mother->globalId;
 	int fid = node.father == NULL? -1: node.father->globalId;
 	
 	setStr(mid, '<', fid);
 	
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
    
	int p = 0;
    
	const string& tst = node.get_ufname();
	int mxsz = tst.size() + node.outname.size() + node.multi_mother.size() * 8 + 17;
	if(tmpbuf.size() < mxsz){
		tmpbuf.resize(mxsz);
	}
    
	char* tch = &tmpbuf[0];
	
	memcpy(tch, tst.c_str(), tst.size());
	p+= tst.size();
	if(node.ignoreAsserts){
		tch[p] = '$'; p++;
		tch[p] = 'I'; p++;
	}
	tch[p] = '.'; p++;
	memcpy(tch+p, node.outname.c_str(), node.outname.size());
	p+=node.outname.size();
	tch[p] = '('; p++;
	writeInt(tch, node.mother->globalId, p);
	tch[p] = ')'; p++;
	tch[p] = '('; p++;
	int mmsize = node.multi_mother.size();
	for(int i=0; i<mmsize; ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
		writeInt(tch, mmid, p);
		tch[p] = ','; p++;
 	}
	tch[p] = ')'; p++;
	Assert(p < mxsz , "Strange!!!!");
	tch[p] = 0;
	ccode = tch;
 	
 	Dtime(stimer.stop();)
}



void DagCSE::visit(  ARRACC_node& node ){
 	Dtime(stimer.restart();)
	int p = 0;
	int mid = node.mother == NULL? -1: node.mother->globalId;
	int mmsize = node.multi_mother.size();
	if(mmsize * 20 > tmpbuf.size()){ tmpbuf.resize(mmsize * 22  );}
	char* tch = &tmpbuf[0];
	writeInt(tch, mid, p);
	tch[p] = '~'; p++;
	for(int i=0; i<mmsize; ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->globalId;
		writeInt(tch, mmid, p);
		tch[p] = ','; p++;
 	}
	tch[p] = 0;
	ccode = tch;
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
 	int mid = node.mother == NULL? -1: node.mother->globalId;
	char* tch = &tmpbuf[0];
	int p = 0;
	writeInt(tch, mid, p);
	tch[p] = '='; p++;
	writeInt(tch, node.quant, p);
	tch[p] = '?'; p++;
	writeInt(tch, node.multi_mother[0]->globalId, p);
	tch[p] = ':'; p++;
	writeInt(tch, node.multi_mother[1]->globalId, p);
	tch[p] = 0;
 	ccode = tch;
    
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
	int mid = node.mother == NULL? -1: node.mother->globalId;
	setStr(0, ';', mid);
 	Dtime(stimer.stop();)
}


