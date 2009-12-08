
#include "Checkpointer.h"


////////////////////////////////////////////////////////////
/////////  	Checkpointing
////////////////////////////////////////////////////////////



void Checkpointer::resizeInput(const string& name, int newsz){
	if( doCheckpoint ){
		cptfile<<'r';
		cptfile<<newsz;
		cptfile<<endl;
	}
}


void  Checkpointer::checkpoint(char nm, vector<int>& ar){
	if( doCheckpoint ){
		cptfile<<nm;
		for(int i=0; i<ar.size(); ++i){
			cptfile<<(ar[i]==1?1:0);
		}
		cptfile<<endl;
	}
}


void Checkpointer::setCheckpoint(const string& filename){
	cptfile.open(filename.c_str());
	doCheckpoint = true;
	cout<<"checkpointing to "<<filename<<endl;
}
