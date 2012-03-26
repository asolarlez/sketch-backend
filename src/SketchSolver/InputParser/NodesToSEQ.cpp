#include "NodesToSEQ.h"


NodesToSEQ::~NodesToSEQ(void)
{
}


void NodesToSEQ::visit (UFUN_node &node){
	NextState ns;
	ns.cond = tval_lookup(node.mother);
	for(int i=0; i<node.multi_mother.size(); ++i){
		ns.state.push_back(tval_lookup(node.multi_mother[i])  );
	}
	nextStates.push_back(ns);

	node_ids[node.id] = dir.YES;
}




void
NodesToSEQ::visit (SRC_node &node)
{

	if( node.children.size() == 0){ return; }

	Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
		
    {
		
		//This could be removed. It's ok to setSize when get_nbits==1.		
		if (node.get_nbits () > 1) {
			int sz = 1;
			for(int i=0; i<node.get_nbits(); ++i){ sz = sz * 2; }
			
			int cvar = dir.newAnonymousVar(sz);
			node_ids[node.id] = cvar;
		    node_ids[node.id].setSize(sz);
			vector<guardedVal>& result = node_ids[node.id].num_ranges;
			result.clear();
			for(int i=0; i<sz; ++i){
				result.push_back(guardedVal(cvar+i, i-1));
			}

		    Dout (cout << "setting input nodes" << node.name << endl);
		    // In the future, I may want to make some of these holes not-sparse.
			node_ids[node.id].sparsify(dir);
		}else{
			node_ids[node.id] = dir.getArr (node.get_name(), 0);
		}
    }

	
	if(node.name == "SCHEDULE"){
		int id = node_ids[node.id].getId();
		output<<".names IN "<<writer.nm( id )<<endl;
		output<<writer.sgn(id)<<" 1"<<endl;
	}
}

void NodesToSEQ::complete(vector<bool_node*>& initState){

	vector<bool_node*>& ins = dag.getNodesByType(bool_node::SRC);
	vector<string> inname;
	for(int i=0; i<ins.size(); ++i){
		SRC_node* srcn = dynamic_cast<SRC_node*>(ins[i]);
		if(srcn->name != "SCHEDULE"){
			input.push_back( node_ids[srcn->id] );
			inname.push_back(srcn->get_name());
		}
	}

	//First, we need to connect all the recursive states into a single state.
	vector<Tvalue> current;
	current.resize(input.size());
	for(int i=0; i<input.size(); ++i){
		current[i] = input[i];
	}

	for(int j=0; j<nextStates.size(); ++j){
		NextState& ns = nextStates[j];
		Assert( ns.state.size() == input.size(), "This is an error");
		Assert(!ns.cond.isSparse(), "Cond can't be sparse");
		for(int i=0; i<ns.state.size(); ++i){
			Tvalue res;
			if(input[i].isSparse()){				
				int flag=false;
				mergeTvalues(ns.cond.getId(), current[i], ns.state[i], res, flag);				
			}else{
				Assert(!ns.state[i].isSparse() && !current[i].isSparse(), "This is an inconsistency");
				int cvar = dir.addChoiceClause(ns.cond.getId() , ns.state[i].getId (), current[i].getId ());
				res = cvar;
			}
			current[i] = res;
		}
	}
	

	for(int i=0; i<input.size(); ++i){
		output<<" # registers "<<inname[i]<<endl;
		Tvalue& tvin = input[i];
		Tvalue& tvout = current[i];
		int iin = 0;
		int iout = 0;		
		if(tvin.isSparse()){
			int v = intVal( initState[i]);
			bool found = false;
			while(iin < tvin.getSize() || iout < tvout.getSize()){
				int idxin;			
				int idxout;
				bool hasin = false;
				bool hasout = false;
				if(iin< tvin.getSize()){
					idxin = tvin.num_ranges[iin].value;
					hasin = true;
				}
				if(iout < tvout.getSize()){
					idxout = tvout.num_ranges[iout].value;
					hasout = true;
				}
				if(hasin && hasout && idxin == idxout){
					int lval = (v==idxin?1:0);
					if(lval == 1){ found = true; }
					output<<".latch "<<writer.nm(tvout.getId(iout))<<"  "<<writer.nm(tvin.getId(iin))<<" "<<lval<<"  # val="<<idxin<<endl;
					++iin;
					++iout;
					continue;
				}
				if((hasin && hasout && idxin < idxout) || !hasout){
					//This means there is a value that the input may take, but that can not be produced by the output.
					//This should never happen, because we are merging with the input, so all the possibilities from the input should be here.
					Assert(false, "This can never happen");
					++iin;
					continue;
				}
				if((hasin && hasout && idxout < idxin) || !hasin){
					//This means there was a value in the ouptut that was not contemplated in the input.
					//For now, I am just going to assert this as false.
					writer.assertVarClause(-tvout.getId(iout));
					output<<"# value "<<idxout<<" is illegal"<<endl;
					++iout;
					continue;
				}
			}
			Assert(found, "This is bad.");
		}else{
			int v = intVal( initState[i]);
			output<<".latch "<<writer.nm(tvout.getId())<<"  "<<writer.nm(tvin.getId())<<" "<<v<<endl;
		}

	}

	
}