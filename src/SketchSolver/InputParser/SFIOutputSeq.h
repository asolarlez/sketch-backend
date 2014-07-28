#pragma once
#include "SolveFromInput.h"
#include "NodesToSEQ.h"
#include "BLIFwriter.h"
#include "CEGISSolver.h"
#include "CommandLineArgs.h"

class SFIOutputSeq :
	public CEGISSolver
{
public:
	BooleanDAG* seqMiter;

	BooleanDAG* initializer;
	int iter;

	SFIOutputSeq(ostream& out_p, BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, CommandLineArgs& args, BooleanDAG* seqMiter_p, BooleanDAG* initializer_p):
	  CEGISSolver(finder,args)
	{
		Assert(false, "This does not work anymore");
		seqMiter = seqMiter_p;
		initializer = initializer_p;
		iter = 0;
	}
public:

	///In the original 
	UFUN_node* fixSchedule(BooleanDAG* seq, BooleanDAG* init){
		vector<bool_node*> bn = seq->getNodesByType(bool_node::SRC);
		bool_node* srcrep = NULL;
		for(int i=0; i<bn.size(); ++i){
			srcrep = bn[i];
			int pos = srcrep->get_name().find("schedule");
			if(pos>=0){
				srcrep = bn[i];
				break;
			}
		}
		Assert(srcrep->children.size() == 3, "This can't happen");
		bool_node* schedRoot;
		child_iter it= srcrep->children.begin();
		do{
			schedRoot = *it;
			++it;
		}while(typeid(*schedRoot)!= typeid(ARRACC_node));

		SRC_node* nsrc  = new SRC_node("SCHEDULE");
		nsrc->id = seq->size();
		seq->addNewNode(nsrc);
		seq->replace(schedRoot->id, nsrc);
		UFUN_node* inufun = NULL;
		for(int i=0; i<init->size(); i++){
			if(typeid(*(*init)[i])==typeid(UFUN_node)){
				inufun = dynamic_cast<UFUN_node*>((*init)[i]);
				break;
			}
		}


		bool_node* badzero = new CONST_node(0);
		vector<bool_node*> nins;
		for(int i=0; i<bn.size(); ++i){
			if(bn[i]->get_name().find("schedule")!=-1){
				seq->replace(bn[i]->id, nsrc);
			}else{
				if(bn[i]->get_name().find("count")==-1 && bn[i]->get_name().find("_step")==-1){
					nins.push_back(inufun->multi_mother[i]);
				}else{					
					seq->replace(bn[i]->id, badzero);
					while(true){
						childset tmpset(badzero->children);
						child_iter it = tmpset.begin();
						bool foundsomething = false;
						while(it != tmpset.end()){
							if(typeid(**it)!=typeid(UFUN_node)){
								seq->replace((*it)->id, badzero);
								foundsomething = true;
							}
							++it;
						}
						if(!foundsomething) break;
					}
				}
			}
		}
		swap(nins, inufun->multi_mother);

		childset nch(nsrc->children);
		it = nch.begin();
		while(it != nch.end()){
			if(typeid(**it)==typeid(UFUN_node)){
				nsrc->children.erase(*it);
				UFUN_node* rc = dynamic_cast<UFUN_node*>(*it);
				vector<bool_node*>& op = rc->multi_mother;
				vector<bool_node*> np;
				for(int i=0; i<op.size(); ++i){
					if(op[i] != nsrc && op[i] != badzero){
						np.push_back(op[i]);
					}
				}
				swap(op, np);
			}
			++it;
		}

		seq->removeNullNodes();
		seq->cleanup();
		//seq->sort_graph();
		return inufun;
	}

	void setNewControls(VarStore& controls){


		BooleanDAG* seq = hardCodeINode(seqMiter, controls, bool_node::CTRL);
		BooleanDAG* init = hardCodeINode(initializer, controls, bool_node::CTRL);
		UFUN_node* ufinit = fixSchedule(seq, init);

		


		string name("seq_miter_");
		name += int2str(iter);
		++iter;
		name += ".blif";

		{
			BLIFwriter writer(name, SATSolver::CHECKER);
			SolverHelper sh(writer);
			sh.setYes();
			vector<bool_node*>& ins = seq->getNodesByType(bool_node::SRC);
			for(vector<bool_node*>::iterator it = ins.begin(); it != ins.end(); ++it){	
				SRC_node* n = dynamic_cast<SRC_node*>(*it);
				if(n->get_nbits() > 1){
					n->set_nbits(5);
				}
				sh.declareInArr((*it)->get_name(), n->get_nbits());
			}
			


			map<bool_node*, int> nvals;
			vector<Tvalue> p_node_ids;
			p_node_ids.resize(seq->size());
			NodesToSEQ ns(writer.output, writer, sh, name, nvals, p_node_ids,*seq);
			ns.process(*seq);
			ns.complete(ufinit->multi_mother);
			writer.solve();
		}

		delete seq;
		delete init;

		// CEGISSolver::setNewControls(controls, dirC);
	}

	void output_control_map(ostream& out){
		Assert(false, "NYI");
	}

	virtual ~SFIOutputSeq(void)
	{
	}
};
