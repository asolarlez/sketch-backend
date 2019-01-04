
#include "HoleHardcoder.h"



void HoleHardcoder::afterInline() {
	for (map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ) {
		if (LEAVEALONE(it->second)) {
			randholes.erase(it++);
		}
		else {
			++it;
		}
	}
}


void HoleHardcoder::printControls(ostream& out) {
	for (map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ++it) {
		if (!LEAVEALONE(it->second)) {
			out << it->first << "\t" << it->second << endl;
		}
	}
}

void HoleHardcoder::get_control_map(map<string, string>& values) {
	for (map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ++it) {
		if (!LEAVEALONE(it->second)) {
			stringstream str;
			str << it->second;
			values[it->first] = str.str();
		}
	}	
}





int HoleHardcoder::fixValue(CTRL_node& node, int bound, int nbits) {
	int rv;
	if (node.is_sp_concretize()) {
		Assert(node.max <= bound, "max should be less than bound");
		bound = node.max + 1;

		vector<string> parents = node.predecessors;
		for (int i = 0; i < parents.size(); i++) {
			map<string, int>::iterator it = randholes.find(parents[i]);
			Assert(it != randholes.end(), "Parent hole should have been seen before this node");
			if (it->second >= 0) {
				if ((it->second - i) < bound) {
					bound = max(1, it->second - i);
				}
			}
		}

		int nflips = 0;
		if (bound > 2) {
			nflips = 1;
		}
		//if (bound > 4) {
		// nflips = 2;
		//}
		rv = rand() % bound;
		for (int i = 0; i < nflips; i++) {
			int x = rand() % bound;
			if (x < rv) rv = x;
		}
		cout << "rv for special node " << rv << " bound " << bound << endl;
	} else {
		rv = rand() % bound;
	}
	const string& s = node.get_name();
	Tvalue& glob = globalSat->declareControl(&node);
	Assert(glob.isSparse(), "Must be SPARSE!!!");
	//tloc is a copy of the tvalue in the local sat solver; loc is a pointer to that copy.
	Tvalue* loc = NULL;
	Tvalue tloc;
	if (sat->checkVar(s)) {
		tloc = sat->getControl(s);
		if (!tloc.isSparse()) {
			tloc.makeSparse(*sat, node.get_nbits());
		}
		loc = &tloc;
	}
	const gvvec& options = glob.num_ranges;
	int sz = options.size();
	int bnd = sz;
	if (node.is_sp_concretize()) {
		if (sz > bound) {
			sz = bound;
			bnd = sz - rv;
		}
	}

	for (int i = 0; i<bnd; ++i) {
		const guardedVal& gv = options[(i + rv) % sz];
		int xx = globalSat->getMng().isValKnown(gv.guard);
		if (xx == 0) {
			//global has no opinion. Should check local.
			if (loc != NULL) {
				const gvvec& locopt = loc->num_ranges;
				const guardedVal& lgv = locopt[(i + rv) % sz];
				Assert(lgv.value == gv.value, "Can't happen. c");
				int yy = sat->getMng().isValKnown(lgv.guard);
				if (yy == -1) {
					//local had already decided that this value was bad. Move to the next.
					continue;
				} else {
					//local also has no opinion. 
					//record decision.
					if (!sat->assertIfPossible(lgv.guard)) {
						// tried to set it but didn't work. we'll have to try something different.
						//this is the same as yy==-1
						continue;
					}
					addedConstraint();
					if (!globalSat->tryAssignment(gv.guard)) {
						if (yy == 1) {
							throw BadConcretization();
						}
						else {
							continue;
						}
					}
					if (yy == 1) {
						return gv.value;
					}
					else {
						return recordDecision(options, (i + rv) % sz, sz, node.is_sp_concretize());
					}
				}
			}
			else {
				//There is no local, so we take this value.
				if (!globalSat->tryAssignment(gv.guard)) {
					continue;
				}
				return recordDecision(options, (i + rv) % sz, sz, node.is_sp_concretize());
			}
		}
		if (xx == 1) {
			//global says it should be true. local better agree.
			//no need to record this decision because it was not really a decision.
			if (loc != NULL) {
				const gvvec& locopt = loc->num_ranges;
				const guardedVal& lgv = locopt[(i + rv) % sz];
				Assert(lgv.value == gv.value, "Can't happen. d");
				int yy = sat->getMng().isValKnown(lgv.guard);
				if (yy == -1) {
					//local had already decided that this value was bad. 
					throw BadConcretization();
				}
				else {
					//local has no opinion. return without recording
					if (!sat->assertIfPossible(lgv.guard)) {
						// tried to set it but didn't work. we'll have to try something different.
						//this is the same as yy==-1
						throw BadConcretization();
					}
					addedConstraint();
					return (lgv.value);
				}
			}
			else {
				//There is no local, so we take this value.
				return (gv.value);
			}
		}
		if (xx == -1) {
			//global says it should be false. local better agree.
			//no need to record this decision because it was not really a decision.
			if (loc != NULL) {
				const gvvec& locopt = loc->num_ranges;
				const guardedVal& lgv = locopt[(i + rv) % sz];
				Assert(lgv.value == gv.value, "Can't happen. b");
				int yy = sat->getMng().isValKnown(lgv.guard);
				if (yy == 1) {
					//local had also decided that is should be true.
					throw BadConcretization();
				}
				continue;
			}
			else {
				continue;
			}
		}
	}
	//I wasn't able to concretize to anything. 
	throw BadConcretization();
}

void DepTracker::helper(int harnid, vector<char>& visited, set<int>& out) {
	visited[harnid] = 1;
	vector<Lit>& lits = decisionsPerHarness[harnid];

	for (vector<Lit>::iterator llit = lits.begin(); llit < lits.end(); ++llit) {
		out.insert(toInt(*llit));
	}

	set<int>& holes = holesPerHarness[harnid];
	for (set<int>::iterator it = holes.begin(); it != holes.end(); ++it) {
		set<int>& vi = harnessPerHole[*it];
		if (vi.size()>1) {
			for (set<int>::iterator vvi = vi.begin(); vvi != vi.end(); ++vvi) {
				if (visited[*vvi] == 0) {
					helper(*vvi, visited, out);
				}
			}
		}
	}
}

void DepTracker::genConflict(int harnid, vec<Lit>& out) {
	cout << " charness = " << harnid << endl;
	out.clear();
	vector<char> visited(holesPerHarness.size(), 0);
	set<int> tout;
	helper(harnid, visited, tout);
	for (set<int>::iterator it = tout.begin(); it != tout.end(); ++it) {
		out.push(toLit(*it));
	}
}


int HoleHardcoder::recordDecision(const gvvec& options, int rv, int bnd, bool special) {
	if (!special) {
		const guardedVal& gv = options[rv];
		Lit l = lfromInt(-gv.guard);
		sofar.push(l);
		dt.recordDecision(l);
		return gv.value;
	}
	else {
		for (int i = rv + 1; i < bnd; i++) {
			const guardedVal& gv = options[i];
			Lit l = lfromInt(gv.guard);
			sofar.push(l);
			dt.recordDecision(l);

		}
		return options[rv].value;
	}
}

int HoleHardcoder::computeHoleScore(CTRL_node* node) {
	int tchld = 0;
	int bchld = 0;
	for (childset::iterator chit = node->children.begin(); chit != node->children.end(); ++chit) {
		bool_node* chld = *chit;
		if (chld->type == bool_node::ARRACC) {
			ARRACC_node* an = (ARRACC_node*)chld;
			bool allconst = true;
			for (auto mit = an->arg_begin(); mit != an->arg_end(); ++mit) {
				if ((*mit)->type != bool_node::CONST) {
					allconst = false;
					break;
				}
			}
			if (allconst) {
				// cout<<"     ALLCONSTS "<<an->children.size()<<endl;
				tchld += an->children.size();
			}
			else {
				tchld += 1;
			}
		}
		else if (chld->type == bool_node::NOT) {
			bchld += chld->children.size();
		}
		else {
			if (chld->type == bool_node::AND || chld->type == bool_node::OR) {
				bchld += 1;
			}
			else {
				tchld += 1;
			}
		}
	}
	// TODO: test different boolean weights: 0.5, 0.75, and 1.0
	return tchld + bchld * 0.5;
}

bool_node* HoleHardcoder::checkRandHole(CTRL_node* node, DagOptim& opt) {
	if (node->get_toMinimize()) {
		minholes.insert(node->get_name());
	}

	auto settled = settledHoles.find(node->get_name());
	if (settled != settledHoles.end()) {
		return opt.getCnode(settled->second);
	}

	

	int chsize = node->children.size();
	if (chsize == 0 || node->get_Angelic()) return node;
	string name = node->get_name();
	dt.regHoleInHarness(name);

	map<string, int>::iterator it = randholes.find(name);
	int tchld = computeHoleScore(node);

	if (it != randholes.end()) {
		if (LEAVEALONE(it->second)) {
			int oldchld = -it->second; //how many chlidren it had the first time around.
			if (tchld > oldchld + 10) {
				// cout<<"I've seen this before, but I am going to try again "<< node->lprint() <<" nchildren ="<<tchld<<endl;
			}
			else {
				return node;
			}
		}
		else {
			return opt.getCnode(it->second);
		}
	}
	//At this point, either the hole has never been attempted to concretize,
	//or it was attempted and failed, but we are giving it another chance.
	{
		int baseline = randdegree;
		double odds;
		if (node->is_sp_concretize()) {
			odds = max(1, baseline / (tchld > 0 ? tchld : 1));
		} else {
			odds = (double)baseline / (tchld > 0 ? tchld : 1);
		}
		chsize = tchld;
		if (chsize == 1) {
			bool_node* bn = *node->children.begin();
			if (bn->type == bool_node::DST || bn->type == bool_node::TUPLE_CREATE || bn->type == bool_node::UFUN) {
				// cout<<"postponing for later"<<endl;
				return node;
			} else {
				// cout<<"Single child is "<<bn->lprint()<<endl;
			}
		}
		double p;
		bool conc_flag;
		if (node->is_sp_concretize()) {
			p = 1.0 / odds;
			conc_flag = (chsize > 1500 && totsize < log(10000)) || (chsize > 5000);
			conc_flag = conc_flag || (rand() < p * ((double)RAND_MAX + 1.0));
		} else {
			p = (1.0 / (1.0 + exp(-1.0 / odds)) - 0.5) * 2.0;
			conc_flag = (p > 0) && (rand() < p * ((double)RAND_MAX + 1.0));
		}
		bool reallyConcretize = false;
		//If hardcodeMinholes is set, concflag is true regardless.
		if (node->get_toMinimize() && hardcodeMinholes) {
			conc_flag = true;
			reallyConcretize = true;
		}

		if (conc_flag) {
			cout << node->get_name() << " odds = 1/" << odds << " " << p << " (" << chsize << ", " << tchld << ") " << " try to replace" << endl;
			int bound = 1;

			int nbits = node->get_nbits();
			for (int qq = 0; qq < nbits; ++qq) {
				bound = bound * 2;
			}
			bool ARRASSEQonly = true;
			chkrbuf.resize(bound);
			for (int i = 0; i<bound; ++i) { chkrbuf[i] = false; }
			int obound = bound;
			int ul = -1;
			for (childset::iterator it = node->children.begin(); it != node->children.end(); ++it) {
				// cout<<node->get_name()<<"  "<<(*it)->lprint()<<endl;
				bool_node* child = *it;
				if (child->type == bool_node::LT) {
					if (child->mother() == node && child->father()->type == bool_node::CONST) {
						ul = max(ul, opt.getIval(child->father()));
					}
					else if (child->father() == node && child->mother()->type == bool_node::CONST) {

						if (dynamic_cast<CONST_node*>(child->mother())->getVal() == 0) {
							//nothing to do, but so far so good.
							// cout<<"so far so good"<<child->lprint()<<endl;
						}
						else if (child->children.size() == 1 && (*child->children.begin())->type == bool_node::NOT) {
							ul = max(ul, opt.getIval(child->mother()) + 1);
						}
						else {
							if (!reallyConcretize) {
								//cout << "    has a bad child" << child->lprint() << endl;
								//randholes[node->get_name()] = REALLYLEAVEALONE;
								//return node;
							}
						}
					}
					else {
						if (!reallyConcretize) {
							//cout << "    has a bad child" << child->lprint() << endl;
							//randholes[node->get_name()] = REALLYLEAVEALONE;
							//return node;
						}
					}
					ARRASSEQonly = false;
				}
				else {
					if (child->type == bool_node::ARRACC && child->mother() == node) {
						bound = min(bound, (int)((ARRACC_node*)child)->nargs());
						ARRASSEQonly = false;
					}
					else {
						if (child->type == bool_node::ARRASS && child->mother() == node) {
							int quant = ((ARRASS_node*)child)->quant;
							chkrbuf[quant] = true;
							ul = max(ul, quant);
						}
						else {
							if (!(child->type == bool_node::EQ)) {
								/*
								if(child->type == bool_node::AND || child->type == bool_node::OR || child->type == bool_node::NOT || child->type == bool_node::UFUN){
								// cout<<"so far so good"<<child->lprint()<<endl;
								}else{
								cout<<"    has a bad child"<<child->lprint()<<endl;
								randholes[node->get_name()] = REALLYLEAVEALONE;
								return node;
								}*/
							}
							else { //child->type == eq
								if (child->father()->type == bool_node::CONST) {
									chkrbuf[((CONST_node*)child->father())->getVal()] = true;
								}
								if (child->mother()->type == bool_node::CONST) {
									chkrbuf[((CONST_node*)child->mother())->getVal()] = true;
								}
							}
						}
					}
				}
			}
			if (ul > 0 && bound == obound) {
				bound = min(bound, ul);
			}
			totsize += log(bound);
			int rv = fixValue(*node, bound, nbits);
			randholes[node->get_name()] = rv;
			cout << node->get_name() << ": replacing with value " << rv << " bnd= " << bound << " totsize= " << totsize << endl;
			return opt.getCnode(rv);
		}
		else {
			if (PARAMS->verbosity>5) {
				cout << node->get_name() << " odds = 1/" << odds << "  (" << chsize << ", " << tchld << ") " << " not replacing" << endl;
			}
			if (chsize == 0) { chsize = 1; }
			randholes[node->get_name()] = -chsize;
			return node;
		}
	}
}
