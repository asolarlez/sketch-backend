#include "InequalityHelper.h"


InequalityHelper::InequalityHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): NumericalSolverHelper(_fm, _dag, _imap) {
	//dag->lprint(cout);
	// generate ctrls mapping and counter mapping
	bool_node* counterNode = NULL;
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->type == bool_node::LT) {
			bool_node* m = n->mother;
			bool_node* f = n->father;
			if (m->type == bool_node::CTRL) {
				Assert(f->type == bool_node::CONST, "Not supported inequality");
				string name = m->get_name();
				if (ctrlToNodesMap.find(name) != ctrlToNodesMap.end()) {
					ctrlToNodesMap[name].push_back(n->id);
				} else {
					vector<int> nodes;
					nodes.push_back(n->id);
					ctrlToNodesMap[name] = nodes;
					ctrlToMinValues[name] = 0.0;
					ctrlToMaxValues[name] = 0.0;
				}
			} else if (f->type == bool_node::CTRL) {
				Assert(m->type == bool_node::CONST, "Not supported inequality");
				string name = f->get_name();
				if (ctrlToNodesMap.find(name) != ctrlToNodesMap.end()) {
					ctrlToNodesMap[name].push_back(n->id);
				} else {
					vector<int> nodes;
					nodes.push_back(n->id);
					ctrlToNodesMap[name] = nodes;
				}
			} else if (f->type == bool_node::CONST) {
				// this should be the counter
				Assert(counterNode == NULL, "More than one counter?");
				counterNode = m;
				maxCount = ((CONST_node*)f)->getFval();
			}
		}
	}
	vector<bool_node*> nodesToProcess;
	if (counterNode != NULL) {
		nodesToProcess.push_back(counterNode);
	}
	while (nodesToProcess.size() > 0) {
		bool_node* curnode = nodesToProcess.back();
		nodesToProcess.pop_back();
		Assert(curnode->type == bool_node::PLUS, "Not supported counter");
		bool_node* m = curnode->mother;
		bool_node* f = curnode->father;
		//cout << m->lprint() << endl;
		//cout << f->lprint() << endl;
		if (m->type == bool_node::ARRACC) {
			ARRACC_node* ma = (ARRACC_node*)m;
			Assert(ma->mother->getOtype() == OutType::BOOL, "Not supported counter");
			Assert(ma->multi_mother[0]->type == bool_node::CONST, "Not supported counter");
			Assert(ma->multi_mother[1]->type == bool_node::CONST, "Not supported counter");
			double mval = ((CONST_node*) (ma->multi_mother[0]))->getFval();
			double fval = ((CONST_node*) (ma->multi_mother[1]))->getFval();
			Assert((mval == 0.0 && fval == 1.0) || (mval == 1.0 && fval == 0.0), "Not supported counter");
			if (mval == 0.0) {
				counterPosNodes.push_back(ma->mother->id);
			} else {
				counterNegNodes.push_back(ma->mother->id);
			}
		} else if (m->type == bool_node::PLUS) {
			nodesToProcess.push_back(m);
		} else {
			Assert(false, "Not supported counter");
		}
		
		if (f->type == bool_node::ARRACC) {
			ARRACC_node* fa = (ARRACC_node*)f;
			Assert(fa->mother->getOtype() == OutType::BOOL, "Not supported counter");
			Assert(fa->multi_mother[0]->type == bool_node::CONST, "Not supported counter");
			Assert(fa->multi_mother[1]->type == bool_node::CONST, "Not supported counter");
			double mval = ((CONST_node*) (fa->multi_mother[0]))->getFval();
			double fval = ((CONST_node*) (fa->multi_mother[1]))->getFval();
			Assert((mval == 0.0 && fval == 1.0) || (mval == 1.0 && fval == 0.0), "Not supported counter");
			if (mval == 0.0) {
				counterPosNodes.push_back(fa->mother->id);
			} else {
				counterNegNodes.push_back(fa->mother->id);
			}
		} else if (f->type == bool_node::PLUS) {
			nodesToProcess.push_back(f);
		} else {
			Assert(false, "Not supported counter");
		}
	}
	
	/*cout << "Counter pos nodes ";
	for (int i = 0; i < counterPosNodes.size(); i++) {
		cout << (*dag)[counterPosNodes[i]]->lprint() << ", ";
	}
	cout << endl;
	
	cout << "Counter neg nodes ";
	for (int i = 0; i < counterNegNodes.size(); i++) {
		cout << (*dag)[counterNegNodes[i]]->lprint() << ", ";
	}
	cout << endl;*/
	
	vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
	int ctr = 0;
	for (int i = 0; i < ctrls.size(); i++) {
		if (ctrls[i]->getOtype() == OutType::FLOAT) {
			ctr++;
		}
	}
	Assert(ctr == ctrlToNodesMap.size(), "There are some unused float ctrls?");
}

void InequalityHelper::setInputs(vector<vector<int>>& allInputs_, vector<int>& instanceIds_) {
	allInputs = allInputs_;
	instanceIds = instanceIds_;
}



bool InequalityHelper::checkInputs(int rowid, int colid) {
	int nid = imap[colid];
	if (nid < 0) return true;
	for (int i = 0; i < allInputs[0].size(); i++) {
		if (allInputs[0][i] != 0 && allInputs[0][i] != 1) {
			return false;
		}
	}
	return true;
}

bool InequalityHelper::checkSAT() {
	cout << tcount++ << endl;
	conflictNodes.clear();
	Assert(allInputs.size() == 1, "Multiple inputs not yet supported");
	const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
	if (counterPosNodes.size() > 0 || counterNegNodes.size() > 0) {
		int count = 0;
		//cout << counterPosNodes.size() << endl;
		for (int i : counterPosNodes) {
			auto it = nodeValsMap.find(i);
			if (it != nodeValsMap.end() && it->second == 1) {
				count++;
				conflictNodes.push_back(i);
			}
			if (count > maxCount) {
				//cout << count << "  " << maxCount << endl;
				return false;
			}
		}
		//cout << counterNegNodes.size() << endl;
		for (int i: counterNegNodes) {
			auto it = nodeValsMap.find(i);
			if (it != nodeValsMap.end() && it->second == 0) {
				count++;
				conflictNodes.push_back(i);
			}
			if (count > maxCount) {
				//cout << count << " " << maxCount << endl;
				return false;
			}
		}
		//cout << count << " " << maxCount << endl;
	}
	
	conflictNodes.clear();
	for (auto it = ctrlToNodesMap.begin(); it != ctrlToNodesMap.end(); it++) {
		string name = it->first;
		vector<int>& nodes = it->second;
		double min = GradUtil::MINVAL;
		double max = GradUtil::MAXVAL;
		int minId = 0;
		int maxId = 0;
		for (int i : nodes) {
			auto vit = nodeValsMap.find(i);
			if (vit != nodeValsMap.end()) {
				int val = vit->second;
				bool_node* n = (*dag)[i];
				bool_node* m = n->mother;
				bool_node* f = n->father;
				
				if (m->type == bool_node::CTRL) {
					double thres = ((CONST_node*)f)->getFval();
					if (val == 1) {
						if (thres < max) {
							max = thres;
							maxId = i;
						}
					} else {
						if (thres > min) {
							min = thres;
							minId = i;
						}
					}
				}
				if (f->type == bool_node::CTRL) {
					double thres = ((CONST_node*)m)->getFval();
					if (val == 0) {
						if (thres < max) {
							max = thres;
							maxId = i;
						}
					} else {
						if (thres > min) {
							min = thres;
							minId = i;
						}
					}
				}
			}
			if (min > max) {
				conflictNodes.push_back(minId);
				conflictNodes.push_back(maxId);
				return false;
			}
		}

		ctrlToMinValues[name] = min;
		ctrlToMaxValues[name] = max;
		//Assert(min <= max, "Something is wrong here");
		if (min <= max) {
		if (min == GradUtil::MINVAL && max == GradUtil::MAXVAL) {
			ctrlVals[name] = 0.0;
		} else if (min == GradUtil::MINVAL) {
			ctrlVals[name] = max - 10.0;
		} else if (max == GradUtil::MAXVAL) {
			ctrlVals[name] = min + 10.0;
		} else {
			ctrlVals[name] = (min + max)/2.0;
		}
		}
	}
	return true;
}

bool InequalityHelper::ignoreConflict() {
	return false;
}

vector<tuple<int, int, int>> InequalityHelper::collectSuggestions() {
	vector<tuple<int, int, int>> suggestions;
	
	/*for (int i = 0; i < allInputs[0].size(); i++) {
		if (allInputs[0][i] != 0 && allInputs[0][i] != 1) {
			bool_node* n = (*dag)[imap[i]];
			if (n->type == bool_node::LT) {
				bool_node* m = n->mother;
				bool_node* f = n->father;
				if (m->type == bool_node::CTRL) {
					string name = m->get_name();
					double val = ctrlVals[name];
					bool expected = val < ((CONST_node*)f)->getFval();
					suggestions.push_back(make_tuple(0, i, expected));
				}
				
				if (f->type == bool_node::CTRL) {
					string name = f->get_name();
					double val = ctrlVals[name];
					bool expected = ((CONST_node*)m)->getFval() < val;
					suggestions.push_back(make_tuple(0, i, expected));
				}
			}
		}
	}*/
	
	return suggestions;
}

vector<pair<int, int>> InequalityHelper::getConflicts(int rowid, int colid) {
	vector<pair<int, int>> conflicts;
	for (int i = 0 ; i < allInputs[0].size(); i++) {
		if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
			int nid = imap[i];
			if (find(conflictNodes.begin(), conflictNodes.end(), nid) != conflictNodes.end()) {
				//cout << (*dag)[nid]->lprint() << endl;
				conflicts.push_back(make_pair(0, i));
			}
		}
	}
	conflicts.push_back(make_pair(rowid, colid)); // add the recently set bit to the conflict
	return conflicts;
}


void InequalityHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
		ctrls[it->first] = it->second;
	}
}
