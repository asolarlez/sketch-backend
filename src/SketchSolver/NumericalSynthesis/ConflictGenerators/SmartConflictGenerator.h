#pragma once
#include <gsl/gsl_vector.h>
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include <map>
#include "SymbolicEvaluator.h"
#include "ConflictGenerator.h"

#include <iostream>
#include "Util.h"


class SmartConflictGenerator: public ConflictGenerator {
	SymbolicEvaluator* eval;
	map<int, int>& imap;
	BooleanDAG* dag;
	set<int>& ignoredBoolNodes;
	set<int>& ctrlNodes;
	
	
	vector<set<int>> dependentInputs;
	vector<set<int>> dependentCtrls;

public:
	ConflictGenerator(SymbolicEvaluator* eval_, map<int, int>& imap_, BooleanDAG* dag_, set<int>& ignoredBoolNodes_, set<int>& ctrlNodes_): eval(eval_), imap(imap_), dag(dag_), ignoredBoolNodes(ignoredBoolNodes_), ctrlNodes(ctrlNodes_) {
		// process the dag and collect dependencies
		for (auto node_it = dag->begin(); node_it != dag->end(); ++node_it) {
			bool_node* n = *node_it;
			set<int> inputs; set<int> ctrls;
			
			const vector<bool_node*>& parents = n->parents();
			for (int i = 0; i < parents.size(); i++) {
				bool_node* parent = parents[i];
				set<int>& parentInputs = dependentInputs[parent->id];
				set<int>& parentCtrls = dependentCtrls[parent->id];
				if (parent->getOtype() == OutType::BOOL) {
					if (ignoredBoolNodes.find(parent->id) == ignoredBoolNodes.end()) {
						inputs.insert(parent->id);
						//inputs.insert(parentInputs.begin(), parentInputs.end());
					}
				} else {
					inputs.insert(parentInputs.begin(), parentInputs.end());
					ctrls.insert(parentCtrls.begin(), parentCtrls.end());
				}
				
				if (ctrlNodes.find(parent->id) != ctrlNodes.end()) {
					ctrls.insert(parent->id);
				}
			}
			dependentInputs.push_back(inputs);
			dependentCtrls.push_back(ctrls);
		}
		
	}
	
	bool isConflict(set<int> influentialControls, set<int> myControls) {
		for (int i : influentialControls) {
			if (myControls.find(i) != myControls.end()) {
				return true;
			}
		}
		return false;
	}
	
	virtual vector<pair<int, int>> getConflicts(gsl_vector* state, vector<vector<int>>& allInputs, vector<int>& instanceIds, int rowid, int colid) {
		vector<pair<int, int>> conflicts;
		for (int i = 0; i < allInputs.size(); i++) {
			const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[i]);
			eval->run(state, nodeValsMap);
			
			// First, find a failing node
			bool_node* failedNode = NULL;
			for (auto node_it = dag->begin(); node_it != dag->end(); ++node_it) {
				bool_node* n = *node_it;
				if (n->type == bool_node::ASSERT) {
					if (!eval->check(n->mother, 1)) {
						failedNode = n;
						break;
					}
				}
				if (n->getOtype() == OutType::BOOL) {
					auto it = nodeValsMap.find(n->id);
					if (it != nodeValsMap.end()) {
						int val = it->second;
						if (!eval->check(n, val)) {
							failedNode = n;
							break;
						}
					}
				}
			}
			
			Assert(failedNode != NULL, "No conflict?");
			cout << failedNode->lprint() << endl;
			set<int> conflictNodes;
			// First, add all dependent inputs
			set<int>& depInputs = dependentInputs[failedNode->id];
			conflictNodes.insert(depInputs.begin(), depInputs.end());
			cout << "Dep inputs:" << endl;
			for (auto it = depInputs.begin(); it != depInputs.end(); it++) {
				cout << (*dag)[(*it)]->lprint() << endl;
			}
			
			
			set<int>& depCtrls = dependentCtrls[failedNode->id];
			cout << "Dep ctrls:" << endl;
			for (auto it = depCtrls.begin(); it != depCtrls.end(); it++) {
				cout << (*dag)[(*it)]->lprint() << endl;
			}
			// Next, collect all boolean nodes that are influenced by the controls that affect the failed node
			for (auto node_it = dag->begin(); node_it != dag->end(); ++node_it) {
				bool_node* n = *node_it;
				if (n->type == bool_node::ASSERT) {
					if (isConflict(depCtrls, dependentCtrls[n->id])) {
						conflictNodes.insert(n->id);
					}
				}
				if (n->getOtype() == OutType::BOOL) {
					if (isConflict(depCtrls, dependentCtrls[n->id])) {
						conflictNodes.insert(n->id);
					}
				}
			}
			
			cout << "Final conflict nodes" << endl;
		
			//conflictNodes.insert(colid);
			
			
			// Process the conflicts
			for (int j = 0; j < imap.size(); j++) {
				if (allInputs[i][j] != EMPTY) {
					if (ignoredBoolNodes.find(imap[j]) == ignoredBoolNodes.end()) {
						if (imap[j] < 0 || conflictNodes.find(imap[j]) != conflictNodes.end()) {
							if (imap[j] >= 0) {
								cout << (*dag)[imap[j]]->lprint() << " " << allInputs[i][j] << endl;
							}
							conflicts.push_back(make_pair(instanceIds[i], j));
						}
					}
				}
			}
		}
		return conflicts;
	}
	
};
