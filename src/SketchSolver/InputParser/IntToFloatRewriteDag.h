#ifndef INTOTOFLOATREWRITEDAG_H_
#define INTOTOFLOATREWRITEDAG_H_

#include "DagOptim.h"

class IntToFloatRewriteDag: public DagOptim
{
	float threshold = 0.9;
	BooleanDAG& dag;
	map<int, vector<CTRL_node*> > ctrl_node_id_to_replacement_nodes;
	map<int, vector<bool_node*> > node_id_to_replacement_nodes; 
public:
	IntToFloatRewriteDag(BooleanDAG& _dag, FloatManager& fm): dag(_dag), DagOptim(_dag, fm)
	{
		
	}
	BooleanDAG* rewrite()
	{
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;
		cout << "Hello World" << endl;
		for(int i = 0; i< dag.size(); i++)
		{
			bool_node* node = (dag)[i];
			cout << "node_" <<i << ":: name: " << node->get_name() << ", out_type: " << node->getOtype()->str() << endl;
		}

		dag.lprint(cout);

		process(dag); // main

		dag.lprint(cout);

		cout << "HERE" << endl;
		vector<bool_node*> asserts = dag.getNodesByType(bool_node::ASSERT);
		cout << "Asserts: ";
		for(int i = 0;i<asserts.size();i++)
		{
			cout << asserts[i]->id << " ";
		}
		cout << endl;
		BooleanDAG* ret = &dag;
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;

		return ret;
	}
	void extract_result(gsl_vector* result, map<string, int>& ctrls)
	{
		cout << "###################################################" << endl;
		cout << "#######################RESULT######################" << endl;
		cout << "###################################################" << endl;

		for(auto it : ctrl_node_id_to_replacement_nodes)
		{
			int original_node_id = it.first;
			vector<CTRL_node*> replacement_nodes = it.second;
			cout << "original_node_id = " << original_node_id <<" :: ";
        	for(int i = 0; i< replacement_nodes.size();i ++)
        	{
        		cout << gsl_vector_get(result, ctrls[replacement_nodes[i]->name]) << " "; 
        	}	
        	cout << endl;
		}

		cout << "###################################################" << endl;
		cout << "###################################################" << endl;
		cout << "###################################################" << endl;
	}

	bool_node* after_create(bool_node* node)
	{
		if(node->type != bool_node::CONST)
		{
			node->addToParents();
			node = optAdd(node);
		}
		return node;
	}

	//returns the output bool_node of a circuit that sums the nodes in bool_nodes
	bool_node* sum_nodes(vector<bool_node*> bool_nodes)
	{
		bool_node* ret;
		if(bool_nodes.size()>=2)
		{
			bool_node* new_node = PLUS_node::create(bool_nodes[0], bool_nodes[1]);
			new_node = after_create(new_node);
			for (int i = 2; i < bool_nodes.size();i++)
			{
				new_node = PLUS_node::create(new_node, bool_nodes[i]);
				new_node = after_create(new_node);
			}
			ret = new_node;
		}
		else if (bool_nodes.size() == 1)
		{
			ret = bool_nodes[0];
		}
		else
		{
			ret = getCnode(0.0);
			ret = after_create(ret);
		}
		cout << "return sum_node " << ret->id << " new nodes: ";
		for(int i = 0;i<bool_nodes.size();i++)
		{
			cout << bool_nodes[i]->id << " ";
		}
		cout << endl;
		return ret;
	}


	// creates a circuit that 
	// asserts lower_bound <= node <= higher_bound
	// where lb ~ lower_bound
	// hb ~ higher_bound
	bool_node* assert_lb_lt_node_lt_hb(bool_node* node, float lb, float hb)
	{
		// lb < node
		bool_node* lb_lt_node = node_gt_c(node, lb);
		
		// node < hb
		bool_node* node_hb = getCnode(hb);
		node_hb = after_create(node_hb);
		bool_node* node_lt_hb = LT_node::create(node, node_hb);
		node_lt_hb = after_create(node_lt_hb);
		
		// lb < node and node < hb
		bool_node* lt_and_lt = AND_node::create(lb_lt_node, node_lt_hb);
		lt_and_lt = after_create(lt_and_lt);

		// assert lb < node and node < hb 
		bool_node* assert_node = ASSERT_node::create(lt_and_lt);
		assert_node = after_create(assert_node);
		if(lt_and_lt->type == bool_node::CONST)
		{
			assert(getBval(lt_and_lt));
		}
		else
		{
			dag.assertions.append(getDllnode(assert_node));
		}

		return assert_node;
	}

	bool_node* node_gt_c(bool_node* node, float c)
	{
		bool_node* node_c = getCnode(c);
		node_c = after_create(node_c);
		bool_node* pred = LT_node::create(node_c, node);
		pred = after_create(pred);
		return pred;
	}

	bool_node* assert_max_gt_c(vector<bool_node*> nodes)
	{
		float c = threshold;
		if(nodes.size() >= 2)
		{
			bool_node* node_or = OR_node::create(node_gt_c(nodes[0], c), node_gt_c(nodes[1], c));
			node_or = after_create(node_or);
			for (int i = 2; i< nodes.size();i++)
			{
				node_or = OR_node::create(node_or, node_gt_c(nodes[i], c));
				node_or = after_create(node_or);
			}		
			bool_node* assert_node = ASSERT_node::create(node_or);
			assert_node = after_create(assert_node);
			dag.assertions.append(getDllnode(assert_node));
			return node_or;
		}
		else
		{
			 return node_gt_c(nodes[0], c);
		}
	}

	bool_node* assert_one_hot_constraint(vector<bool_node*> nodes)
	{
		//forall node \in nodes: assert 0.0 <= node <= 1.0
		for(int i = 0; i < nodes.size(); i++) {
			assert_lb_lt_node_lt_hb(nodes[i], -0.001, 1.001);
		}
		assert_max_gt_c(nodes);
		// assert 0.99 <= sum(nodes) <= 1.0 
		bool_node* ret_assert = assert_lb_lt_node_lt_hb(sum_nodes(nodes), threshold, 1.001);
		return ret_assert;
	}

	virtual void visit( CTRL_node& node )
	{
		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			cout << "visit( CTRL_node& node ) at node.id = " << node.id << endl;
			vector<bool_node*> new_nodes;
			vector<CTRL_node*> new_ctrl_nodes;
			//create 2^nbits new float holes for every int. 
			for (int i = 0;i<(1<<node.get_nbits());i++)
			{
				CTRL_node* new_node = CTRL_node::create();
				new_node->setFloat();
				new_node->hasRange = true;
				new_node->low = 0.0;
				new_node->high = 1.0;
				new_node->name = node.get_name() + "_is" + std::to_string(i);
				bool_node* p_new_node = new_node;
				p_new_node = after_create(p_new_node);
				new_nodes.push_back(p_new_node);
				new_ctrl_nodes.push_back(new_node);
			}
			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;
			assert(ctrl_node_id_to_replacement_nodes.find(node.id) == ctrl_node_id_to_replacement_nodes.end());
			ctrl_node_id_to_replacement_nodes[node.id] = new_ctrl_nodes;
			cout << "added " << new_nodes.size() << " new nodes: ";
			for(int i = 0;i<new_nodes.size();i++)
			{
				cout << new_nodes[i]->id << " ";
			}
			cout << endl;
			bool_node* assert_node = assert_one_hot_constraint(new_nodes);
			if (node.getOtype() == OutType::BOOL)
			{
				// replace with an equivalent predicate
				rvalue = node_gt_c(new_nodes[1], threshold);
			}
			else
			{
				DagOptim::visit(node);	
			}
		}
		else
		{
			DagOptim::visit(node);	
		}
	}

	virtual void visit( PLUS_node& node )
	{
		if (node.getOtype() == OutType::INT)
		{
			cout << "visit( PLUS_node& node ) at node.id = " << node.id << endl;
			cout << "parent ids: "  << node.mother()->id << " "<< node.father()->id <<endl;

			bool_node* mother = node.mother();
			bool_node* father = node.father();

			assert(mother->getOtype() == OutType::INT);
			assert(father->getOtype() == OutType::INT);

			bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
			bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

			if(mother_is_one_hot && father_is_one_hot)
			{
				vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
				vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];

				int max_val = replace_mother_nodes.size() - 1 + replace_father_nodes.size() - 1;

				vector<bool_node*> new_nodes;
				for (int sum = 0;sum<=max_val;sum++)
				{
					vector<bool_node*> inter_node_ids;
					for(int new_mother_id = max(0, sum-(int)(replace_father_nodes.size()-1)); 
						new_mother_id < min(sum+1, (int)replace_mother_nodes.size()); new_mother_id+=1)
					{
						int new_father_id = sum - new_mother_id;
						bool_node* new_node = TIMES_node::create(replace_mother_nodes[new_mother_id], replace_father_nodes[new_father_id]);
						new_node = after_create(new_node);
						inter_node_ids.push_back(new_node);
					}
					
					new_nodes.push_back(sum_nodes(inter_node_ids));
				}
				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;

				bool_node* assert_node = assert_one_hot_constraint(new_nodes);
			}
			else if(mother_is_one_hot || father_is_one_hot)
			{
				assert(!mother_is_one_hot || !father_is_one_hot);
				if(father_is_one_hot)
				{
					bool_node* tmp = father;
					father = mother;
					mother = tmp;
				}
				assert(father->type == bool_node::CONST);
				//mother is always one_hot, father is always const
				vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];

				vector<bool_node*> new_nodes;
				for(int i = 0;i<getIval(father);i++)
				{
					bool_node* new_c_node = getCnode(0.0);
					new_c_node = after_create(new_c_node);
					new_nodes.push_back(new_c_node);
				}
				for(int i = 0;i<replace_mother_nodes.size();i++)
				{
					new_nodes.push_back(replace_mother_nodes[i]);
				}

				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;

			}
		}
		DagOptim::visit(node);
	}

	bool_node* create__lb_lt_sum_nodes(vector<bool_node*> to_sum)
	{
		bool_node* sum_node = sum_nodes(to_sum);
		bool_node* const_node = getCnode(threshold);
		const_node = after_create(const_node);		
		bool_node* lt_node = LT_node::create(const_node, sum_node);
		lt_node = after_create(lt_node);
		cout << "create__lb_lt_sum_nodes " << lt_node->id << endl; 
		return lt_node;
	}

	virtual void visit( LT_node& node )
	{

		cout << "visit( LT_node& node ) at node.id = " << node.id << endl;
		cout << "parent ids: "  << node.mother()->id << " "<< node.father()->id <<endl;

		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

		bool has_rvalue = false;
		if(father->type == bool_node::CONST && mother_is_one_hot)
		{
			vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
			if(father->getOtype() == OutType::INT)
			{
				cout << "ENTER 1" << endl;
				vector<bool_node*> to_sum;
				for(int i = 0; i < min((int)replace_mother_nodes.size(), getIval(father)); i++)
				{
					to_sum.push_back(replace_mother_nodes[i]);
				}
				rvalue = create__lb_lt_sum_nodes(to_sum);		
				has_rvalue = true;
			}
		}
		else if(mother->type == bool_node::CONST && father_is_one_hot)
		{
			vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];
			if(mother->getOtype() == OutType::INT)
			{
				cout << "ENTER 2" << endl;
				vector<bool_node*> to_sum;
				for(int i = getIval(mother)+1; i < replace_father_nodes.size(); i++)
				{
					to_sum.push_back(replace_father_nodes[i]);
				}
				rvalue = create__lb_lt_sum_nodes(to_sum);		
				has_rvalue = true;
			}
		}
		else if (mother_is_one_hot && father_is_one_hot)
		{
			vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
			vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];

			vector<bool_node*> to_sum;

			for(int pivot = 0; pivot < replace_father_nodes.size(); pivot++)
			{
				vector<bool_node*> left_sum;
				for(int i = 0;i<min(pivot, (int)replace_mother_nodes.size());i++)
				{
					left_sum.push_back(replace_mother_nodes[i]);
				}
				bool_node* sum_node = sum_nodes(left_sum);

				bool_node* times_node = TIMES_node::create(sum_node, replace_father_nodes[pivot]);
				times_node = after_create(times_node);

				to_sum.push_back(times_node);
			}

			rvalue = node_gt_c(sum_nodes(to_sum), threshold);
			has_rvalue = true;

		}

		if(!has_rvalue)
		{
			DagOptim::visit(node);
		}
	}


	virtual void visit( AND_node& node )
	{

		cout << "visit( AND_node& node ) at node.id = " << node.id << endl;
		cout << "parent ids: "  << node.mother()->id << " "<< node.father()->id <<endl;

		DagOptim::visit(node);
	}	
};

#endif  // INTOTOFLOATREWRITEDAG_H_
