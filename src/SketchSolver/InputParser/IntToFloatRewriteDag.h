#ifndef INTOTOFLOATREWRITEDAG_H_
#define INTOTOFLOATREWRITEDAG_H_

#include "DagOptim.h"

class IntToFloatRewriteDag: public DagOptim
{
	bool meta_print = true;
	float threshold = 0.9;
	float sum_trehsold = 0.95;
	float epsilon = 0.001;
	BooleanDAG& dag;
	map<string, vector<CTRL_node*> > ctrl_node_name_to_replacement_nodes;
	set<string> float_holes;
	map<int, vector<bool_node*> > node_id_to_replacement_nodes;
	set<int> inserted_asserts; 
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

		/*
		todo: handle NEG(int);
		
		int = int | bool
	    DONE;; case PLUS: return "PLUS";
        DONE;; case TIMES: return "TIMES";
        DONE;; case DIV: return "DIV";
        DONE;; case MOD: return "MOD";
        TODO;; case NEG: return "NEG";
        DONE +int; TODO -int;; case CONST: return "CONST";
        DONE;; case AND: return "AND";
        DONE;; case OR: return "OR";
        DONE;; case XOR: return "XOR";
        /;; case SRC: return "S";
        /;; case DST: return "D";
        DONE;; case NOT: return "NOT";
        DONE;; case CTRL: return "CTRL";
        DONE;; case LT: return "LT";
        DONE;; case EQ: return "EQ";
        DONE;; case ASSERT: return "ASSERT";
        TODO;; case ARRACC: return "ARRACC"; 
        	DONE len == 2; 
        	DONE len > 2 for floats; 
        	TODO: len > 2 for INTS;
        For later;; case UFUN: return "UFUN";
        For later;; case ACTRL: return "ACTRL";
        DONE;; case ARRASS: return "ARRASS";
        TODO;; case ARR_R: return "ARR_R";
        TODO;; case ARR_W: return "ARR_W";
        TODO;; case ARR_CREATE: return "ARR_CREATE";
        For later;; case TUPLE_CREATE: return "TUPLE_CREATE";
        For later;; case TUPLE_R: return "TUPLE_R";

		1. Implement ARR_CREATE for float 
		2. Implement ARR_R as ARRACC
			convert to an ARRACC and call ARRACC
		3. Implement ARR_W as ARRASS
			convert to ARRASS and call ARRASS
		
		*/
		process(dag); // main

		dag.lprint(cout);


		BooleanDAG* ret = &dag;
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;
		cout << "----------------------------------------------------------" << endl;

		return ret;
	}
	map<string, string> extract_result(gsl_vector* result, map<string, int>& ctrls)
	{
		map<string, int> currentControlInts;
		map<string, float> currentControlFloats;

		return extract_result_typed(result, ctrls, currentControlInts, currentControlFloats);
	}

	map<string, string> extract_result_typed(
		gsl_vector* result, map<string, int>& ctrls, 
		map<string, int>& currentControlInts, map<string, float>& currentControlFloats)
	{

		cout << "###################################################" << endl;
		cout << "###################  RESULT  ######################" << endl;
		cout << "###################################################" << endl;

		map<string, string> ret;	
		for(auto it : ctrl_node_name_to_replacement_nodes)
		{
			string original_node_name = it.first;
			vector<CTRL_node*> replacement_nodes = it.second;
			cout  << original_node_name <<" :: ";
			int best_id = -1;
			float best = -1;
        	for(int i = 0; i< replacement_nodes.size();i ++)
        	{
        		float val = gsl_vector_get(result, ctrls[replacement_nodes[i]->get_name()]);
        		cout << val << " "; 
        		if (val > best)
        		{
        			best = val;
        			best_id = i;
        		}
        	}	
        	cout << " | argmax = " << best_id << endl;
        	ret[original_node_name] = std::to_string(best_id);
        	currentControlInts[original_node_name] = best_id;
		}

		for(auto it : float_holes)
		{
			float local_res = gsl_vector_get(result, ctrls[it]);
			ret[it] = std::to_string(local_res);
			currentControlFloats[it] = local_res;
			cout << it << " = " << local_res << endl;
		}

		cout << "###################################################" << endl;
		cout << "###################################################" << endl;
		cout << "###################################################" << endl;
		
		return ret;
	}

	bool_node* after_create(bool_node* node)
	{
		cout << "new node (pre add) : " << node->lprint() << endl;
		if(node->type != bool_node::CONST)
		{
			node->addToParents();
			node = optAdd(node);
			if(node->type == bool_node::ASSERT)
			{
				if(inserted_asserts.find(node->id) == inserted_asserts.end())
				{
					inserted_asserts.insert(node->id);
					bool_node* parent = node->get_parent(0);
					if(parent->type == bool_node::CONST)
					{
						assert(getBval(parent));
					}
					else
					{
						dag.assertions.append(getDllnode(node));
					}
				}
			}
		}
		cout << "new node (post add): " << node->lprint() << endl;
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
		return ret;
	}

	bool_node* get_node_lt_hb(bool_node* node, float hb)
	{

		bool_node* node_hb = getCnode(hb);
		node_hb = after_create(node_hb);
		bool_node* node_lt_hb = LT_node::create(node, node_hb);
		node_lt_hb = after_create(node_lt_hb);

		return node_lt_hb;
	}


	// creates a circuit that 
	// asserts lower_bound <= node <= higher_bound
	// where lb ~ lower_bound
	// hb ~ higher_bound
	bool_node* assert_lb_lt_node_lt_hb(bool_node* node, float lb, float hb)
	{
		// lb < node
		bool_node* lb_lt_node = get_node_gt_lb(node, lb);
		
		// node < hb
		bool_node* node_lt_hb = get_node_lt_hb(node, hb);
		
		// lb < node and node < hb
		bool_node* lt_and_lt = AND_node::create(lb_lt_node, node_lt_hb);
		lt_and_lt = after_create(lt_and_lt);

		// assert lb < node and node < hb 
		bool_node* assert_node = ASSERT_node::create(lt_and_lt);
		assert_node = after_create(assert_node);


		return assert_node;
	}

	bool_node* get_node_gt_lb(bool_node* node, float c)
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
			bool_node* node_or = OR_node::create(get_node_gt_lb(nodes[0], c), get_node_gt_lb(nodes[1], c));
			node_or = after_create(node_or);
			for (int i = 2; i< nodes.size();i++)
			{
				node_or = OR_node::create(node_or, get_node_gt_lb(nodes[i], c));
				node_or = after_create(node_or);
			}		
			bool_node* assert_node = ASSERT_node::create(node_or);
			assert_node = after_create(assert_node);
			return node_or;
		}
		else
		{
			 return get_node_gt_lb(nodes[0], c);
		}
	}

	bool_node* assert_one_hot_constraint(vector<bool_node*> nodes)
	{
		//forall node \in nodes: assert 0.0 <= node <= 1.0
		for(int i = 0; i < nodes.size(); i++) {
			assert_lb_lt_node_lt_hb(nodes[i], -epsilon, 1+epsilon);
		}
		assert_max_gt_c(nodes);
		// assert 0.99 <= sum(nodes) <= 1.0 
		bool_node* ret_assert = assert_lb_lt_node_lt_hb(sum_nodes(nodes), sum_trehsold, 1+epsilon);
		return ret_assert;
	}

	virtual void visit( CTRL_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( CTRL_node& node ); node.id = " << node.id << endl;

		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			if(print)
			cout << "ENTER" << endl;

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
			assert(ctrl_node_name_to_replacement_nodes.find(node.get_name()) == ctrl_node_name_to_replacement_nodes.end());
			ctrl_node_name_to_replacement_nodes[node.get_name()] = new_ctrl_nodes;

			assert_one_hot_constraint(new_nodes);
		}
		else if(node.getOtype() == OutType::FLOAT)
		{
			assert(float_holes.find(node.get_name()) == float_holes.end());
			float_holes.insert(node.get_name());
		}
		DagOptim::visit(node);
	}

	virtual void visit(CONST_node& init_node)
	{
		DagOptim::visit(init_node);	
		if(init_node.id == -1) {
			return;
		}
		CONST_node& node = (CONST_node&)*rvalue;

		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( CONST_node& node ); node.id = " << node.id << endl;

		if(node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{		
			if(print)
			cout << "ENTER" << endl;

			vector<bool_node*> new_nodes;
			int c = getIval(&node);
			for(int i = 0;i<c;i++)
			{
				new_nodes.push_back(after_create(getCnode(0.0)));
			}
			new_nodes.push_back(after_create(getCnode(1.0)));

			if(c == 0)
			{
				new_nodes.push_back(after_create(getCnode(0.0)));
			}

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;
		}

	}

	virtual void visit( PLUS_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( PLUS_node& node ); node.id = " << node.id << endl;
		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			bool_node* mother = node.mother();
			bool_node* father = node.father();

			assert(mother->getOtype() == OutType::INT || mother->getOtype() == OutType::BOOL);
			assert(father->getOtype() == OutType::INT || father->getOtype() == OutType::BOOL);

			bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
			bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

			if(mother_is_one_hot && father_is_one_hot)
			{
				if(print)
				cout << "ENTER 1" << endl;
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

				assert_one_hot_constraint(new_nodes);
			}
			else
			{
				cout << "ALL INTS AND BOOLS SHOULD HAVE node_id_to_replacement_nodes" << endl;
				assert(false);
			}
		}
		DagOptim::visit(node);
	}

	virtual void visit( TIMES_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( TIMES_node& node ); node.id = " << node.id << endl;
		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			bool_node* mother = node.mother();
			bool_node* father = node.father();

			assert(mother->getOtype() == OutType::INT || mother->getOtype() == OutType::BOOL);
			assert(father->getOtype() == OutType::INT || father->getOtype() == OutType::BOOL);

			bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
			bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

			if(mother_is_one_hot && father_is_one_hot)
			{
				if(print)
				cout << "ENTER" << endl;
				vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
				vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];

				int max_val = (replace_mother_nodes.size() - 1) * (replace_father_nodes.size() - 1);

				vector<vector<bool_node*> > matrix = vector<vector<bool_node*> >(max_val+1, vector<bool_node*> ());

				for(int i = 0;i<replace_mother_nodes.size();i++)
				{
					for(int j = 0;j<replace_father_nodes.size();j++)
					{
						matrix[i*j].push_back(my_mult(replace_mother_nodes[i], replace_father_nodes[j]));
					}
				}
				vector<bool_node*> new_nodes;
				for(int i = 0;i<matrix.size();i++)
				{
					new_nodes.push_back(sum_nodes(matrix[i]));
				}
				
				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;

				assert_one_hot_constraint(new_nodes);
			}
			else
			{
				cout << "ALL INTS AND BOOLS SHOULD HAVE node_id_to_replacement_nodes" << endl;
				assert(false);
			}
		}
		DagOptim::visit(node);
	}

	virtual void visit( DIV_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( DIV_node& node ); node.id = " << node.id << endl;
		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			bool_node* mother = node.mother();
			bool_node* father = node.father();

			assert(mother->getOtype() == OutType::INT || mother->getOtype() == OutType::BOOL);
			assert(father->getOtype() == OutType::INT || father->getOtype() == OutType::BOOL);

			bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
			bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

			if(mother_is_one_hot && father_is_one_hot)
			{
				if(print)
				cout << "ENTER" << endl;
				vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
				vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];

				int max_val = replace_mother_nodes.size() - 1;

				vector<vector<bool_node*> > matrix = vector<vector<bool_node*> >(max_val+1, vector<bool_node*> ());

				after_create(ASSERT_node::create(get_node_lt_hb(replace_father_nodes[0], epsilon)));

				for(int i = 0;i<replace_mother_nodes.size();i++)
				{
					for(int j = 1;j<replace_father_nodes.size();j++)
					{
						matrix[i/j].push_back(my_mult(replace_mother_nodes[i], replace_father_nodes[j]));
					}
				}
				vector<bool_node*> new_nodes;
				for(int i = 0;i<matrix.size();i++)
				{
					new_nodes.push_back(sum_nodes(matrix[i]));
				}
				
				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;

				assert_one_hot_constraint(new_nodes);
			}
			else
			{
				cout << "ALL INTS AND BOOLS SHOULD HAVE node_id_to_replacement_nodes" << endl;
				assert(false);
			}
		}
		DagOptim::visit(node);
	}

	virtual void visit( MOD_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( MOD_node& node ); node.id = " << node.id << endl;
		if (node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL)
		{
			bool_node* mother = node.mother();
			bool_node* father = node.father();

			assert(mother->getOtype() == OutType::INT || mother->getOtype() == OutType::BOOL);
			assert(father->getOtype() == OutType::INT || father->getOtype() == OutType::BOOL);

			bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
			bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

			if(mother_is_one_hot && father_is_one_hot)
			{
				if(print)
				cout << "ENTER" << endl;
				vector<bool_node*> replace_mother_nodes = node_id_to_replacement_nodes[mother->id];
				vector<bool_node*> replace_father_nodes = node_id_to_replacement_nodes[father->id];

				int max_val = replace_mother_nodes.size() - 1 - 1;

				vector<vector<bool_node*> > matrix = vector<vector<bool_node*> >(max_val+1, vector<bool_node*> ());

				after_create(ASSERT_node::create(get_node_lt_hb(replace_father_nodes[0], epsilon)));

				for(int i = 0;i<replace_mother_nodes.size();i++)
				{
					for(int j = 1;j<replace_father_nodes.size();j++)
					{
						matrix[i%j].push_back(my_mult(replace_mother_nodes[i], replace_father_nodes[j]));
					}
				}
				vector<bool_node*> new_nodes;
				for(int i = 0;i<matrix.size();i++)
				{
					new_nodes.push_back(sum_nodes(matrix[i]));
				}
				
				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;

				assert_one_hot_constraint(new_nodes);
			}
			else
			{
				cout << "ALL INTS AND BOOLS SHOULD HAVE node_id_to_replacement_nodes" << endl;
				assert(false);
			}
		}
		DagOptim::visit(node);
	}

	bool_node* one_minus_node(bool_node* node)
	{
		bool_node* c_one = after_create(getCnode(1.0));
		bool_node* c_minus_one = after_create(getCnode(-1.0));
		bool_node* one_minus_node = my_sum(c_one, my_mult(c_minus_one, node));
		cout << "EXIT one_minus_node" << endl;
		return one_minus_node;
	}

	virtual void visit( LT_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( LT_node& node ); node.id = " << node.id << endl;
		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

		if (mother_is_one_hot && father_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
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

			bool_node* sum_node = sum_nodes(to_sum);

			vector<bool_node*> bool_as_floats;
			bool_as_floats.push_back(one_minus_node(sum_node));
			bool_as_floats.push_back(sum_node);

			assert_max_gt_c(bool_as_floats);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = bool_as_floats;
		}


		DagOptim::visit(node);
	}

	virtual void visit(ASSERT_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( ASSERT_node& node ); node.id = " << node.id << endl;
		int cond_id = node.get_parent(0)->id;

		bool cond_is_one_hot = node_id_to_replacement_nodes.find(cond_id) != node_id_to_replacement_nodes.end();

		if(print)
		{
			cout << cond_id << endl;
			cout << cond_is_one_hot << endl;
		}

		if(cond_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> cond_one_hot = node_id_to_replacement_nodes[cond_id];
			assert(cond_one_hot.size() == 2);

			bool_node* new_assert_node = ASSERT_node::create(get_node_gt_lb(cond_one_hot[1], threshold));
			new_assert_node = after_create(new_assert_node);

			rvalue = new_assert_node;
		}
		else
		{
			DagOptim::visit(node);
		}
	}

	bool_node* my_mult(bool_node* left, bool_node* right)
	{
		bool_node* ret = TIMES_node::create(left, right);
		ret = after_create(ret);
		return ret;
	}


	bool_node* my_sum(bool_node* left, bool_node* right)
	{
		bool_node* ret = PLUS_node::create(left, right);
		ret = after_create(ret);
		return ret;
	}

	virtual void visit( AND_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( AND_node& node ); node.id = " << node.id << endl;
		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

		if(mother_is_one_hot && father_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> left = node_id_to_replacement_nodes[mother->id];
			vector<bool_node*> right = node_id_to_replacement_nodes[father->id];
			assert(left.size() == 2);
			assert(right.size() == 2);

			vector<bool_node*> new_nodes;

			// left[0]*right[0]+left[0]*right[1] + left[1]*right[0];
			bool_node* nand_case = my_sum(my_sum(my_mult(left[0], right[0]), my_mult(left[0], right[1])), my_mult(left[1], right[0]));

			// left[1]*right[1]
			bool_node* and_case = my_mult(left[1], right[1]);

			new_nodes.push_back(nand_case);
			new_nodes.push_back(and_case);

			assert_one_hot_constraint(new_nodes);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;

		}

		DagOptim::visit(node);
	}	

	virtual void visit( OR_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( OR_node& node ); node.id = " << node.id << endl;
		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

		if(mother_is_one_hot && father_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> left = node_id_to_replacement_nodes[mother->id];
			vector<bool_node*> right = node_id_to_replacement_nodes[father->id];
			assert(left.size() == 2);
			assert(right.size() == 2);

			vector<bool_node*> new_nodes;

			bool_node* or_case = my_sum(my_sum(my_mult(left[1], right[0]), my_mult(left[0], right[1])), my_mult(left[1], right[1]));
			bool_node* nor_case = my_mult(left[0], right[0]);

			new_nodes.push_back(nor_case);
			new_nodes.push_back(or_case);

			assert_one_hot_constraint(new_nodes);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;

		}

		DagOptim::visit(node);
	}	

	virtual void visit( XOR_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( XOR_node& node ); node.id = " << node.id << endl;
		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();

		if(print)
		{
			cout << mother->id <<" "<< father->id << endl;
			cout << mother_is_one_hot << " " << father_is_one_hot << endl;
		}

		if(mother_is_one_hot && father_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> left = node_id_to_replacement_nodes[mother->id];
			vector<bool_node*> right = node_id_to_replacement_nodes[father->id];
			assert(left.size() == 2);
			assert(right.size() == 2);

			vector<bool_node*> new_nodes;

			bool_node* xor_case = my_sum(my_mult(left[1], right[0]), my_mult(left[0], right[1]));
			bool_node* xnot_case = my_sum(my_mult(left[1], right[1]), my_mult(left[0], right[0]));

			new_nodes.push_back(xnot_case);
			new_nodes.push_back(xor_case);

			assert_one_hot_constraint(new_nodes);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;

		}

		DagOptim::visit(node);
	}	

	virtual void visit( EQ_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( EQ_node& node ); node.id = " << node.id << endl;
		bool_node* mother = node.mother();
		bool_node* father = node.father();

		bool mother_is_one_hot = node_id_to_replacement_nodes.find(mother->id) != node_id_to_replacement_nodes.end();
		bool father_is_one_hot = node_id_to_replacement_nodes.find(father->id) != node_id_to_replacement_nodes.end();
		if(print)
		{
			cout << mother->id <<" "<< father->id << endl;
			cout << mother_is_one_hot << " "  << father_is_one_hot << endl;
		}
		if(mother_is_one_hot && father_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> left = node_id_to_replacement_nodes[mother->id];
			vector<bool_node*> right = node_id_to_replacement_nodes[father->id];
			vector<bool_node*> new_nodes;

			vector<bool_node*> mults;
			for(int i = 0;i<min((int)left.size(), (int) right.size()); i++)
			{
				mults.push_back(my_mult(left[i], right[i]));
			}

			bool_node* eq_case = sum_nodes(mults);
			bool_node* neq_case = one_minus_node(eq_case);

			new_nodes.push_back(neq_case);
			new_nodes.push_back(eq_case);

			assert_max_gt_c(new_nodes);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;

		}

		DagOptim::visit(node);
	}	


	virtual void visit( NOT_node& node )
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( NOT_node& node ); node.id = " << node.id << endl;

		bool_node* parent = node.mother();

		bool parent_is_one_hot = node_id_to_replacement_nodes.find(parent->id) != node_id_to_replacement_nodes.end();

		if(parent_is_one_hot)
		{
			if(print)
			cout << "ENTER" << endl;
			vector<bool_node*> parent_nodes = node_id_to_replacement_nodes[parent->id];

			assert(parent_nodes.size() == 2);

			vector<bool_node*> new_nodes;

			new_nodes.push_back(parent_nodes[1]);
			new_nodes.push_back(parent_nodes[0]);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;

		}

		DagOptim::visit(node);
	}	

	virtual void visit(ARR_CREATE_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;

		if(node.getOtype() == OutType::FLOAT_ARR)
		{
			cout << "THIS STILL HASN'T BEEN TESTED." << endl;
			assert(false);
			int n = node.nargs();
			vector<bool_node*> new_nodes;
			for(int i = 0;i<n;i++)
			{
				new_nodes.push_back(node.get_parent(i));
			}

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;
		}
		
		DagOptim::visit(node);
	}

	void make_vectors_equal_length(vector<bool_node*>& vec1, vector<bool_node*>& vec2)
	{
		while(vec1.size() > vec2.size())
		{
			vec2.push_back(after_create(getCnode(0.0)));
		}
		while(vec1.size() < vec2.size())
		{
			vec1.push_back(after_create(getCnode(0.0)));
		}
		assert(vec1.size() == vec2.size());
	}

	virtual void visit(ARRACC_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( ARRACC_node& node ); node.id = " << node.id << endl;

		bool_node* cond_node = node.get_parent(0);
		bool_node* if_false_node = node.get_parent(1);
		bool_node* if_true_node = node.get_parent(2);

		if(print)
		cout << cond_node->id << " " << if_false_node->id <<" "<< if_true_node->id << endl;

		bool cond_is_one_hot = node_id_to_replacement_nodes.find(cond_node->id) != node_id_to_replacement_nodes.end();
		bool if_true_is_one_hot = node_id_to_replacement_nodes.find(if_false_node->id) != node_id_to_replacement_nodes.end();
		bool if_false_is_one_hot = node_id_to_replacement_nodes.find(if_true_node->id) != node_id_to_replacement_nodes.end();

		if(print)
		{
			cout << cond_is_one_hot <<" "<< if_true_is_one_hot << " " << if_false_is_one_hot << endl;
		}
		if(cond_is_one_hot && if_true_is_one_hot && if_false_is_one_hot)
		{
			if(print)
			cout << "ENTER 1" << endl;
			vector<bool_node*> cond_one_hot = node_id_to_replacement_nodes[cond_node->id];
			vector<bool_node*> if_true_one_hot = node_id_to_replacement_nodes[if_true_node->id];
			vector<bool_node*> if_false_one_hot = node_id_to_replacement_nodes[if_false_node->id];

			make_vectors_equal_length(if_true_one_hot, if_false_one_hot);

			assert(cond_one_hot.size() == 2);

			vector<bool_node*> new_nodes;

			int len = if_true_one_hot.size();

			for(int i = 0;i<len;i++)
			{
				//cond_one_hot[1]*if_true_one_hot[i] +cond_one_hot[0]*if_false_one_hot[i];
				bool_node* both = 
					my_sum(	
							my_mult(cond_one_hot[0], if_false_one_hot[i]), 
							my_mult(cond_one_hot[1], if_true_one_hot[i]));
				new_nodes.push_back(both);	
			}

			assert_one_hot_constraint(new_nodes);

			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;
		}
		else if(cond_is_one_hot && !if_true_is_one_hot && !if_false_is_one_hot)
		{
			vector<bool_node*> cond_one_hot = node_id_to_replacement_nodes[cond_node->id];

			bool all_floats = true;
			int n = node.nargs();

			assert(cond_one_hot.size() >= n);

			for(int i = 1;i<=n;i++)
			{
				all_floats &= node.get_parent(i)->getOtype() == OutType::FLOAT;
			}

			if(all_floats)
			{
				if(print) cout << "ENTER 2" << endl;

				vector<bool_node*> dot_product;
				for(int i = 1;i<=n;i++)
				{
					dot_product.push_back(my_mult(cond_one_hot[i-1], node.get_parent(i)));
				}

				rvalue = sum_nodes(dot_product);
				return;
			}
		}
		
		DagOptim::visit(node);			
	}


	virtual void visit(ARRASS_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;
		if(print)
		cout << "IN visit( ARRASS_node& node ); node.id = " << node.id << endl;

		bool_node* lhs_of_cond = node.mother();
		int rhs_of_cond = node.quant;
		bool_node* new_value = node.getNewVal();
		bool_node* old_value = node.getOldVal();

		bool lhs_of_cond_has_one_hot = node_id_to_replacement_nodes.find(lhs_of_cond->id) != node_id_to_replacement_nodes.end();
		bool new_value_has_one_hot = node_id_to_replacement_nodes.find(new_value->id) != node_id_to_replacement_nodes.end();
		bool old_value_has_one_hot = node_id_to_replacement_nodes.find(old_value->id) != node_id_to_replacement_nodes.end();

		if(lhs_of_cond_has_one_hot && new_value_has_one_hot && old_value_has_one_hot)
		{
			if(print) cout << "ENTER 1" << endl;
			vector<bool_node*> lhs_nodes = node_id_to_replacement_nodes[lhs_of_cond->id];
			vector<bool_node*> new_nodes;
			if(rhs_of_cond < lhs_nodes.size())
			{
				bool_node* quant_node = lhs_nodes[rhs_of_cond];

				vector<bool_node*> new_value_nodes = node_id_to_replacement_nodes[new_value->id];
				vector<bool_node*> old_value_nodes = node_id_to_replacement_nodes[old_value->id];
				
				make_vectors_equal_length(new_value_nodes, old_value_nodes);

				int n = new_value_nodes.size();

				for(int i = 0;i<n;i++)
				{
					bool_node* new_node = my_sum(
								my_mult(new_value_nodes[i], quant_node), 
								my_mult(old_value_nodes[i], one_minus_node(quant_node)));
					new_nodes.push_back(new_node);
				}
				assert_one_hot_constraint(new_nodes);
			}
			else
			{
				vector<bool_node*> old_value_nodes = node_id_to_replacement_nodes[old_value->id];
				new_nodes = old_value_nodes;
			}
			assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
			node_id_to_replacement_nodes[node.id] = new_nodes;
		}
		else if(lhs_of_cond_has_one_hot && !new_value_has_one_hot && !old_value_has_one_hot)
		{
			if(print) cout << "ENTER 2" << endl;
			vector<bool_node*> lhs_nodes = node_id_to_replacement_nodes[lhs_of_cond->id];

			if(new_value->getOtype() == OutType::FLOAT && old_value->getOtype() == OutType::FLOAT)
			{
				if(rhs_of_cond < lhs_nodes.size())
				{
					bool_node* quant_node = lhs_nodes[rhs_of_cond];
					rvalue = my_sum(
						my_mult(new_value, quant_node), 
						my_mult(old_value, one_minus_node(quant_node)));
				}
				else
				{
					rvalue = old_value;
				}
				return;
			}
		}

		DagOptim::visit(node);	
	}

	virtual void visit(ARR_W_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;

		if(print) cout << "IN visit( ARR_W_node& node ); node.id = " << node.id << endl;

		bool_node* idx_node = node.mother();
		bool_node* arr_node = node.getOldArr();
		bool_node* new_val = node.getNewVal();

		bool is_idx_node_one_hot = node_id_to_replacement_nodes.find(idx_node->id) != node_id_to_replacement_nodes.end();

		cout << "idx_node " << idx_node->lprint() << endl;
		cout << "arr_node " << arr_node->lprint() << " out type: " << arr_node->getOtype()->str() << endl;
		if(arr_node->getOtype() == OutType::FLOAT_ARR || arr_node->getOtype() == OutType::INT_ARR)
		{
			cout <<"arr_node->nparents() " << arr_node->nparents() << endl;
		}
		cout << "new_val " << new_val->lprint() << endl;

		if(is_idx_node_one_hot)
		{
			vector<bool_node*> idx_nodes = node_id_to_replacement_nodes[idx_node->id];
			if(arr_node->getOtype() == OutType::FLOAT && new_val->getOtype() == OutType::FLOAT)
			{
				if(print) cout << "ENTER 1" << endl;
				vector<bool_node*> new_nodes;
				for(int i = 0;i<idx_nodes.size();i++)
				{
					new_nodes.push_back(my_mult(idx_nodes[i], new_val));
				}
				assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
				node_id_to_replacement_nodes[node.id] = new_nodes;
			}
			else if (arr_node->getOtype() == OutType::FLOAT_ARR && new_val->getOtype() == OutType::FLOAT)
			{
				bool is_arr_node_replaced = node_id_to_replacement_nodes.find(arr_node->id) != node_id_to_replacement_nodes.end();
				if(is_arr_node_replaced)
				{
					if(print) cout << "ENTER 2" << endl;
					vector<bool_node*> arr_nodes = node_id_to_replacement_nodes[arr_node->id];

					make_vectors_equal_length(arr_nodes, idx_nodes);

					int n = idx_nodes.size();

					vector<bool_node*> new_nodes;
					for(int i = 0;i<n;i++)
					{
						new_nodes.push_back(
							my_sum(
									my_mult(one_minus_node(idx_nodes[i]), arr_nodes[i]),
									my_mult(idx_nodes[i], new_val)
								)
							);
					}

					assert(node_id_to_replacement_nodes.find(node.id) == node_id_to_replacement_nodes.end());
					node_id_to_replacement_nodes[node.id] = new_nodes;
				}
			}
		}

		DagOptim::visit(node);	
	}

	virtual void visit(ARR_R_node& node)
	{
		if(node.id == -1) {
			DagOptim::visit(node);
			return;
		}
		bool print = meta_print || node.id != -1;

		if(print) cout << "IN visit( ARR_R_node& node ); node.id = " << node.id << endl;

		bool_node* idx_node = node.mother();
		bool_node* arr_node = node.father();

		bool is_idx_node_one_hot = node_id_to_replacement_nodes.find(idx_node->id) != node_id_to_replacement_nodes.end();
		bool is_arr_node_replaced = node_id_to_replacement_nodes.find(arr_node->id) != node_id_to_replacement_nodes.end();

		if(is_idx_node_one_hot && is_arr_node_replaced)
		{ 
			if(print) cout << "ENTER" << endl;
			vector<bool_node*> idx_nodes = node_id_to_replacement_nodes[idx_node->id];
			vector<bool_node*> arr_nodes = node_id_to_replacement_nodes[arr_node->id];
			vector<bool_node*> dot_product;
			for(int i = 0;i<min(idx_nodes.size(), arr_nodes.size());i++)
			{
				dot_product.push_back(my_mult(idx_nodes[i], arr_nodes[i]));
			}
			rvalue = sum_nodes(dot_product);
			return ;
		}
		DagOptim::visit(node);	
	}
};

#endif  // INTOTOFLOATREWRITEDAG_H_
