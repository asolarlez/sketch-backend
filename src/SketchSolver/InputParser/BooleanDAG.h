// BooleanDAG.h: interface for the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)
#define AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include <string>
#include <list>
#include <map>
#include <set>

#include "CegisCApi.h"
#include "BooleanNodes.h"
#include "BasicError.h"
#include "timerclass.h"
#include "NodeVisitor.h"
#include "Dllist.h"


#ifdef CONST
#undef CONST
#endif

using namespace std;
#define SCHECKMEM

inline bool comp_id(bool_node* n1, bool_node* n2){
  unsigned int n1id = (unsigned int) n1->id;
  /*
  if(n1id == -1){
    return false;
  } */
  unsigned int n2id = (unsigned int) n2->id;
  /*
  if(n2id == -1){ //& n1->id != -1
    return n1id!= n2id;
  } */
  return n1id < n2id;
}


inline bool comp_layer(bool_node* n1, bool_node* n2){
  return n1->layer < n2->layer ;
}

//extern timerclass TTMMPP;

class FloatManager;
class File;

string zeros(int n);

class BooleanDAG  
{
private:
    long long dag_id;
    static long long global_boolean_dag_id;
#ifdef SCHECKMEM
    class AllDAGs : private set<BooleanDAG*> {
        size_t global_max_num_allocated_dags = 0;
        int num_dags = 0;
    public:
        size_t get_max() {
            return global_max_num_allocated_dags;
        }

        size_t get_num() {
            return num_dags;
        }

        void insert(BooleanDAG* new_dag) {
            set<BooleanDAG*>::insert(new_dag);
            num_dags++;
            global_max_num_allocated_dags = max(global_max_num_allocated_dags, size());
        }

        size_t size() const {
            return set<BooleanDAG*>::size();
        }

        void erase(BooleanDAG* to_erase) {
            set<BooleanDAG*>::erase(to_erase);
        }
    };
	static AllDAGs allocated;
public:
    static AllDAGs& get_allocated()
    {
        return allocated;
    }
#endif
    static string create_suffix(bool modify_name, long long int dag_id);
    static long long get_global_boolean_dag_id()
    {
        return global_boolean_dag_id;
    }
    long long get_dag_id() const
    {
        return dag_id;
    }
private:
    int dag_id_from_the_user = -1;

  int n_inputs;
  int n_outputs;
  int n_controls;
  int offset;
  int intSize;
  bool ownsNodes;
  bool useSymbolicSolver;

  vector<bool_node*> nodes;
  vector<int> layer_sizes;
  map<string, INTER_node*> named_nodes;

  map<bool_node::Type, vector<bool_node*> > nodesByType;
  

  const string name;
  const bool is_clone;
  bool is_sorted; //The sorted property implies that everyone comes after their parents
  bool is_layered;  //The layered property implies that nodes are sorted by layer. is_layered implies is_sorted.
  
  void compute_layer_sizes();

  void shareparent_remove(int i);
  INTER_node* create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type);

public:
    bool is_original() const {
        return !is_clone;
    }
	bool isModel;
    Dllist assertions;
	typedef vector<bool_node*>::iterator iterator;
	typedef vector<bool_node*>::reverse_iterator reverse_iterator;
  ////////////////////////////////////////////////////////////////////////
  //Mutators for graph creation.
  ////////////////////////////////////////////////////////////////////////
  INTER_node* create_inputs(int n, OutType* type, const string& gen_name=string("INPUT"), int arrSz=-1, int tupDepth = -1);
  INTER_node* create_controls(int n, const string& gen_name=string("CONTROL"), bool toMinimize = false, bool angelic = false, bool spConcretize = false, int max = -1, bool isFloat = false, bool isSpeical = false);

    void growInputIntSizes();

    vector<CTRL_node*> get_CTRL_nodes() const;

    void disownNodes(){
        ownsNodes=false;
    }

  int getIntSize() const{
	return intSize;
  }

  /**
	Creates an N-bit output named gen_name, connected to internal node nodeToOutput.
  */
  INTER_node* create_outputs(int n, bool_node* nodeToOutput, const string& gen_name=string("OUTPUT"));
  INTER_node* create_outputs(int n, const string& gen_name=string("OUTPUT"));

  bool_node* new_node(bool_node* mother, bool_node* father, bool_node::Type t);
	
  //void nameNode(bool_node* node){ named_nodes[node->name] = node; }

  bool check_ctrl_node_source_dag_naming_invariant() const
  {
      for(auto it : getNodesByType(bool_node::CTRL)) {
          assert(((CTRL_node*)it)->get_source_dag_name() == get_name());
      }
      return true;
  }

  ////////////////////////////////////////////////////////////////////////
  //Mutators for graph cleanup and refactoring.
  ////////////////////////////////////////////////////////////////////////
  void removeNullNodes();
  void replace(int original, bool_node* replacement);
  void remove(int i);
  void addNewNode(bool_node* v);
  void addNewNodes(vector<bool_node*>& v);
  void clearBackPointers();
  void change_father(const string& father, const string& son);
  void change_mother(const string& father, const string& son);
  void layer_graph();  
  void relabel() const;
  void cleanup(); //Sorts and cleans up the graph.
  void cleanup_children();
  void cleanUnshared();
  void clear();
  void rename(const string& oldname,  const string& newname);
  void resetBackPointers();
  int get_n_inputs()const{ return n_inputs; };
  int get_n_outputs()const{ return n_outputs; };
  int get_n_controls()const{ return n_controls; };
  int get_n_layers()const{ return layer_sizes.size(); };
  void setOffset(int ofs){ offset = ofs; }
  void printSlice(bool_node* node, ostream& out)const;
  /*Both of these functions destroy their argument*/
  virtual void makeMiter(BooleanDAG* bdag);  
  virtual void andDag(BooleanDAG* bdag);
  ////////////////////////////////////////////////////////////////////////
  //Field Access methods.	
  ////////////////////////////////////////////////////////////////////////
  bool_node* const & operator[](int idx)const{ return nodes[idx]; };
  bool_node*& operator[](int idx){ return nodes[idx]; };
  int size()const {return nodes.size();}
  int get_lsize(int layer){ return layer_sizes[layer];};
  
  bool_node* get_node(const string& name);  
  bool_node* unchecked_get_node(const string& name);
  auto begin() const { return nodes.begin(); }
  auto end() const { return nodes.end(); }

    auto rbegin() const { return nodes.rbegin(); }
    auto rend() const { return nodes.rend(); }


  inline bool checkNodePosition(bool_node* bn) const {
	  int bid = bn->id;
	return bid < this->size() && bid >= 0 && bn == nodes[bid];
  }

  vector<bool> evaluate_inputs(const File* file, FloatManager& floats, int repeats = 1) const;
    int get_passing_input_id(const File* file, FloatManager& floats);


  const vector<bool_node*>& getNodesByType(bool_node::Type t) const;
  vector<bool_node*>& getNodesByType_NonConst(bool_node::Type t);


  
  bool has_name(const string& s){
  	return named_nodes.find(s) != named_nodes.end();
  }
  
  const string& get_name()const{
	return name;
  }
  
  void registerOutputs();
  
  void sliceH(bool_node* n, BooleanDAG* bd);
  BooleanDAG* slice(int i, ASSERT_node* out){
		return slice(nodes.end(), nodes.end(), i, out);
	}

  
  
  template<typename forward_iter>
  BooleanDAG* slice(forward_iter begin, forward_iter end, int i, ASSERT_node* out){
	BooleanDAG* bd = slice(begin, end, i);
	if(out->mother() != NULL){
		out->mother()->mother() = nodes[i];
		// BUGFIX xzl: need to add to parents, otherwise the simulate in CEGISSolver will fail sometimes because out->mother()->mother() is an SRC_node whose children is empty, causing NodesToSolver to fail.
		out->mother()->addToParents();
		if(out->mother()->hasFather()){
			bd->addNewNode(out->mother()->father());
		}
		bd->addNewNode(out->mother());
	}else{
		out->mother() = nodes[i];
	}
	// BUGFIX xzl: for safety, we link parents here too
	out->addToParents();
	bd->addNewNode(out);
	return bd;
}

  


// interesting are the interesting nodes (stored in [begin, end)) in this dag!
// and they must be sorted according to the normal order!
  template<typename forward_iter>
  BooleanDAG* slice(forward_iter begin, forward_iter end, int i){
		for(auto it = nodes.begin(); it != nodes.end(); ++it){
			(*it)->flag = 0;
		}
		BooleanDAG* bd = new BooleanDAG(this->name);
		bd->intSize = intSize;
		bd->ownsNodes = false;
		for (; begin!=end; ++begin) {
			//cout << "slicing on " << (*begin)->lprint() << endl;
			bd->sliceH(*begin, bd);
		}
		if (i>=0) {
			bd->sliceH(nodes[i], bd);
		}
		return bd;
	}

  void repOK();

  BooleanDAG* clone(const string& explict_name = "", const bool rename_holes = false, const map<string, string>* = nullptr) const;
  void clone_nodes(vector<bool_node*>& nstore, Dllist* dl=nullptr) const;

  void print(ostream& out) const;
  void lprint(ostream& out) const ;
  void mrprint(ostream& out, bool print_only_nodes = false) const;
  void smtlinprint(ostream& out, int &nbits);
  void smt_exists_print(ostream &out);
  void print_wrapper()const;
  void lprint_wrapper();
  void print_wrapper(const char* fileName)const;
  void lprint_wrapper(const char* fileName);
  
  void setUseSymbolic() {
	  useSymbolicSolver = true;
  }

  bool useSymbolic() const {
	  return useSymbolicSolver;
  }

   BooleanDAG(const string& name_="anon", bool isModel_=false, const string& explicit_name = "", bool is_clone = false);

	virtual ~BooleanDAG();

private:
    ASSERT_node *failed_assert = nullptr;
public:
    void set_failed_assert(ASSERT_node *_failed_assert)
    {
        failed_assert = _failed_assert;
    }
    const ASSERT_node* get_failed_assert() const
    {
        return failed_assert;
    }

    void replace_label_with_another(const string replace_this, const string with_this);

    bool get_is_clone();

    vector<string> get_ufun_names() const;

    map<string, string> get_hole_assignment_map();

    void set_dag_id_from_the_user(int dag_id_from_the_user);

    int get_dag_id_from_the_user();
};



#endif // !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)

