#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"

#include <iostream>

using namespace std;

/*Started creating a class to represent tuples */
class cptuple{
	int bnd;
public:
    
	int *vv;
    ~cptuple(){
		if(vv != NULL){
			delete[] vv;
		}
	}
	cptuple(int n):vv(new int[n]){
		memset(vv, 0, n*sizeof(int));
        bnd = n;
    }
    int size() const{
		return bnd;
	}
   


};


const int UNSET = -22;
class cpvec{
	int bnd;
	cpvec* parent;
	//Entry 0 corresponds to the true value stored here.
	//Entry's 1 and 2 are cache locations to avoid having to traverse too much.
	int val[3];
	int idx[3];
	int flip;
public:
    class LocalVectorTrait
    {
    public:
        virtual int size() const
        {
            assert(false);
            return 0;
        }
        virtual int operator [] (int idx) const = 0;
        virtual void set_int_at_index(int idx, int val) = 0;
    };

    class LocalVectorTrait_rep_BitVector: public LocalVectorTrait
    {
        objP::BitMetaVector_rep_CHOOSE* const mutable_vv = nullptr;
        const objP::BitMetaVector_rep_CHOOSE* const vv = nullptr;
    public:
        LocalVectorTrait_rep_BitVector(int sz, int init_val):
        mutable_vv(new objP::BitMetaVector_rep_CHOOSE(sz, init_val)),
        vv(mutable_vv) {}
        LocalVectorTrait_rep_BitVector(const objP::BitMetaVector_rep_CHOOSE* _vv): vv(_vv) {}

        int size() const override
        {
            return vv->get_num_vectors();
        }

        int operator [] (int idx) const override {
            return vv->get_vector_as_int(idx);
        }

        void set_int_at_index(int idx, int val) override
        {
            mutable_vv->set_vector_from_int(idx, val);
        }
    };

    LocalVectorTrait* const mutable_vv = nullptr;
    const LocalVectorTrait* const vv = nullptr;
	~cpvec(){}

	void update(cpvec* pp, int ii, int v){
		Assert(vv==NULL, "qwejh;u88");
		parent = pp;
		if(parent==NULL){
			bnd = max(0, ii+1);
		}else{
			if(parent->idx[0] == UNSET && parent->vv == NULL && parent->parent != NULL){
				parent = parent->parent;
			}
			bnd = max(parent->bnd, ii+1);
		}		
		if(ii<0){
			idx[0] = UNSET;
		}else{
			idx[0] = ii;			
			val[0] = v;
		}
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}

	cpvec(cpvec* pp, int ii, int v):vv(NULL){		
		update(pp, ii, v);
	}
	cpvec(int sz):
    // NOTE xzL: set uninitialized value to be 0
    mutable_vv(new LocalVectorTrait_rep_BitVector(sz, 0)), vv(mutable_vv) {
        assert(vv == mutable_vv);
		bnd = sz;		
		parent = NULL;
		idx[0] = UNSET;
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}
	cpvec(int sz, const objP* op): vv(new LocalVectorTrait_rep_BitVector(op->as_chosen_rep_pointer())){
		// NOTE xzL: set uninitialized value to be 0
//		memset(vv, 0, sz*sizeof(int));
        assert(vv->size() == sz);
		bnd = sz;
//        op->populate_vec(vv, sz);
		parent = NULL;
		idx[0] = UNSET;
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}
	int size() const{
		return bnd;
	}
	bool lget(int ii, int& rv){
		if(vv != NULL){ 
			if(ii>=bnd){ return false; }		
			rv = (*vv)[ii];
			return true;
		}

		int b0 = idx[0]==ii;
		int b1 = idx[1]==ii;
		int b2 = idx[2]==ii;
		if(b0){ rv= val[0]; return true;}
		if(b1){ rv= val[1]; return true;}
		if(b2){ rv= val[2]; return true;}
		return false;
	}
	int get(int ii, int deflt){
		if(ii>=bnd){
			return deflt;
		}
		Assert(ii < bnd, "Out of bounds error in solver ;qek;kl");		
		int rv=-1;
		cpvec* tt = this;
		while(tt != NULL && !tt->lget(ii, rv)){
			tt = tt->parent;
		}
		if(tt == NULL){ 
			return deflt; 
		}
		if(tt == this){ return rv; }
		//If the value was stored far away, we keep a copy closer.
		idx[1+flip] = ii;
		val[1+flip] = rv;
		flip = 1-flip;
		return rv;
	}
	void print(ostream & os) {
		// recursively print the structure of a cpvec
		os << bnd << "[";
		for (int i=0; i<bnd; i++) {
			os << get(i, -1) << " ";
		}
		for (int i=0; i<3; i++) {
			os << idx[i] << "," << val[i] << " ";
		}
		if (parent) {
			parent->print(os);
		}
		os << "]";
	}
};


class NodeEvaluator :
	public NodeVisitor
{
private:
    const VarStore* inputs = nullptr;
    bool src_name_id_linking_done = false;
    bool_node::Type node_type = bool_node::NO_TYPE;
    int input_size = 0;
protected:
    void set_inputs(const VarStore* _inputs, bool assert_invariant = true) {
        inputs = _inputs;

        if(_inputs->size() == 0)
        {
            src_name_id_linking_done = true;
            return;
        }

        assert(_inputs->size() != 0);
        input_size = _inputs->size();
        if(node_type == bool_node::NO_TYPE) {
            node_type = _inputs->getObjConst(0).get_type();
            for (int i = 1; i < _inputs->size(); i++) {
                assert(_inputs->getObjConst(i).get_type() == node_type);
            }
        } else{
            assert(src_name_id_linking_done);
        }

        if(node_type == bool_node::SRC) {
            if (!src_name_id_linking_done) {
                for (auto it: bdag.getNodesByType(bool_node::SRC)) {
                    SRC_node &node = *(SRC_node *) it;
                    assert(node.current_node_evaluator == nullptr);
                    node.current_node_evaluator = this;
                    node.local_id_in_inputs = inputs->getId(node.get_name());
                }
                src_name_id_linking_done = true;
            } else if (assert_invariant) {
                for (auto it: bdag.getNodesByType(bool_node::SRC)) {
                    SRC_node &node = *(SRC_node *) it;
                    assert(node.current_node_evaluator == this);
                    assert(inputs->getObjConst(node.local_id_in_inputs).get_name() == node.get_name());
                }
            }
        }
        else
        {
            assert(node_type == bool_node::CTRL);
            if (!src_name_id_linking_done) {
                for (auto it: bdag.getNodesByType(bool_node::CTRL)) {
                    CTRL_node &node = *(CTRL_node *) it;
                    assert(node.current_node_evaluator == nullptr);
                    node.current_node_evaluator = this;
                    if(inputs->contains(node.get_name())) {
                        node.local_id_in_inputs = inputs->getId(node.get_name());
                    } else {
                        node.local_id_in_inputs = inputs->get_id_from_original_name(node.get_original_name());
                    }
                }
                src_name_id_linking_done = true;
            } else if (assert_invariant) {
                int ctrl_node_id = 0;
                auto nodes_by_type = bdag.getNodesByType(bool_node::CTRL);
                for (auto it: nodes_by_type) {
                    CTRL_node &node = *(CTRL_node *) it;
                    assert(node.current_node_evaluator == this);
                    assert(node.local_id_in_inputs >= 0);
                    auto local_obj = inputs->getObjConst(node.local_id_in_inputs);
//                    assert(local_obj.get_original_name() == node.get_original_name());
                    assert(local_obj.get_is_array() == node.isArr());
                    assert(local_obj.element_size() == node.get_nbits());
                    assert(local_obj.arrSize() == node.getArrSz()
                            || (node.getArrSz() == -1 &&
                                    !local_obj.get_is_array() &&
                                    local_obj.arrSize() == 1));
                    ctrl_node_id++;
                }
            }
        }
    }
	float epsilon;
	FloatManager& floats;
	map<string, vector<pair<int, vector<int> > > > funargs;
	map<UFUN_node*, NodeEvaluator> recursives;
	const BooleanDAG& bdag;
	vector<int> values;
	vector<cpvec*> vecvalues;
    vector<cptuple*> tuplevalues;
	vector<bool> changes;
	vector<bool> isset;
	bool failedAssert;
	bool failedHAssert;
	bool trackChange;
	int i(bool_node& bn){
        assert(isset[bn.id]);
		return values[bn.id];
	}

public:
    bool get_isset(bool_node& bn)
    {
        return isset[bn.id];
    }
protected:

	

	bool checkKnownFun(UFUN_node& node);
	void builtinRetVal(UFUN_node& node, int idxval);

	bool b(bool_node& bn){
		return values[bn.id] == 1;
	}

	bool argcomp(bool_node::parent_iter parents_beg, bool_node::parent_iter parents_end, vector<int>& v1, vector<int>& v2);
	void setbn(bool_node& bn, int i){
		if(trackChange){
			int id = bn.id;
			int& t = values[id];
			if(isset[id]){
				changes[id] = changes[id] || (t!=i);
			}else{
				isset[id] = true;
			}
			t = i;
		}else{
			values[bn.id] = i;
			isset[bn.id] = true;
		}
	}

	void setbn(bool_node& bn, bool c){
		setbn(bn, c ? 1 : 0);
	}
public:
	NodeEvaluator(const BooleanDAG &bdag_p, FloatManager &_floats);
	~NodeEvaluator(void);
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ASSERT_node &node);		

	virtual void visit( ARR_R_node &node);
	virtual void visit( ARR_W_node &node);
	virtual void visit( ARR_CREATE_node &node);
    virtual void visit( TUPLE_CREATE_node &node);
    virtual void visit( TUPLE_R_node &node);

	bool run(const VarStore &inputs_p, bool reset_src_to_input_id = true, bool assert_invariant = true);
    bool get_src_name_id_linking_done()
    {
        return src_name_id_linking_done;
    }
    void reset_src_to_input_id()
    {
        if(node_type == bool_node::SRC) {
            assert(src_name_id_linking_done);
            for (auto it: bdag.getNodesByType(bool_node::SRC)) {
                SRC_node &node = *(SRC_node *) it;
                assert(node.current_node_evaluator == this);
                node.current_node_evaluator = nullptr;
                node.local_id_in_inputs = -1;
            }
            src_name_id_linking_done = false;
            input_size = 0;
            node_type = bool_node::NO_TYPE;
        }
        else if(node_type == bool_node::CTRL) {
            assert(src_name_id_linking_done);
            for (auto it: bdag.getNodesByType(bool_node::CTRL)) {
                CTRL_node &node = *(CTRL_node *) it;
                assert(node.current_node_evaluator == this);
                node.current_node_evaluator = nullptr;
                node.local_id_in_inputs = -1;
            }
            src_name_id_linking_done = false;
            input_size = 0;
            node_type = bool_node::NO_TYPE;
        }
        else
        {
            assert(input_size == 0);
        }
    }
	void display(ostream& out);
	// get unchanged node, but only starting from start
	int scoreNodes(int start = 0);
	void trackChanges(){
		trackChange = true;
	}
	void printNodeValue(int i);

	int getValue(bool_node& bn){
		return i(bn);
	}
	int getValue(bool_node* bn){
		return i(*bn);
	}
  
  vector<int> getTuple(int i) {
    cptuple* tup = tuplevalues[i];
    vector<int> res;
    for (int k = 0; k < tup->size(); k++) {
      res.push_back(tup->vv[k]);
    }
    return res;
  }
};
