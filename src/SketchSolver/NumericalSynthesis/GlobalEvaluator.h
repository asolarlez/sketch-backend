#pragma once
#include <gsl/gsl_vector.h>
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"
#include <glpk.h>

#include <iostream>
#include <fstream>

using namespace std;



class Line { // m(x - x0) + d < 0 (x0 is fixed for a particular evaluator, hence not stored in the line)
public:
	double d;
	gsl_vector* m;
	Line(double _d, gsl_vector* _m): d(_d), m(_m) {}
	~Line(void) {
		delete m;
	}
	
	double getOffset(const gsl_vector* x0) { // m.x0 - d
		double r = 0.0;
		for (int i = 0; i < x0->size; i++) {
			r += gsl_vector_get(x0, i) * gsl_vector_get(m, i);
		}
		return r - d;
	}
	
	double getCoeff(int i) {
		return gsl_vector_get(m, i);
	}
	
	double getDist() {
		double r = 0;
		for (int i = 0; i < m->size; i++) {
			r += gsl_vector_get(m, i) * gsl_vector_get(m, i);
		}
		return d/sqrt(r);
	}
	
	string print() {
		stringstream str;
		str << "<";
		for (int i = 0; i < m->size; i++) {
			str << gsl_vector_get(m, i) << " ";
		}
		str << d << ">";
		return str.str();
	}
};


class Region { // Intersection of lines
public:
	vector<Line*> lines;
	int size() {
		return lines.size();
	}
	Line* get(int i) {
		return lines[i];
	}
	void addLine(Line* l) {
		if (find(lines.begin(), lines.end(), l) == lines.end()) {
			lines.push_back(l);
		}
	}
	
	static Region* intersect(Region* r1, Region* r2) {
		Region* r = new Region();
		for (int i = 0; i < r1->size(); i++) {
			r->addLine(r1->get(i));
		}
		for (int i = 0; i < r2->size(); i++) {
			r->addLine(r2->get(i));
		}
		return r;
	}
	
	static Region* copy(Region* r1) {
		Region* r = new Region();
		for (int i = 0; i < r1->size(); i++) {
			r->addLine(r1->get(i));
		}
		return r;
	}
	
	double getMaxDist() {
		double d = - numeric_limits<double>::max();
		for (int i = 0; i < lines.size(); i++) {
			if (lines[i]->getDist() > d) {
				d = lines[i]->getDist();
			}
		}
		return d;
	}
	
	gsl_vector* getMaxGrad() {
		double d = - numeric_limits<double>::max();
		gsl_vector* g;
		for (int i = 0; i < lines.size(); i++) {
			if (lines[i]->d > d) {
				d = lines[i]->d;
				g = lines[i]->m;
			}
		}
		return g;

	}
	
	~Region(void) {
		for (int i = 0; i < lines.size(); i++) {
			delete lines[i];
		}
	}
	
	string print() {
		stringstream str;
		str << "{";
		for (int i = 0; i < lines.size(); i++) {
			str << lines[i]->print() << " ";
		}
		str << "}";
		return str.str();
	}
	
};

class Value {
public:
	double val;
	gsl_vector* grad;
	Value(double v, gsl_vector* g): val(v), grad(g) {}
	~Value(void) {
		delete grad;
	}
	
	double getOffset(const gsl_vector* x0) { // d - m.x0
		double r = 0.0;
		for (int i = 0; i < x0->size; i++) {
			r += gsl_vector_get(x0, i) * gsl_vector_get(grad, i);
		}
		return val - r;
	}
	
	double getCoeff(int i) {
		return gsl_vector_get(grad, i);
	}
	
	static Value* add(Value* v1, Value* v2);
	static Value* mult(Value* v1, Value* v2);
	static Value* div(Value* v1, Value* v2);
	static Value* neg(Value* v1);
	static Value* copy(Value* v1);
	static Value* arctan(Value* v1);
	static Value* tan(Value* v1);
	static Value* cos(Value* v1);
	static Value* sin(Value* v1);
	static Value* sqrt(Value* v1);
	static Value* square(Value* v1);
	
	string print() {
		stringstream str;
		str << "<";
		for (int i = 0; i < grad->size; i++) {
			str << gsl_vector_get(grad, i) << " ";
		}
		str << val << ">";
		return str.str();
	}
};

class RegionValuePair {
public:
	Region* r;
	Value* v;
	RegionValuePair(Region* _r, Value* _v): r(_r), v(_v) {}
	
	string print() {
		stringstream str;
		str << r->print() << " -> " << v->print();
		return str.str();
	}
};

typedef vector<RegionValuePair*> ValuesList;


class LP {
	glp_prob* lp;
	int ia[1+1000], ja[1+1000];
	double ar[1+1000];
	float LOWBND = -32.0; // TODO: don;t hardcode
	float HIGHBND = 32.0;
	float PRECISION = 1e-5;
	
public:
	double optimize(RegionValuePair* rv, const gsl_vector* x0) {
		lp = glp_create_prob();
		glp_set_obj_dir(lp, GLP_MIN);
		int numrows = rv->r->size();
		if (numrows > 0) {
			glp_add_rows(lp, numrows);
			for (int i = 0; i < numrows; i++) {
				glp_set_row_name(lp, (i+1), "r" + (i+1));
				double c = rv->r->lines[i]->getOffset(x0);
				glp_set_row_bnds(lp, (i+1), GLP_UP, 0.0, c - PRECISION);
			}
		}
		int numcols = x0->size;
		glp_add_cols(lp, numcols);
		for (int i = 0; i < numcols; i++) {
			glp_set_col_name(lp, (i+1), "x" + (i+1));
			glp_set_col_bnds(lp, (i+1), GLP_DB, LOWBND, HIGHBND);
			glp_set_obj_coef(lp, (i+1), rv->v->getCoeff(i));
		}
		int counter = 1;
		for (int i = 0; i < numrows; i++) {
			for (int j = 0; j < numcols; j++) {
				ia[counter] = (i+1);
				ja[counter] = (j+1);
				ar[counter] = rv->r->lines[i]->getCoeff(j);
				counter++;
			}
		}
		Assert(counter-1 <= 1000, "Out of bounds");
		
		glp_load_matrix(lp, counter-1, ia, ja, ar);
		glp_simplex(lp, NULL);
		int status = glp_get_prim_stat(lp);
		cout << "status: " << status << endl;
		if (status == GLP_FEAS) {
			double z = glp_get_obj_val(lp);
			cout << "z = " << z;
			for (int i = 0; i < numcols; i++) {
				cout << " x" << i << " = " << glp_get_col_prim(lp, (i+1));
			}
			glp_delete_prob(lp);
			return z + rv->v->getOffset(x0);
		} else {
			return 1000;
		}
		
	}
	
	bool check(Region* r, const gsl_vector* x0) {
		int numrows = r->size();
		if (numrows == 0) return true;
		
		lp = glp_create_prob();
		glp_set_obj_dir(lp, GLP_MIN);
		glp_add_rows(lp, numrows);
		for (int i = 0; i < numrows; i++) {
			stringstream str;
			str << "r" << (i+1);
			glp_set_row_name(lp, (i+1), str.str().c_str());
			double c = r->lines[i]->getOffset(x0);
			glp_set_row_bnds(lp, (i+1), GLP_UP, 0.0, c-PRECISION);
		}
		int numcols = x0->size;
		glp_add_cols(lp, numcols);
		for (int i = 0; i < numcols; i++) {
			stringstream str;
			str << "x" << (i+1);
			glp_set_col_name(lp, (i+1), str.str().c_str());
			glp_set_col_bnds(lp, (i+1), GLP_DB, LOWBND, HIGHBND);
			glp_set_obj_coef(lp, (i+1), 0.0);
		}
		int counter = 1;
		for (int i = 0; i < numrows; i++) {
			for (int j = 0; j < numcols; j++) {
				ia[counter] = (i+1);
				ja[counter] = (j+1);
				ar[counter] = r->lines[i]->getCoeff(j);
				counter++;
			}
		}
		
		glp_load_matrix(lp, counter-1, ia, ja, ar);
		glp_smcp param;
		glp_init_smcp(&param);
		param.msg_lev = GLP_MSG_ERR;
		glp_simplex(lp, &param);
		int status = glp_get_prim_stat(lp);
		glp_delete_prob(lp);
		if (status == GLP_FEAS) {
			return true;
		} else {
			return false;
		}
	}
	
};

class GlobalEvaluator: NodeVisitor
{
	FloatManager& floats;
	BooleanDAG& bdag;
  map<string, int> floatCtrls; // Maps float ctrl names to indices with grad vectors
  int nctrls; // number of float ctrls
	VarStore* ctrls; // Maps ctrl names to values (int abstraction for floating point values)
  vector<ValuesList> values; // Keeps track of values for each node for different regions
	map<int, int> inputValues; // Maps node id to values set by the SAT solver
	double error = 0.0;
	gsl_vector* errorGrad;
	
	int DEFAULT_INP = -1;
	static gsl_vector* tmp;
	LP* lp;
	
	int MAX_REGIONS = 16;
	int ctr = 0; 
public:
  GlobalEvaluator(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p);
  ~GlobalEvaluator(void);
  
  virtual void visit( SRC_node& node );
  virtual void visit( DST_node& node );
  virtual void visit( CTRL_node& node );
  virtual void visit( PLUS_node& node );
  virtual void visit( TIMES_node& node );
  virtual void visit( ARRACC_node& node );
  virtual void visit( DIV_node& node );
  virtual void visit( MOD_node& node );
  virtual void visit( NEG_node& node );
  virtual void visit( CONST_node& node );
  virtual void visit( LT_node& node );
  virtual void visit( EQ_node& node );
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( NOT_node& node );
  virtual void visit( ARRASS_node& node );
  virtual void visit( UFUN_node& node );
  virtual void visit( TUPLE_R_node& node );
	virtual void visit( ASSERT_node& node );
  
  double run(VarStore& ctrls_p, map<int, int>& inputValues_p, gsl_vector* errorGrad_p, const gsl_vector* x);
	void truncate(ValuesList& vl, const gsl_vector* x0, int numRegions);
	void truncate(vector<Region*>& vl, const gsl_vector* x0, int numRegions);
	void truncateUnfeasibleRegions(ValuesList& vl, const gsl_vector* x0);
	void truncateUnfeasibleRegions(vector<Region*>& vl, const gsl_vector* x0);
	void truncateFarRegions(ValuesList& vl, const gsl_vector* x0, int numRegions);
	void truncateFarRegions(vector<Region*>& vl, const gsl_vector* x0, int numRegions);
	
	void setvalues(bool_node& bn, ValuesList& v) {
		values[bn.id] = v;
	}
	
	ValuesList& getvalues(bool_node& bn) {
		return values[bn.id];
	}
	
	ValuesList& getvalues(bool_node* bn) {
		return getvalues(*bn);
	}
	
	void print() {
    for (int i = 0; i < bdag.size(); i++) {
      cout << bdag[i]->lprint() << endl;
		}
  }
	
	bool isFloat(bool_node& bn) {
		return (bn.getOtype() == OutType::FLOAT);
	}
	
	bool isFloat(bool_node* bn) {
		return (bn->getOtype() == OutType::FLOAT);
	}
	
	int getInputValue(bool_node& bn) {
		if (inputValues.find(bn.id) != inputValues.end()) {
			int val = inputValues[bn.id];
			Assert(val == 0 || val == 1, "NYI: Integer values");
			return val;
		} else {
			return DEFAULT_INP;
		}
	}
	
	int getInputValue(bool_node* bn) {
		return getInputValue(*bn);
	}
	
	static gsl_vector* default_grad(int nctrls) {
		gsl_vector* g = gsl_vector_alloc(nctrls);
		for (int i = 0; i < nctrls; i++) {
			gsl_vector_set(g, i, 0);
		}
		return g;
	}
	
	static gsl_vector* add_grad(gsl_vector* g1, gsl_vector* g2);
	static gsl_vector* mult_grad(double v1, double v2, gsl_vector* g1, gsl_vector* g2);
	static gsl_vector* div_grad(double v1, double v2, gsl_vector* g1, gsl_vector* g2);
	static gsl_vector* neg_grad(gsl_vector* g1);
	static gsl_vector* copy_grad(gsl_vector* g1);
	static gsl_vector* sub_grad(gsl_vector* g1, gsl_vector* g2);
	static gsl_vector* arctan_grad(double v1, gsl_vector* g1);
	static gsl_vector* tan_grad(double v1, gsl_vector* g1);
	static gsl_vector* cos_grad(double v1, gsl_vector* g1);
	static gsl_vector* sin_grad(double v1, gsl_vector* g1);
	static gsl_vector* sqrt_grad(double v1, gsl_vector* g1);
	static gsl_vector* square_grad(double v1, gsl_vector* g1);
	
};

