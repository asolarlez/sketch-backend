#include "RangeDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>

double pi = 3.1415926535897;

RangeDiff::RangeDiff(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p):NodeEvaluator(functionMap_p, bdag_p, _floats), floatCtrls(floatCtrls_p) {
  lgrads.resize(bdag.size(), NULL);
  hgrads.resize(bdag.size(), NULL);
  ranges.resize(bdag.size());
  nctrls = floatCtrls_p.size();
  for (int i = 0; i < hgrads.size(); i++) {
    lgrads[i] = gsl_vector_alloc(nctrls);
    hgrads[i] = gsl_vector_alloc(nctrls);
  }
  tmp = gsl_vector_alloc(nctrls);
}
RangeDiff::~RangeDiff(void) {
  for (int i = 0; i < lgrads.size(); i++) {
    if (lgrads[i] != NULL) {
      delete lgrads[i];
    }
    if (hgrads[i] != NULL) {
      delete hgrads[i];
    }
  }
  delete tmp;
}

void RangeDiff::visit( SRC_node& node ) { //TODO: deal with array src nodes
  //cout << "Visiting SRC node" << endl;
  string name = node.get_name();
  if (inputs->contains(name)) {
    setrange(node, (*inputs)[name]);
  }
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  for (int i = 0; i < nctrls; i++) {
    gsl_vector_set(l, i, 0.0);
    gsl_vector_set(h, i, 0.0);
  }
}
void RangeDiff::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
  NodeEvaluator::visit(node);
}

void RangeDiff::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
  int v = (*inputs)[node.get_name()];
  setrange(node, v);
  int idx = - 1;
  string name = node.get_name();
  if (floatCtrls.find(name) != floatCtrls.end())
    idx = floatCtrls[name];
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  for (int i = 0; i < nctrls; i++) {
    if (i == idx) {
      gsl_vector_set(l, i, 1.0);
      gsl_vector_set(h, i, 1.0);
    } else {
      gsl_vector_set(l, i, 0.0);
      gsl_vector_set(h, i, 0.0);
    }
  }
}
void RangeDiff::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
  if (node.getOtype() == OutType::FLOAT) {
    pair<int,int> mrange = r(node.mother);
    float mlval = floats.getFloat(mrange.first);
    float mhval = floats.getFloat(mrange.second);
    pair<int,int> frange = r(node.father);
    float flval = floats.getFloat(frange.first);
    float fhval = floats.getFloat(frange.second);
    float lval = mlval + flval;
    float hval = mhval + fhval;
    if (!isfinite(lval)) {
      lval = lval > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    if (!isfinite(hval)) {
      hval = hval > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    int lidx = floats.getIdx(lval);
    int hidx = floats.getIdx(hval);
    setrange(node, lidx, hidx);
  } else {
    pair<int,int> mrange = r(node.mother);
    pair<int,int> frange = r(node.father);
    setrange(node, mrange.first + frange.first, mrange.second + frange.second);
  }
  
  gsl_vector* mlgrads = lgrads[node.mother->id];
  gsl_vector* flgrads = lgrads[node.father->id];
  gsl_vector* mhgrads = hgrads[node.mother->id];
  gsl_vector* fhgrads = hgrads[node.father->id];
  gsl_vector* l = lgrads[node.id];
  gsl_vector_memcpy(l, mlgrads);
  gsl_vector_add(l, flgrads);
  gsl_vector* h = hgrads[node.id];
  gsl_vector_memcpy(h, mhgrads);
  gsl_vector_add(h, fhgrads);
}


void RangeDiff::findMinMax(bool_node& node, vector<float>& vals, vector<gsl_vector*>& grads) {
	gsl_vector* l = lgrads[node.id];
	gsl_vector* h = hgrads[node.id];
	
	float minval = numeric_limits<float>::max();
	float maxval = -numeric_limits<float>::max();
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxval) {
			maxval = vals[i];
		}
		if (vals[i] < minval) {
			minval = vals[i];
		}
	}
	//cout << "Values: " << minval << " " << maxval << endl;
	bool isFloat = node.getOtype() == OutType::FLOAT;
	if (isFloat) {
		setrange(node, floats.getIdx(minval), floats.getIdx(maxval));
	} else {
		setrange(node, int(minval), int(maxval));
	}
	
	float alpha = 1.0;
	float explval = 0.0;
	float exphval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		explval += exp(-alpha * (vals[i] - minval));
		exphval += exp(alpha * (vals[i] - maxval));
	}
	//cout << "ExpVals: " << explval << " " << exphval << endl;
	for (int i = 0; i < vals.size(); i++) {
		gsl_vector_memcpy(tmp, grads[i]);
		gsl_vector_scale(tmp, exp(-alpha*(vals[i] - minval)));
		if ( i == 0)
			gsl_vector_memcpy(l, tmp);
		else
			gsl_vector_add(l, tmp);
		//cout << "L ";
		//for (int k = 0; k < l->size; k++) {
		//	cout << gsl_vector_get(l,k) << ",";
		//}
		//cout << endl;
		gsl_vector_memcpy(tmp, grads[i]);
		gsl_vector_scale(tmp, exp(alpha*(vals[i] - maxval)));
		if ( i == 0)
			gsl_vector_memcpy(h, tmp);
		else
			gsl_vector_add(h, tmp);
	}
	gsl_vector_scale(l, 1.0/explval);
	gsl_vector_scale(h, 1.0/exphval);
}

void RangeDiff::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
  NodeEvaluator::visit(node);
  //Assert(false, "NYI");
  
  gsl_vector* mlgrads = lgrads[node.mother->id];
  gsl_vector* flgrads = lgrads[node.father->id];
  gsl_vector* mhgrads = hgrads[node.mother->id];
  gsl_vector* fhgrads = hgrads[node.father->id];
	
  vector<float> vals;
  vector<gsl_vector*> grads;
	
  bool isFloat = node.getOtype() == OutType::FLOAT;
	bool isSquare = node.mother == node.father;
  if (isFloat) {
    pair<int,int> mrange = r(node.mother);
    float mlval = floats.getFloat(mrange.first);
    float mhval = floats.getFloat(mrange.second);
    pair<int,int> frange = r(node.father);
    float flval = floats.getFloat(frange.first);
    float fhval = floats.getFloat(frange.second);
    float v1 = mlval * flval;
    float v2 = mlval * fhval;
    float v3 = mhval * flval;
    float v4 = mhval * fhval;
		
    if (!isfinite(v1)) {
      v1 = v1 > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    if (!isfinite(v2)) {
      v2 = v2 > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    if (!isfinite(v3)) {
      v3 = v3 > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    if (!isfinite(v4)) {
      v4 = v4 > 0 ? numeric_limits<float>::max() : -numeric_limits<float>::max();
    }
    //cout << v1 << " " << v2 << " " << v3 << " " << v4 << endl;

		vals.push_back(v1);
		vals.push_back(v4);

		if (isSquare) {
			if (mlval < 0 && mhval > 0) {
				vals.push_back(0);
			}
		} else {
			vals.push_back(v2);
			vals.push_back(v3);
		}
		
    gsl_vector* g1 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g1, mlgrads);
    gsl_vector_scale(g1, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_add(g1, tmp);
    grads.push_back(g1);
		
		gsl_vector* g4 = gsl_vector_alloc(nctrls);
		gsl_vector_memcpy(g4, mhgrads);
		gsl_vector_scale(g4, fhval);
		gsl_vector_memcpy(tmp, fhgrads);
		gsl_vector_scale(tmp, mhval);
		gsl_vector_add(g4, tmp);
		grads.push_back(g4);
		if (isSquare) {
			if (mlval < 0 && mhval > 0) {
				gsl_vector* g2 = gsl_vector_alloc(nctrls);
				for (int i = 0; i < nctrls; i++) { // TODO: is this correct?
					gsl_vector_set(g2, i, 0);
				}
				grads.push_back(g2);
			}
		} else {
			gsl_vector* g2 = gsl_vector_alloc(nctrls);
			gsl_vector_memcpy(g2, mlgrads);
			gsl_vector_scale(g2, fhval);
			gsl_vector_memcpy(tmp, fhgrads);
			gsl_vector_scale(tmp, mlval);
			gsl_vector_add(g2, tmp);
			grads.push_back(g2);
			
			gsl_vector* g3 = gsl_vector_alloc(nctrls);
			gsl_vector_memcpy(g3, mhgrads);
			gsl_vector_scale(g3, flval);
			gsl_vector_memcpy(tmp, flgrads);
			gsl_vector_scale(tmp, mhval);
			gsl_vector_add(g3, tmp);
			grads.push_back(g3);
		}
  } else {
    pair<int,int> mrange = r(node.mother);
    pair<int,int> frange = r(node.father);
    int mlval =  mrange.first;
    int mhval = mrange.second;
    int flval = frange.first;
    int fhval = frange.second;
    int v1 = mlval * flval;
    int v2 = mlval * fhval;
    int v3 = mhval * flval;
    int v4 = mhval * fhval;
    vals.push_back(v1);
		vals.push_back(v4);
		if (isSquare) {
			if (mlval < 0 && mhval > 0) {
				vals.push_back(0);
			}
		} else {
			vals.push_back(v2);
			vals.push_back(v3);
		}
    gsl_vector* g1 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g1, mlgrads);
    gsl_vector_scale(g1, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_add(g1, tmp);
    grads.push_back(g1);
		
		gsl_vector* g4 = gsl_vector_alloc(nctrls);
		gsl_vector_memcpy(g4, mhgrads);
		gsl_vector_scale(g4, fhval);
		gsl_vector_memcpy(tmp, fhgrads);
		gsl_vector_scale(tmp, mhval);
		gsl_vector_add(g4, tmp);
		grads.push_back(g4);
		
		if (isSquare) {
			if (mlval < 0 && mhval > 0) {
				gsl_vector* g2 = gsl_vector_alloc(nctrls);
				for (int i = 0; i < nctrls; i++) { // TODO: is this correct?
					gsl_vector_set(g2, i, 0);
				}
				grads.push_back(g2);
			}
		} else {
			gsl_vector* g2 = gsl_vector_alloc(nctrls);
			gsl_vector_memcpy(g2, mlgrads);
			gsl_vector_scale(g2, fhval);
			gsl_vector_memcpy(tmp, fhgrads);
			gsl_vector_scale(tmp, mlval);
			gsl_vector_add(g2, tmp);
			grads.push_back(g2);
			gsl_vector* g3 = gsl_vector_alloc(nctrls);
			gsl_vector_memcpy(g3, mhgrads);
			gsl_vector_scale(g3, flval);
			gsl_vector_memcpy(tmp, flgrads);
			gsl_vector_scale(tmp, mhval);
			gsl_vector_add(g3, tmp);
			grads.push_back(g3);
		}
  }
	
	findMinMax(node, vals, grads);
}

void RangeDiff::visit( ARRACC_node& node ) {
  //cout << "Visiting ARRACC node" << endl;
  pair<int,int> irange = r(node.mother);
  Assert(node.mother->type == bool_node::SRC, "Something is wrong");
  SRC_node* inode = dynamic_cast<SRC_node*>(node.mother);
  if (!(*inputs).contains(inode->name)) {
    // take min and max of all values
    vector<float> lvals;
    vector<float> hvals;
    bool isFloat = (node.getOtype() == OutType::FLOAT);
    for (int i = 0; i < node.multi_mother.size(); i++) {
      pair<int,int> range = r(node.multi_mother[i]);
      if (isFloat) {
        lvals.push_back(floats.getFloat(range.first));
        hvals.push_back(floats.getFloat(range.second));
      } else {
        lvals.push_back(range.first);
        hvals.push_back(range.second);
      }
    }
    float minlval = numeric_limits<float>::max();
    float maxlval = -numeric_limits<float>::max();
    float minhval = numeric_limits<float>::max();
    float maxhval = -numeric_limits<float>::max();
    for (int i = 0; i < lvals.size(); i++) {
      if (lvals[i] > maxlval) {
        maxlval = lvals[i];
      }
      if (lvals[i] < minlval) {
        minlval = lvals[i];
      }
      if (hvals[i] > maxhval) {
        maxhval = hvals[i];
      }
      if (hvals[i] < minhval) {
        minhval = hvals[i];
      }
    }
    float alpha = 1.0;
    float explval = 0.0;
    float exphval = 0.0;
    for (int i = 0; i < lvals.size(); i++) {
      explval += exp(-alpha * (lvals[i] - maxlval));
      exphval += exp(alpha * (hvals[i] - maxhval));
    }
    /*cout << "lvals: ";
    for (int i = 0; i < lvals.size(); i++) {
      cout << lvals[i] << " ";
    }
    cout << endl;
    cout << "hvals: ";
    for (int i = 0; i < hvals.size(); i++) {
      cout << hvals[i] << " ";
    }
    cout << endl;
    
    cout << "maxlval: " << maxlval << endl;
    cout << "maxhval: " << maxhval << endl;
    
    cout << "explval: " << explval << endl;
    cout << "exphval: " << exphval << endl;*/
    float lval = (log(explval) - alpha*maxlval)/(-alpha);
    float hval = (log(exphval) + alpha*maxhval)/alpha;
    /*cout << "lval: " << lval << endl;
    cout << "hval: " << hval << endl;*/
    if (isFloat) {
      setrange(node, floats.getIdx(minlval), floats.getIdx(maxhval));
    } else {
      setrange(node, int(minlval), int(maxhval));
    }
    gsl_vector* l = lgrads[node.id];
    gsl_vector* h = hgrads[node.id];
    for (int i = 0; i < lvals.size(); i++) {
      gsl_vector_memcpy(tmp, lgrads[node.multi_mother[i]->id]);
      gsl_vector_scale(tmp, exp(-alpha*(lvals[i] - maxlval)));
      if ( i == 0)
        gsl_vector_memcpy(l, tmp);
      else
        gsl_vector_add(l, tmp);
      gsl_vector_memcpy(tmp, hgrads[node.multi_mother[i]->id]);
      gsl_vector_scale(tmp, exp(alpha*(hvals[i] - maxhval)));
      if ( i == 0)
        gsl_vector_memcpy(h, tmp);
      else
        gsl_vector_add(h, tmp);
    }
    gsl_vector_scale(l, 1.0/explval);
    gsl_vector_scale(h, 1.0/exphval);
    
    //Assert(false, "NYI");
  } else {
    assert(irange.first == irange.second);
    int idx = irange.first;
    if (idx < node.multi_mother.size() && idx >= 0) {
      pair<int,int> range = r(node.multi_mother[idx]);
      setrange(node, range.first, range.second);
      gsl_vector* l = lgrads[node.id];
      gsl_vector* h = hgrads[node.id];
      gsl_vector_memcpy(l, lgrads[node.multi_mother[idx]->id]);
      gsl_vector_memcpy(h, hgrads[node.multi_mother[idx]->id]);
    }
  }
}
void RangeDiff::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
  NodeEvaluator::visit(node);
  //Assert(false, "NYI");
  gsl_vector* mlgrads = lgrads[node.mother->id];
  gsl_vector* flgrads = lgrads[node.father->id];
  gsl_vector* mhgrads = hgrads[node.mother->id];
  gsl_vector* fhgrads = hgrads[node.father->id];
  
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  
  vector<float> vals;
  vector<gsl_vector*> grads;
  
  bool isFloat = node.getOtype() == OutType::FLOAT;
  if (isFloat) {
    pair<int,int> mrange = r(node.mother);
    float mlval = floats.getFloat(mrange.first);
    float mhval = floats.getFloat(mrange.second);
    pair<int,int> frange = r(node.father);
    float flval = floats.getFloat(frange.first);
    float fhval = floats.getFloat(frange.second);
    if (flval <= 0 && fhval >= 0) {
      //cout <<"Div range: " << flval << " " << fhval << endl;
      //Assert(false, "NYI: Zero in the interval for division");
      // TODO: check if this correct
      if ((mlval < 0 && mhval > 0) || (flval < 0 && fhval > 0)) {
        setrange(node, floats.getIdx(-numeric_limits<float>::max()), floats.getIdx(numeric_limits<float>::max()));
        for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
          gsl_vector_set(l, i, 0.0);
          gsl_vector_set(h, i, 0.0);
        }
			} else if (flval == 0 && fhval == 0) {
				if (mlval >= 0) {
					setrange(node, floats.getIdx(numeric_limits<float>::max()), floats.getIdx(numeric_limits<float>::max()));
					for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
						gsl_vector_set(l, i, 0.0);
						gsl_vector_set(h, i, 0.0);
					}
				} else if (mhval <= 0) {
					setrange(node, floats.getIdx(-numeric_limits<float>::max()), -floats.getIdx(numeric_limits<float>::max()));
					for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
						gsl_vector_set(l, i, 0.0);
						gsl_vector_set(h, i, 0.0);
					}
				}
				// the case mlval < 0 and mhval > 0 is already handled.
			} else if (mlval >= 0 && flval == 0) {
        float minv = mlval/fhval;
        setrange(node, floats.getIdx(minv), floats.getIdx(numeric_limits<float>::max()));
        for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
          gsl_vector_set(h, i, 0.0);
        }
        gsl_vector_memcpy(l, mlgrads);
        gsl_vector_scale(l, fhval);
        gsl_vector_memcpy(tmp, fhgrads);
        gsl_vector_scale(tmp, mlval);
        gsl_vector_sub(l, tmp);
        gsl_vector_scale(l, 1.0/(fhval*fhval));
      } else if (mhval <= 0 && flval == 0) {
        float maxv = mhval/fhval;
        setrange(node, floats.getIdx(-numeric_limits<float>::max()), floats.getIdx(maxv));
        for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
          gsl_vector_set(l, i, 0.0);
        }
        gsl_vector_memcpy(h, mhgrads);
        gsl_vector_scale(h, fhval);
        gsl_vector_memcpy(tmp, fhgrads);
        gsl_vector_scale(tmp, mhval);
        gsl_vector_sub(h, tmp);
        gsl_vector_scale(h, 1.0/(fhval*fhval));
        
      } else if (mlval >= 0 && fhval == 0) {
        float maxv = mlval/flval;
        setrange(node, floats.getIdx(-numeric_limits<float>::max()), floats.getIdx(maxv));
        for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
          gsl_vector_set(l, i, 0.0);
        }
        gsl_vector_memcpy(h, mlgrads);
        gsl_vector_scale(h, flval);
        gsl_vector_memcpy(tmp, flgrads);
        gsl_vector_scale(tmp, mlval);
        gsl_vector_sub(h, tmp);
        gsl_vector_scale(h, 1.0/(flval*flval));
        
      } else if (mhval <= 0 && fhval == 0) {
        float minv = mhval/flval;
        setrange(node, floats.getIdx(minv), floats.getIdx(numeric_limits<float>::max()));
        for (int i = 0; i < nctrls; i++) { // TODO: can we do any thing better here?
          gsl_vector_set(h, i, 0.0);
        }
        gsl_vector_memcpy(l, mhgrads);
        gsl_vector_scale(l, flval);
        gsl_vector_memcpy(tmp, flgrads);
        gsl_vector_scale(tmp, mhval);
        gsl_vector_sub(l, tmp);
        gsl_vector_scale(l, 1.0/(flval*flval));
      } else {
        Assert(false, "NYI: unaccounted case");
      }
      return;
      
    }
    float v1 = mlval / flval;
    float v2 = mlval / fhval;
    float v3 = mhval / flval;
    float v4 = mhval / fhval;
    vals.push_back(v1);
    vals.push_back(v2);
    vals.push_back(v3);
    vals.push_back(v4);
    
    gsl_vector* g1 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g1, mlgrads);
    gsl_vector_scale(g1, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_sub(g1, tmp);
    gsl_vector_scale(g1, 1.0/(flval*flval));
    grads.push_back(g1);
    gsl_vector* g2 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g2, mlgrads);
    gsl_vector_scale(g2, fhval);
    gsl_vector_memcpy(tmp, fhgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_sub(g2, tmp);
    gsl_vector_scale(g2, 1.0/(fhval*fhval));
    grads.push_back(g2);
    gsl_vector* g3 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g3, mhgrads);
    gsl_vector_scale(g3, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mhval);
    gsl_vector_sub(g3, tmp);
    gsl_vector_scale(g3, 1.0/(flval*flval));
    grads.push_back(g3);
    gsl_vector* g4 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g4, mhgrads);
    gsl_vector_scale(g4, fhval);
    gsl_vector_memcpy(tmp, fhgrads);
    gsl_vector_scale(tmp, mhval);
    gsl_vector_sub(g4, tmp);
    gsl_vector_scale(g4, 1.0/(fhval*fhval));
    grads.push_back(g4);
  } else {
    pair<int,int> mrange = r(node.mother);
    pair<int,int> frange = r(node.father);
    int mlval =  mrange.first;
    int mhval = mrange.second;
    int flval = frange.first;
    int fhval = frange.second;
    int v1 = mlval/flval;
    int v2 = mlval/fhval;
    int v3 = mhval/flval;
    int v4 = mhval/fhval;
    vals.push_back(v1);
    vals.push_back(v2);
    vals.push_back(v3);
    vals.push_back(v4);
    gsl_vector* g1 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g1, mlgrads);
    gsl_vector_scale(g1, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_sub(g1, tmp);
    gsl_vector_scale(g1, 1.0/(flval*flval));
    grads.push_back(g1);
    gsl_vector* g2 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g2, mlgrads);
    gsl_vector_scale(g2, fhval);
    gsl_vector_memcpy(tmp, fhgrads);
    gsl_vector_scale(tmp, mlval);
    gsl_vector_sub(g2, tmp);
    gsl_vector_scale(g2, 1.0/(fhval*fhval));
    grads.push_back(g2);
    gsl_vector* g3 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g3, mhgrads);
    gsl_vector_scale(g3, flval);
    gsl_vector_memcpy(tmp, flgrads);
    gsl_vector_scale(tmp, mhval);
    gsl_vector_sub(g3, tmp);
    gsl_vector_scale(g3, 1.0/(flval*flval));
    grads.push_back(g3);
    gsl_vector* g4 = gsl_vector_alloc(nctrls);
    gsl_vector_memcpy(g4, mhgrads);
    gsl_vector_scale(g4, fhval);
    gsl_vector_memcpy(tmp, fhgrads);
    gsl_vector_scale(tmp, mhval);
    gsl_vector_sub(g4, tmp);
    gsl_vector_scale(g4, 1.0/(fhval*fhval));
    grads.push_back(g4);

  }
	findMinMax(node, vals, grads);
  
}
void RangeDiff::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  NodeEvaluator::visit(node);
  Assert(false, "NYI");
}
void RangeDiff::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
  pair<int,int> mrange = r(node.mother);
  setrange(node, -mrange.second, -mrange.first);
  gsl_vector* mlgrads = lgrads[node.mother->id];
  gsl_vector* mhgrads = hgrads[node.mother->id];
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  gsl_vector_memcpy(l, mhgrads);
  gsl_vector_scale(l, -1.0);
  gsl_vector_memcpy(h, mlgrads);
  gsl_vector_scale(h, -1.0);
}
void RangeDiff::visit( CONST_node& node ) {
  //cout << "Visiting CONST node" << endl;
  int val = -1;
  if (node.isFloat()) {
    val = floats.getIdx(node.getFval());
  } else {
    val = node.getVal();
  }
  setrange(node, val, val);
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  for (int i = 0; i < nctrls; i++) {
    gsl_vector_set(l, i, 0.0);
    gsl_vector_set(h, i, 0.0);
  }
}
void RangeDiff::visit( LT_node& node ) {
  //cout << "Visiting LT node" << endl;
  NodeEvaluator::visit(node);
}
void RangeDiff::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
  NodeEvaluator::visit(node);
}
void RangeDiff::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  NodeEvaluator::visit(node);
  Assert(false, "NYI");
}

void RangeDiff::visit( UFUN_node& node ){
  NodeEvaluator::visit(node);
  gsl_vector* mlgrads = lgrads[node.multi_mother[0]->id];
  gsl_vector* mhgrads = hgrads[node.multi_mother[0]->id];
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  gsl_vector_memcpy(l, mlgrads);
  gsl_vector_memcpy(h, mhgrads);
  float dl = 0.0;
  float dh = 0.0;
  float vl = 0.0;
  float vh = 0.0;
  const string& name = node.get_ufname();
  if (name == "_cast_int_float_math") {
    dl = 1.0;
    dh = 1.0;
  }
  else if (floats.hasFun(name)) {
    pair<int,int> mrange = r(node.multi_mother[0]);
    float xl = floats.getFloat(mrange.first);
    float xh = floats.getFloat(mrange.second);
    //cout << name << " " << xl << " " << xh << endl;

    if (name == "arctan_math") {
      Assert(xl == xh, "NYI: range computation for arctan");
      dl = 1.0/(xl*xl + 1.0);
      dh = 1.0/(xh*xh + 1.0);
      vl = atan(xl);
      vh = atan(xh);
    } else if (name == "sin_math") {
      //Assert(xl == xh, "NYI: range computation for sin");
      if (xl == xh) {
        dl = cos(xl);
        dh = cos(xh);
        vl = sin(xl);
        vh = sin(xh);
			} else if (xl >= -pi/2 && xl <= pi/2 && xh >= -pi/2 && xh <= pi/2) {
				vl = sin(xl);
				vh = sin(xh);
				dl = cos(xl);
				dh = cos(xh);
			}else { // TODO: this is so course grained
        dl = 0.0;
        dh = 0.0;
        vl = -1.0;
        vh = 1.0;
      }
    } else if (name == "cos_math") {
      //Assert(xl == xh, "NYI: range computation for cos");
      if (xl == xh) {
        dl = -sin(xl);
        dh = -sin(xh);
        vl = cos(xl);
        vh = cos(xh);
			} else if (xl >= 0 && xl <= pi && xh >= 0 && xh <= pi) {
				vl = cos(xh);
				vh = cos(xl);
				dl = -sin(xh);
				dh = -sin(xl);
			} else if (xl <= 0 && xl >= -pi && xh <= 0 && xh >= -pi) {
				vl = cos(xl);
				vh = cos(xh);
				dl = -sin(xl);
				dh = -sin(xh);
			} else {
        dl = 0.0;
        dh = 0.0;
        vl = -1.0;
        vh = 1.0;
      }
    } else if (name == "tan_math") {
      Assert(xl == xh, "NYI: range computation for tan");
      dl = 1.0/(cos(xl)*cos(xl));
      dh = 1.0/(cos(xh)*cos(xh));
      vl = tan(xl);
      vh = tan(xh);
    } else if (name == "sqrt_math") {
      if (xl < 0.0) {
        xl = 0.0;
      }
      dl = 0.5/sqrt(xl);
      dh = 0.5/sqrt(xh);
      vl = sqrt(xl);
      vh = sqrt(xh);
    } else {
      Assert(false, "NYI");
    }
  } else {
    Assert(false, "NYI");
  }
  //cout << vl << " " << vh << endl;
  if (vl > vh) {
    setrange(node, floats.getIdx(vh), floats.getIdx(vl));
    gsl_vector_scale(l, dh);
    gsl_vector_scale(h, dl);
  } else {
    setrange(node, floats.getIdx(vl), floats.getIdx(vh));
    gsl_vector_scale(l, dl);
    gsl_vector_scale(h, dh);
  }
}

void RangeDiff::visit( TUPLE_R_node& node) {
  NodeEvaluator::visit(node);
  gsl_vector* l = lgrads[node.id];
  gsl_vector* h = hgrads[node.id];
  
  if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
    gsl_vector_memcpy(l, lgrads[node.mother->id]);
    gsl_vector_memcpy(h, hgrads[node.mother->id]);
    pair<int,int> mrange = r(node.mother);
    setrange(node, mrange.first, mrange.second);
  } else {
    Assert(false, "NYI");
  }
}

bool RangeDiff::run(VarStore& inputs_p){
  funargs.clear();
  inputs = &inputs_p;
  int i=0;
  failedAssert = false;
  failedHAssert = false;
  for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
    (*node_it)->accept(*this);
    /*cout << (*node_it)->lprint() << endl;
    pair<int,int> mrange = r(*node_it);
    if (true  || mrange.first != mrange.second) {
    if ((*node_it)->getOtype() == OutType::FLOAT) {
      // TODO: add these asserts properly
      //if (floats.getFloat(mrange.first) > floats.getFloat(mrange.second)) {
      //  cout << "wrong" << endl;
      //}
      cout << "Range: " << floats.getFloat(mrange.first) << " " << floats.getFloat(mrange.second) << endl;
		} else {
      //if (mrange.first > mrange.second) {
      //  cout << "wrong" << endl;
      //}
      cout << "Range: " << mrange.first << " " << mrange.second << endl;
    }
			gsl_vector* lgrads = getLGrad((*node_it));
			cout << "LGrads: ";
			for (int i = 0; i < lgrads->size; i++) {
				cout << gsl_vector_get(lgrads, i) << ", ";
			}
			cout << endl;
			gsl_vector* hgrads = getHGrad((*node_it));
			cout << "HGrads: " ;
			for (int i = 0; i < hgrads->size; i++) {
				cout << gsl_vector_get(hgrads, i) << ", ";
			}
			cout << endl;

    }*/
    if(failedAssert){
      return true;
    }
    if(failedHAssert){
      return false;
    }
  }
  return false;
}

