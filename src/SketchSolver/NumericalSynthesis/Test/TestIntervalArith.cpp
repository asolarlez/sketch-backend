#import "IntervalGrad.h"
#import <iostream>
#include <cassert>


using namespace std;

void init(int nctrls) {
	IntervalGrad::tmp = gsl_vector_alloc(nctrls);
	IntervalGrad::tmp1 = gsl_vector_alloc(nctrls);
	IntervalGrad::tmp2 = gsl_vector_alloc(nctrls);
	IntervalGrad::tmp3 = gsl_vector_alloc(nctrls);
	IntervalGrad::tmpT = gsl_vector_alloc(nctrls);
}

void destroy() {
	delete IntervalGrad::tmp;
	delete IntervalGrad::tmp1;
	delete IntervalGrad::tmp2;
	delete IntervalGrad::tmp3;
}

void testFindMin1() {
	init(1);
	float v1 = 1.0;
	float v2 = 2.0;
	gsl_vector* g1 = gsl_vector_alloc(1);
	gsl_vector_set(g1, 0, 0.0);
	gsl_vector* g2 = gsl_vector_alloc(1);
	gsl_vector_set(g2, 0, 0.0);
	gsl_vector* l = gsl_vector_alloc(1);
	
	float minv = IntervalGrad::findMin(v1, v2, g1, g2, l);
	assert(minv == 1.0);
	assert(gsl_vector_get(l, 0) == 0.0);
	destroy();
	delete g1;
	delete g2;
	delete l;
}

void testConditionalUnion1() {
	init(1);
	float v1 = 1.0;
	float v2 = 2.0;
	gsl_vector* g1l = gsl_vector_alloc(1);
	gsl_vector_set(g1l, 0, 0.0);
	gsl_vector* g1h = gsl_vector_alloc(1);
	gsl_vector_set(g1h, 0, 0.0);
	gsl_vector* g2l = gsl_vector_alloc(1);
	gsl_vector_set(g2l, 0, 0.0);
	gsl_vector* g2h = gsl_vector_alloc(1);
	gsl_vector_set(g2h, 0, 0.0);
	IntervalGrad* m = new IntervalGrad(v1, v1, g1l, g1h);
	IntervalGrad* f = new IntervalGrad(v2, v2, g2l, g2h);
	float d = 0.0;
	gsl_vector* gd = gsl_vector_alloc(1);
	gsl_vector_set(gd, 0, 1.0);
	DistanceGrad* dg = new DistanceGrad(d, gd);
	
	gsl_vector* gol = gsl_vector_alloc(1);
	gsl_vector* goh = gsl_vector_alloc(1);
	IntervalGrad* o = new IntervalGrad(0, 0, gol, goh);
	
	IntervalGrad::ig_conditionalUnion(m, f, dg, o);
	assert(gsl_vector_get(o->getLGrad(), 0) > 0);
	assert(gsl_vector_get(o->getHGrad(), 0) > 0);
	
	destroy();
	delete m;
	delete f;
	delete dg;
	delete o;
}


void testSigmoid() {
	init(4);
	gsl_vector* g1l = gsl_vector_alloc(4);
	gsl_vector_set(g1l, 0, 0); gsl_vector_set(g1l, 1, 0); gsl_vector_set(g1l, 2, 0); gsl_vector_set(g1l, 3, 0.0333333);
	gsl_vector* g1h = gsl_vector_alloc(4);
	gsl_vector_memcpy(g1h, g1l);
	IntervalGrad* m = new IntervalGrad(10.0, 10.0, g1l, g1h);
	gsl_vector* g2l = gsl_vector_alloc(4);
	gsl_vector_set(g2l, 0, 0); gsl_vector_set(g2l, 1, 0); gsl_vector_set(g2l, 2, 0); gsl_vector_set(g2l, 3, 0.1333333);
	gsl_vector* g2h = gsl_vector_alloc(4);
	gsl_vector_memcpy(g2h, g2l);
	IntervalGrad* f = new IntervalGrad(10.0, 10.0, g2l, g2h);
	gsl_vector* gd = gsl_vector_alloc(4);
	gsl_vector_set(gd, 0, -0.119203); gsl_vector_set(gd, 1, 0.880797); gsl_vector_set(gd, 2, 0); gsl_vector_set(gd, 3, 0);
	DistanceGrad* dg = new DistanceGrad(-0.112693, gd);
	gsl_vector* gol = gsl_vector_alloc(4);
	gsl_vector* goh = gsl_vector_alloc(4);
	IntervalGrad* o = new IntervalGrad(0, 0, gol, goh);
	IntervalGrad::ig_conditionalUnion(m, f, dg, o);
	destroy();
	delete m;
	delete f;
	delete dg;
	delete o;
}
int main() {
	//testFindMin1();
	
	//testConditionalUnion1();
	
	testSigmoid();
	
	cout << "Passed tests" << endl;
	return 0;
}
