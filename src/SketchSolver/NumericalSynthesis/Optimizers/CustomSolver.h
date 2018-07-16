#pragma once

#include <vector>
#include "OptimizationWrapper.h"
#include "SymbolicEvaluator.h"
#include "FakeGSL.h"
#include "CommandLineArgs.h"
#include "GradUtil.h"

extern CommandLineArgs* PARAMS;

using namespace std;
typedef int integer;
typedef double doublereal;

typedef float(*floatfun)(float);

typedef int(DFT)(integer*, integer *, doublereal [],
	integer    *, integer *, doublereal [],
	integer    *, integer *, doublereal [],
	char      *, integer *,
	integer    [], integer *,
	doublereal [], integer *);

#include "Util.h"


#if 0

class SnoptParameters {
public:
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	//vector<vector<int>>& allInputs;
	//map<int, int>& imap;
	const set<int>& boolNodes;
	int minimizeNode;
	double beta;
	double alpha;
	bool onlyInputs = false;

	SnoptParameters(SymbolicEvaluator* eval_, const set<int>& boolNodes_, int minimizeNode_) : eval(eval_), boolNodes(boolNodes_), minimizeNode(minimizeNode_) {}
	//SnoptParameters(SymbolicEvaluator* eval_, BooleanDAG* dag_, vector<vector<int>>& allInputs_, map<int, int>& imap_, set<int>& boolNodes_, bool onlyInputs_ = false) : eval(eval_), dag(dag_), allInputs(allInputs_), imap(imap_), boolNodes(boolNodes_), onlyInputs(onlyInputs_) {}
};


class SnoptEvaluator {
	static gsl_vector* state;
	static gsl_vector* grad;
public:
	static int counter;
	static void init(int size) {
		state = gsl_vector_alloc(size);
		grad = gsl_vector_alloc(size);
		//tmpGrad = gsl_vector_alloc(size);
	}

	static int df(integer    *Status, integer *n, doublereal x[],
		integer    *needF, integer *neF, doublereal F[],
		integer    *needG, integer *neG, doublereal G[],
		char      *cu, integer *lencu,
		integer    iu[], integer *leniu,
		doublereal ru[], integer *lenru) {
		SnoptParameters* p = (SnoptParameters*)cu;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		//cout << counter++ << endl;
		//cout << "x: ";
		for (int i = 0; i < *n; i++) {
			gsl_vector_set(state, i, x[i]);
			// cout << x[i] << " ";
		}
		//cout << endl;
		p->eval->run(state);

		int fcounter = 0;
		int gcounter = 0;
		if (p->minimizeNode < 0) {
			F[fcounter++] = 0; // objective
			for (int j = 0; j < *n; j++) { // objective gradients
				G[gcounter++] = 0;
			}
		}
		else {
			double dist = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
			F[fcounter++] = dist;
			for (int j = 0; j < *n; j++) {
				G[gcounter++] = gsl_vector_get(grad, j);
			}
		}
		for (auto it = p->boolNodes.begin(); it != p->boolNodes.end(); it++) {
			double dist = p->eval->getErrorOnConstraint(*it, grad);
			F[fcounter++] = dist;
			for (int j = 0; j < *n; j++) {
				G[gcounter++] = gsl_vector_get(grad, j);
			}
		}

		//cout << "Fcounter: " << fcounter << " " << *neF << endl;
		for (int i = fcounter; i < *neF; i++) {
			F[i] = 1000;
			for (int j = 0; j < *n; j++) {
				G[gcounter++] = gsl_vector_get(grad, j);
			}
		}
		Assert(fcounter <= *neF, "dfqeurq");
		return 0;
	}
};
#endif 


typedef enum {STALL=(12), DONE=(2), SATISFIED=(5)}  NoSnoptStatus;


class NotSnoptSolver {

	integer n; integer neF; integer lenA; integer lenG; integer neG;

	integer *iAfun;
	integer *jAvar;
	doublereal *A;

	integer *iGfun;
	integer *jGvar;

	vec<doublereal> x;
	doublereal *xlow;	// lower bound on unknowns
	doublereal *xupp;	// upper bound on unknowns
	doublereal *xmul;
	integer *xstate;

	doublereal *F;
	doublereal *Flow;	// lower bound on constraint outputs
	doublereal *Fupp;	// upper bound on constraint outputs
	doublereal *Fmul;
	doublereal *G;
	vec<doublereal> Gtotal;
	vec<doublereal> GtotOld;
	integer *Fstate;

	integer nxnames;
	integer nFnames = 1;
	char *xnames;
	char *Fnames;
	DFT* mydf;
	char* workspace;
	gsl_vector* result;
	double objectiveVal;

	integer Cold = 0, Basis = 1, Warm = 2;
public:
	NotSnoptSolver(integer n_, integer neF_, integer lenA_) : n(n_), neF(neF_), lenA(lenA_) {
		iAfun = new integer[lenA];
		jAvar = new integer[lenA];
		A = new doublereal[lenA];
		Gtotal.growTo(n); 
		GtotOld.growTo(n); 

		lenG = n*neF;
		iGfun = new integer[lenG];
		jGvar = new integer[lenG];
		G = new doublereal[lenG];

		x.growTo(n);
		xlow = new doublereal[n]; // lower bounds for every unknown.
		xupp = new doublereal[n]; // upper bounds for every unknown.
		xmul = new doublereal[n];
		xstate = new integer[n];

		F = new doublereal[neF];
		Flow = new doublereal[neF];
		Fupp = new doublereal[neF];
		Fmul = new doublereal[neF];
		Fstate = new integer[neF];

		nxnames = 1;
		nFnames = 1;
		xnames = new char[nxnames * 8];
		Fnames = new char[nFnames * 8];

		

		result = gsl_vector_alloc(n);
	}

	~NotSnoptSolver() {
		gsl_vector_free(result);
		delete[]iAfun; delete[]jAvar; delete[]A;		
		delete[]iGfun; delete[]jGvar;
		delete[]xlow; delete[]xupp;
		delete[]xmul; delete[]xstate;
		delete[]F; delete[]Flow; delete[]Fupp;
		delete[]Fmul; delete[]Fstate;
		delete[]xnames; delete[]Fnames;
	}
	
	void init(char* _workspace, integer neF_, DFT _df, integer ObjRow, doublereal ObjAdd, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_) {
		Assert(neF_ <= neF, "Error: small nef");
		neF = neF_; // number of constraints + 1
		for (integer i = 0; i < n; i++) {
			xlow[i] = xlow_[i];
			xupp[i] = xupp_[i];
		}

		for (integer i = 0; i < neF; i++) {
			Flow[i] = Flow_[i];
			Fupp[i] = Fupp_[i];
		}
		integer neA = 0;
		neG = 0;
		for (integer i = 0; i < neF; i++) {
			for (integer j = 0; j < n; j++) {
				iGfun[neG] = i;
				jGvar[neG] = j;
				neG++;
			}
		}
		workspace = _workspace;
		mydf = _df;
		/*
		snoptProb.setProblemSize(n, neF);
		snoptProb.setObjective(ObjRow, ObjAdd);
		snoptProb.setA(lenA, iAfun, jAvar, A);
		snoptProb.setG(lenG, iGfun, jGvar);
		snoptProb.setX(x, xlow, xupp, xmul, xstate);
		snoptProb.setF(F, Flow, Fupp, Fmul, Fstate);

		snoptProb.setNeA(neA);
		snoptProb.setNeG(neG);
		snoptProb.setUserFun(df);
		snoptProb.setWorkspace(workspace);
		*/

	}

	double dot(doublereal* v1, doublereal* v2, integer len) {
		double rv = 0.0;
		for (int i = 0; i < len; ++i) {		
			rv += v1[i]*v2[i];
		}
		return rv;
	}

	double partialNorm(doublereal* v, integer len) {
		double rv = 0.0;
		for (int i = 0; i < len; ++i) {
			double t = v[i];
			if (t < 0) {
				rv += t*t;
			}
		}
		return sqrt(rv);
	}

	double l2norm(doublereal* v, integer len) {
		double rv = 0.0;
		for (int i = 0; i < len; ++i) {
			double t = v[i];
			rv += t*t;			
		}
		return sqrt(rv);
	}

	double normalize(doublereal*v, integer len, double threshold=0.0001) {
		double norm = l2norm(v, len);
		if (norm > threshold) {
			for (int i = 0; i < len; ++i) {
				v[i] = v[i] / norm;
			}
		}
		return norm;
	}

	double reduce(double* in, double*Fval, int lenOut, int lenIn, double* out) {
		double minval = 0.0;
		int minj = -1;
		for (int j = 1; j < lenIn; ++j) {
			if (Fval[j] < minval) {
				minval = Fval[j];
				minj = j;
			}
		}

		double rv = 0.0;
		for (int j = 1; j < lenIn; ++j) {
			if (Fval[j] < 0.0) {
				rv += (Fval[j] / minval)*Fval[j];
			}
		}

		if (minj == -1) {
			return 0.01;
		}

		// For the when j == minj, out[i] += in[j][i]
		// when j != minj, rv += F[j]^2 / F[minj]
		//             so out[i] = (2*F[j]*in[j][i]*F[minj] - F[j]^2*in[minj][i]) / F[minj]^2
		//                       = 2*F[j]*in[j][i]/minval  - F[j]*F[j]*in[minj][i]/(minval*minval)
		for (int i = 0; i < lenOut; ++i) {
			out[i] = 0.0;
			double in_i_minj = in[i + minj*lenOut];
			for (int j = 1; j < lenIn; ++j) {				
				if (Fval[j] < 0.0) {
					if (j == minj) {
						out[i] += in[i + j*lenOut];
					}
					else {
						double Fval_j = Fval[j];
						out[i] += 2 * Fval_j*in[i + j*lenOut] / minval - Fval_j*Fval_j*in_i_minj / (minval*minval);
					}					
				}
			}
		}
		return 0.01-rv;
	}

	double dist(vec<doublereal>& v1, vec<doublereal>& v2) {
		double rv = 0.0;
		for (int i = 0; i < v1.size(); ++i) {
			auto t = v1[i] - v2[i];
			rv += t*t;
		}
		return sqrt(rv);
	}

	void update(doublereal* x, doublereal* G, doublereal gamma, int n) {
		for (int j = 0; j < n; ++j) {			
				x[j] += gamma * G[j];	
		}
	}

	void cpy(doublereal* out, doublereal *in, int n) {
		for (int j = 0; j < n; ++j) {			
			out[j] = in[j];
		}
	}

	bool checkexit(doublereal* G, doublereal* F, int neF, int n) {
		for (int i = 1; i < neF; ++i) {
			if (F[i] < 0.0) {
				return false;
			}
		}
		for (int i = 0; i < n; ++i) {
			if (G[i] * G[i] > 0.00001) {
				return false;
			}
		}
		return true;
	}

	bool checkstall(doublereal* G, doublereal* Gopt, int neF, int lenG) {
		for (int i = 0; i < lenG; ++i) {
			if (G[i]*G[i] > 0.00001) {
				return false;
			}
		}
		for (int i = 0; i < lenG; ++i) {
			if (Gopt[i] * Gopt[i] > 0.00001) {
				return false;
			}
		}
		return true;
	}


	void display(vec<doublereal>& in) {
		/*
		cout << "HH [";
		for (int i = 0; i < n; ++i) {
			cout << in[i]; 
			if (i < n - 1) {
				cout << ", ";
			}
		}
		cout << "], \\"<<endl;
		*/
	}



	NoSnoptStatus solveConstraints() {
		integer status;
		integer lencu = 0;
		integer leniu = 0;
		integer lenru = 0;
		integer iu[2];
		doublereal ru[2];
		double prevfv = 100000000.0;
		doublereal gamma = 1.0;
		vec<doublereal> bestx;
		bestx.growTo(n);
		doublereal bestfv = 1000.0;
		int sinceLastImprovement = 0;
		bool frst = true;


		auto supercheck = [&]() {

			for (int i = 1; i < neF; ++i) {
				bestx.copyTo(x);
				mydf(&status, &this->n, x.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
					workspace, &lencu, iu, &leniu, ru, &lenru);
				double fv = F[i];

				double gnorm = normalize(G + i*n, n);
				update(x, G + i*n, 1e-6, n);
				mydf(&status, &this->n, x.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
					workspace, &lencu, iu, &leniu, ru, &lenru);
				double newfv = F[i];
				if (newfv > fv) {
					cout << "OK: " << i << endl;
				}
				else {
					cout << " BAD: " << i << endl;
				}
			}


		};




		auto lineSearch = [&]() {
			bestx.copyTo(x);
			mydf(&status, &this->n, x.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
				workspace, &lencu, iu, &leniu, ru, &lenru);
			double fv = reduce(G, F, n, neF, Gtotal);
			double gnorm = normalize(Gtotal, n);
			double fgamma = gamma * (fv / gnorm);
			cpy(GtotOld, Gtotal.begin(), n);
			do {
				update(x, Gtotal, fgamma, n);				
				mydf(&status, &this->n, x.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
					workspace, &lencu, iu, &leniu, ru, &lenru);
				fv = reduce(G, F, n, neF, Gtotal);
				gnorm = normalize(Gtotal, n);				

				if (fv < bestfv || gnorm < 0.0000001 || fgamma < 1e-14) {
					return;
				}
				else {
					double dd = dot(Gtotal.begin(), GtotOld, n);
					bestx.copyTo(x);
					fgamma = fgamma * 0.65;
					cpy(Gtotal.begin(), GtotOld, n);
				}
			} while (true);			
		};

		int zigzag = 0;
		do {
			display(x);
			mydf(&status, &this->n, x.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
				workspace, &lencu, iu, &leniu, ru, &lenru);

			double fv = reduce(G, F, n, neF, Gtotal);
			if (fv < bestfv) {
				bestfv = fv;
				x.copyTo(bestx);
				sinceLastImprovement = 0;
			}
			else {
				sinceLastImprovement++;
			}
			

			prevfv = fv;

			if (checkexit(G, F, neF, n)) {
				//cout << x[0] << "  " << x[1] << "  " << F[1] << "  " << F[2] << "  " << F[3] << "  " << F[4] << "  " << F[5] << "  " << F[6] << endl;				
				return DONE;
			}
			bool chkst = checkstall(Gtotal, G, neF, n);
			if (chkst || gamma < 0.0000001 || sinceLastImprovement > 150) {
				bestx.copyTo(x);
				lineSearch();
				return STALL;
			}
			


			auto updOld = [&](double* GVal) {
				if (frst) {
					gamma = 1.0;
					frst = false;
					cpy(GtotOld, GVal, n);
				}
				else {
					double dd = dot(GVal, GtotOld, n);
					double oldGamma = gamma;
					if (dd > 0.2) {
						gamma = gamma * 1.2;
						zigzag = 0;
					}
					if (dd < -0.2) {
						gamma = gamma * 0.8;
					}
					if (dd < 0.0) {
						zigzag += 1;
					}
					if (zigzag > 4) {
						//cout << "NFZZAG" << endl;
						gamma = oldGamma;
						for (int i = 0; i < n; ++i) {
							GVal[i] += GtotOld[i] * 0.4;
						}
						normalize(GVal, n);
					}

					cpy(GtotOld, GVal, n);
				}
			};
			double gnorm = normalize(Gtotal, n);
			double optnorm = normalize(G, n);


			auto checkBounds = [&]() {
				double mag = 0.0;
				for (int i = 0; i < n; ++i) {
					if (x[i] > xupp[i]) {
						x[i] = xupp[i];
						if (gamma > 1.0) { gamma = 1.0; }
					}
					else {
						if (x[i] < xlow[i]) {
							x[i] = xlow[i];
							if (gamma > 1.0) { gamma = 1.0; }
						}
						else {
							mag += Gtotal[i] * Gtotal[i];
						}
					}					
				}
				return mag < 0.0001;
			};

			if (gnorm > 0) {	

				{					
					double epsilon = 0.00000000001;
					double mag=0.0;
					for (int i = 0; i < n; ++i) {
						if (x[i] + epsilon >= xupp[i]) {
							Gtotal[i] = 0.0;							
						}
						else {
							if (x[i] - epsilon <= xlow[i]) {
								Gtotal[i] = 0.0;
							}
							else {
								mag += Gtotal[i] * Gtotal[i];
							}
						}
					}					
					if (mag < 0.9999999) {
						double threshold = 1e-9;
						double newnorm = normalize(Gtotal.begin(), n, threshold);
						if (newnorm < threshold) {
							cout << "CORNER REACHED " << endl;
							bestx.copyTo(x);
							return STALL;
						}
						gnorm = gnorm * newnorm;
					}
					//cout <<"NFAT "<< bestfv << ", " << sinceLastImprovement << "\t" << gamma <<"\t"<<mag<< endl;
				}

				updOld(Gtotal);
				double fgamma = gamma * (fv / gnorm);
				update(x, Gtotal, fgamma, n);
				if (optnorm > 0) {
					update(x, G, -fgamma / 20.0, n);
				}
				if (checkBounds()) {
					if (sinceLastImprovement > 10) {
						bestx.copyTo(x);
						return STALL;
					}					
				}
			}
			else {
				return SATISFIED;
			}
		} while (true);
	}

	



	integer solve(integer param) {
		integer status;
		integer lencu = 0;
		integer leniu = 0;
		integer lenru = 0;
		integer iu[2];
		doublereal ru[2];
		doublereal gamma = 1.0;
		bool frst = true;
		double prevfv = 100000000.0;

		vec<doublereal> tempx;
		tempx.growTo(n);
		vec<doublereal> tempG;
		tempG.growTo(n);
		vec<doublereal> tempPar;
		tempPar.growTo(n);


		vec<doublereal> ttempx;
		tempx.growTo(n);

		vec<doublereal> ttempPar;
		tempPar.growTo(n);

 
		int regressions = 0;
		int satflip = 0;
		bool lastsat = true;	

		auto rv = solveConstraints();
		if (rv == DONE) {
			cout << "SUCCESS" << endl;
			return (integer)DONE;
		}
		if (rv == STALL) {
			cout << "STALLED" << endl;
			return (integer)STALL;
		}
		int zigzag = 0;

		


		double THRESHOLD = 0.0001;
		double GOODFV = 0.01;


		//Outputs a final x and a final direction. It is also assumed that after this call, G will have the 
		//gradients computed on that final x.
		auto lineSearch = [&](vec<doublereal>& inx, doublereal scale, vec<doublereal>& direction, vec<doublereal>& outx, vec<doublereal>& outG) {



			auto run = [&]() {
				display(outx);
				mydf(&status, &this->n, outx.begin(), &this->neF, &this->neF, F, &this->lenG, &this->lenG, this->G,
					workspace, &lencu, iu, &leniu, ru, &lenru);
			};

			//First we check if we are really close to a condition violation, in which case we will just return with the outG vector without moving further.
			inx.copyTo(outx);
			update(outx, direction.begin(), THRESHOLD, n);
			run();
			double nearfv = reduce(G, F, n, neF, Gtotal);
			if (nearfv > GOODFV && outG.size() > 0) {
				inx.copyTo(outx);
				double ggn = normalize(Gtotal, n);
				double dd = dot(direction.begin(), Gtotal, n);
				if (dd < 0.0 && dd > -0.99) {
					for (int ii = 0; ii < n; ++ii) { outG[ii] = direction[ii] - Gtotal[ii] * dd; }
					run();
					return true;
				}
				else {
					run();
					return false;
				}
			}
			//If that did not fail, we do the standard binary search. 
			double oldF0 = F[0];
			bool hitBoundary = false;
			do {
				inx.copyTo(outx);
				update(outx, direction.begin(), scale, n);				
				run();
				double newfv = reduce(G, F, n, neF, Gtotal);

				if (newfv > GOODFV || F[0] > oldF0) {
					//Not as good as it was before. 
					scale = scale * 0.6;
					if (newfv > GOODFV && outG.size() > 0) {
						//This means we got back to the region where the constraints are not satisfied. 
						double ggn = normalize(Gtotal, n);
						double dd = dot(direction.begin(), Gtotal, n);
						if (dd < 0.0 && dd > -0.99) {							
							hitBoundary = true;
							for (int ii = 0; ii < n; ++ii) { outG[ii] = direction[ii] - Gtotal[ii] * dd; }
						}
					}
					if (scale < THRESHOLD) {
						inx.copyTo(outx);
						run();
						return hitBoundary;
					}
				}else{
					return hitBoundary;
				}
			} while (true);	
		};


		gamma = 1.0;

		do {
			
			double fv = reduce(G, F, n, neF, Gtotal);

			if (fv > prevfv*5.0) {
				regressions++;				
			}

			prevfv = fv;
			
			if (checkexit(G, F, neF, n)) {				
				return 2;
			}
			if (checkstall(Gtotal, G, neF, n) || gamma < 0.0000001 || regressions > 10) {
				return 12;
			}
			
						
			double optnorm = normalize(G, n);

			for (int i = 0; i < n; ++i) { tempG[i] = -G[i]; }
			
			double factor = gamma*optnorm;
			bool hit = lineSearch(x, factor, tempG, tempx, tempPar);

			if (hit) {
				normalize(tempPar.begin(), n);
				lineSearch(tempx, factor, tempPar, ttempx, ttempPar);
				double traveled = dist(ttempx, x);
				if (traveled < THRESHOLD) {
					return 2;
				}
				if (traveled < factor) {
					gamma = traveled / optnorm;
				}

				ttempx.copyTo(x);
			}
			else {				
				double traveled = dist(tempx, x);
				if (traveled < THRESHOLD) {
					return 2;
				}
				if (traveled < factor) {
					gamma = traveled / optnorm;
				}
				else {
					gamma = gamma*1.2;
				}
				tempx.copyTo(x);
			}


		} while (true);
		return 0;
	}

	bool optimize(gsl_vector* initState, bool suppressPrint = false) {
		for (int i = 0; i < n; i++) {
			x[i] = gsl_vector_get(initState, i);
			xstate[i] = 0;
		}

		for (int i = 0; i < neF; i++) {
			Fmul[i] = 0;
			Fstate[i] = 0;
		}
		/*
		snoptProb.setIntParameter("Derivative option", 1);
		snoptProb.setIntParameter("Major Iteration limit", 250);
		snoptProb.setRealParameter("Function precision", 0.0001);
		snoptProb.setRealParameter("Major optimality tolerance", 0.01);
		snoptProb.setRealParameter("Major feasibility tolerance", 0.0001);
		snoptProb.setRealParameter("Minor feasibility tolerance", 0.0001);
		snoptProb.setIntParameter("Scale option", 1);
		*/
		integer status = solve(Cold);
		// cout << "Solving again" << endl;
		// status = solve(Warm);

		for (int i = 0; i < n; i++) {
			gsl_vector_set(result, i, x[i]);
		}
		objectiveVal = F[0];
		if (!suppressPrint) {
			cout << "Status: " << status << endl;
			if (status >= 1 && status <= 9) {
				cout << "Solution found" << endl;
			}
			else if (status >= 11 && status <= 19) {
				cout << "Infeasible constraints" << endl;
			}
			else if (status == 91) {
				cout << "Invalid input" << endl;
			}
			else {
				cout << "Unknown error " << status << endl;
			}

			cout << "x = ";
			for (int i = 0; i < n; i++) {
				cout << x[i] << ", ";
			}
			cout << endl;
			cout << "F = ";
			for (int i = 0; i < 1; i++) {
				cout << F[i] << ", ";
			}
			cout << endl;
		}
		bool constraintsSatisfied = true;
		for (int i = 1; i < neF; i++) {
			if (F[i] < Flow[i] - 0.02 || F[i] > Fupp[i] + 0.02) {
				constraintsSatisfied = false;
			}
		}
		for (int i = 0; i < n; i++) {
			if (x[i] < xlow[i] - 0.02 || x[i] > xupp[i] + 0.02) {
				constraintsSatisfied = false;
			}
		}

		//if (status >= 1 && status <= 9) {
		//    Assert(constraintsSatisfied, "Something is wrong with snopt");
		//}
		if (!suppressPrint) {
			if (constraintsSatisfied) {
				cout << "Constraints satisfied" << endl;
			}
			else {
				cout << "Constraints not satisfied" << endl;
			}
		}

		return constraintsSatisfied;
	}

	gsl_vector* getResults() {
		return result;
	}

	double getObjectiveVal() {
		return objectiveVal;
	}

};

#if 0

class SnoptWrapper : public OptimizationWrapper {
	SnoptSolver* snoptSolver;
	integer n; integer neF; integer lenA;

	NumericalSolverHelper* ns;
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	map<int, int>& imap;
	map<string, int>& ctrlMap;
	set<int>& boolNodes;
	gsl_vector* minState;
	gsl_vector* t;
	gsl_vector* t1;

	int MAX_TRIES = PARAMS->numTries;
	int RANDOM_SEARCH = 10;
	double RANDOM_TARGET = 100;

	int MAX_CONSTRAINT_TRIES = 5;

	doublereal* x;
	doublereal* xlow;
	doublereal* xupp;

	doublereal* Flow;
	doublereal* Fupp;

	double minObjectiveVal;
	double threshold = 0.01;

	int MAX_TRIES_ONLY_INPUTS = 5;

	void getxranges() {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*)ctrls[i];
				xlow[idx] = cnode->hasRange ? cnode->low : -20.0;
				xupp[idx] = cnode->hasRange ? cnode->high : 20.0;
				counter++;
			}
		}
		for (; counter < n; counter++) {
			xlow[counter] = 0;
			xupp[counter] = 1;
		}
	}

	void getFranges() {
		Flow[0] = -1e20;
		Fupp[0] = 1e20;
		for (int i = 1; i < neF; i++) {
			Flow[i] = 0;
			Fupp[i] = 1e20;
		}
	}

public:
	SnoptWrapper(SymbolicEvaluator* eval_, BooleanDAG* dag_, map<int, int>& imap_, map<string, int>& ctrlMap_, set<int>& boolNodes_, int ncontrols_, int numConstraints_) : eval(eval_), dag(dag_), imap(imap_), ctrlMap(ctrlMap_), boolNodes(boolNodes_), n(ncontrols_) {
		SnoptEvaluator::init(n);
		if (PARAMS->useSnoptUnconstrained) {
			neF = 1;
		}
		else {
			neF = numConstraints_ + 1;
		}
		lenA = 10; // we don't use this currently

		cout << "nef: " << neF << endl;
		cout << "n: " << n << endl;
		snoptSolver = new SnoptSolver(n, neF, lenA);
		minState = gsl_vector_alloc(n);
		t = gsl_vector_alloc(n);
		t1 = gsl_vector_alloc(n);

		x = new doublereal[n];
		xlow = new doublereal[n];
		xupp = new doublereal[n];

		Flow = new doublereal[neF];
		Fupp = new doublereal[neF];

		getxranges();
		getFranges();
	}

	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) {
		Assert(neF > constraints.size(), "Increase neF");
		eval->setInputs(inputs);
		// start the snopt solving
		SnoptParameters* p = new SnoptParameters(eval, constraints, minimizeNode);
		snoptSolver->init((char *)p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);

		double betas[3] = { -1, -10, -50 };
		double alphas[3] = { 1, 10, 50 };

		gsl_vector_memcpy(t, initState);
		if (initRandomize) {
			randomizeCtrls(t, inputs, constraints, minimizeNode);
		}

		double obj;
		int numtries = 0;
		minObjectiveVal = 1e50;
		bool solved;
		while (minObjectiveVal > threshold && numtries < MAX_TRIES) {
			if (!suppressPrint) {
				cout << "Attempt: " << (numtries + 1) << endl;
			}

			for (int i = 0; i < 3; i++) {
				if (!suppressPrint) {
					cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
				}
				p->beta = betas[i];
				p->alpha = alphas[i];
				if (!suppressPrint) {
					for (int i = 0; i < n; i++) {
						cout << gsl_vector_get(t, i) << ", ";
					}
					cout << endl;
				}
				SnoptEvaluator::counter = 0;
				solved = snoptSolver->optimize(t, suppressPrint);
				obj = snoptSolver->getObjectiveVal();
				gsl_vector_memcpy(t, snoptSolver->getResults());
			}
			if (numtries == 0) {
				gsl_vector_memcpy(minState, snoptSolver->getResults());
			}
			if (solved && obj < minObjectiveVal) {
				gsl_vector_memcpy(minState, snoptSolver->getResults());
				minObjectiveVal = obj;
			}
			numtries++;
			if (minObjectiveVal > threshold && numtries < MAX_TRIES) {
				randomizeCtrls(t, inputs, constraints, minimizeNode);
			}
		}
		return minObjectiveVal < threshold;
	}

	virtual bool optimizeForInputs(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) {
		int numConstraintRetries = 0;
		bool solved = false;
		const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
		while (numConstraintRetries < MAX_CONSTRAINT_TRIES) {
			// select random subset of bool nodes if we don't have the budget for all constraints
			// TODO: this should be done in a cegis like loop - otherwise it is not sound
			vector<int> boolConstraints;
			const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
			for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
				bool_node* node = *node_it;
				if (boolNodes.find(node->id) != boolNodes.end()) {
					if (node->type == bool_node::ASSERT) {
						// do nothing
					}
					else if (node->getOtype() == OutType::BOOL) {
						auto it = nodeValsMap.find(node->id);
						if (it != nodeValsMap.end()) {
							boolConstraints.push_back(node->id);
						}
					}
				}
			}
			int numToRemove = 0;
			if (!PARAMS->useSnoptUnconstrained && boolConstraints.size() > neF - 1) {
				numToRemove = boolConstraints.size() - neF + 1;
			}
			cout << "Removing " << numToRemove << " constraints" << endl;
			while (numToRemove > 0) {
				int oldsize = boolConstraints.size();
				int randIdx = rand() % (boolConstraints.size());
				boolConstraints.erase(boolConstraints.begin() + randIdx);
				if (boolConstraints.size() != oldsize - 1) {
					cout << "Did not delete " << randIdx << " size " << oldsize << endl;
					Assert(false, "Dfqwerq");
				}
				numToRemove--;
			}
			//cout << "Selected constraints: ";
			//for (int i = 0; i < boolConstraints.size(); i++) {
			//    cout << boolConstraints[i] << ";";
			//}

			set<int> boolConstraintsSet(boolConstraints.begin(), boolConstraints.end());

			// start the snopt solving
			SnoptParameters* p = new SnoptParameters(eval, dag, allInputs, imap, boolConstraintsSet, true);
			snoptSolver->init((char *)p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);

			double betas[1] = { -1 };
			double alphas[1] = { 1 };

			gsl_vector_memcpy(t, initState);

			double obj;
			int numtries = 0;
			minObjectiveVal = 1e50;
			solved = false;
			while (!solved && numtries < MAX_TRIES_ONLY_INPUTS) {
				if (!suppressPrint) {
					cout << "Attempt: " << (numtries + 1) << endl;
				}

				for (int i = 0; i < 1; i++) {
					if (!suppressPrint) {
						cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
					}
					p->beta = betas[i];
					p->alpha = alphas[i];
					if (!suppressPrint) {
						for (int i = 0; i < n; i++) {
							cout << gsl_vector_get(t, i) << ", ";
						}
						cout << endl;
					}
					SnoptEvaluator::counter = 0;
					solved = snoptSolver->optimize(t, suppressPrint);
					obj = snoptSolver->getObjectiveVal();
					gsl_vector_memcpy(t, snoptSolver->getResults());
				}
				if (numtries == 0) {
					gsl_vector_memcpy(minState, snoptSolver->getResults());
				}
				if (solved) {
					gsl_vector_memcpy(minState, snoptSolver->getResults());
					if (obj < minObjectiveVal) {
						minObjectiveVal = obj;
					}
				}
				numtries++;
				if (!solved && numtries < MAX_TRIES_ONLY_INPUTS) {
					randomizeCtrls(t, allInputs);
				}
			}
			if (!solved) {
				return false;
			}
			else {
				// check if it is actually correct
				GradUtil::BETA = -50;
				GradUtil::ALPHA = 50;
				if (eval->checkAll(minState, nodeValsMap, true)) {
					return true;
				}
				solved = false;
				cout << "Failed on full constraints - trying again" << endl;
				numConstraintRetries++;
			}
		}
		//eval->print();
		//eval->printFull();
		return solved;
	}

	virtual gsl_vector* getMinState() {
		return minState;
	}

	virtual double getObjectiveVal() {
		return minObjectiveVal;
	}

	virtual void randomizeCtrls(gsl_vector* state, vector<vector<int>>& allInputs) {
		double best = GradUtil::MAXVAL;
		for (int i = 0; i < RANDOM_SEARCH; i++) {
			randomize(t1);
			cout << "Trying: ";
			for (int j = 0; j < t1->size; j++) {
				cout << gsl_vector_get(t1, j) << ", ";
			}
			map<int, int> nodeValsMap;
			if (allInputs.size() > 0) {
				nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
			}
			GradUtil::BETA = -1;
			GradUtil::ALPHA = 1;
			eval->run(t1, nodeValsMap);
			double error = 0.0;
			for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
				bool_node* node = *node_it;
				if (node->type == bool_node::ASSERT && ((ASSERT_node*)node)->isHard()) { // TODO: currenlty we are only using the minimizatin objective -- we should probably also use the other constraints
					if (eval->hasDist(node->mother)) {
						error = eval->getVal(node->mother);
					}
					break;
				}
			}
			cout << "Error: " << error << endl;
			if (error < best) {
				best = error;
				gsl_vector_memcpy(state, t1);
			}
		}
	}

	void randomize(gsl_vector* state) {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*)ctrls[i];
				double low = cnode->hasRange ? cnode->low : -20.0;
				double high = cnode->hasRange ? cnode->high : 20.0;
				double r = low + (rand() % (int)((high - low) * 10.0)) / 10.0;
				gsl_vector_set(state, idx, r);
				counter++;
			}
		}

		for (; counter < n; counter++) {
			double low = 0.0;
			double high = 1.0;
			double r = low + (rand() % (int)((high - low) * 10.0)) / 10.0;
			gsl_vector_set(state, counter, r);
		}

	}

};

#endif