#pragma once

class CEGISParams{
public:	
	int iterlimit;
	bool printDiag;
	int NINPUTS;
	int nseeds;	
	bool simulate;
	int simiters;
	int simstopsize;
	bool setMemo;
	typedef enum{ NOSIM /*no simplify*/, SIMSIM/*simple simplify*/, RECSIM/*recursive simplify*/} simtype;
	simtype simplifycex;
	bool superChecks;
	bool lightVerif;
	float sparseArray;	
	CEGISParams(CommandLineArgs& args):
		printDiag(false),
		nseeds(1),
		NINPUTS(3),
		iterlimit(-1),		
		simulate(true),
		simiters(3),		
		simplifycex(RECSIM),
		superChecks(false),
		setMemo(args.setMemo),
		lightVerif(args.lightVerif),
		sparseArray(args.sparseArray)
	{
		printDiag = args.printDiag;
		nseeds = args.seedsize;		
		NINPUTS = args.NINPUTS;
		if(args.terminateafter > 0){
			iterlimit = args.terminateafter;
		}		
		simulate = args.simulate;
		simiters = args.simiters;
		simstopsize = args.simstopsize;
		superChecks = args.superChecks;
		if(args.simplifycex == "NOSIM"){ simplifycex = NOSIM; }
		if(args.simplifycex == "SIMSIM"){ simplifycex = SIMSIM; }
		if(args.simplifycex == "RECSIM"){ simplifycex = RECSIM; }
	}

	void activatePrintDiag(){
		printDiag = true;
	}		
	void setIterLimit(int p_iterlimit){iterlimit = p_iterlimit; };

};
