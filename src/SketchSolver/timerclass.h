#ifndef TIMERCLASS_H_
#define TIMERCLASS_H_

#include <sys/time.h>

class timerclass{
	unsigned long long tottime;
	struct timeval stime, endtime;
	const string name;
	public:
	timerclass(){
		tottime = 0;	
	}
	timerclass(const string& pname):name(pname){
		tottime = 0;		
	}
	
	timerclass& start(){
		tottime = 0;
		gettimeofday(&stime, NULL);
		return *this;
	}
	timerclass& restart(){	
		gettimeofday(&stime, NULL);
		return *this;
	}
	timerclass& stop(){
		gettimeofday(&endtime, NULL);
		tottime += 1000000*(endtime.tv_sec - stime.tv_sec)+
					   (endtime.tv_usec - stime.tv_usec);
		stime.tv_sec = endtime.tv_sec;
		stime.tv_usec = endtime.tv_usec;
		return *this;
	}
	
	timerclass& print(){
		cout<<name<<":  "<<(tottime/1000.0)<<" millisecs"<<endl;
		return *this;
	}	
};




#endif /*TIMERCLASS_H_*/
