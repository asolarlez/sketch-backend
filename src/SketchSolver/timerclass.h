#ifndef TIMERCLASS_H_
#define TIMERCLASS_H_

#include <sys/time.h>

class timerclass{
	unsigned long long tottime;
	unsigned long long ctime;
	struct timeval stime, endtime;
	const string name;
	public:
	timerclass(){
		tottime = 0;	
	}
	timerclass(const string& pname):name(pname){
		tottime = 0;		
	}
	
	inline timerclass& start(){
		tottime = 0;
		gettimeofday(&stime, NULL);
		return *this;
	}
	inline timerclass& restart(){	
		gettimeofday(&stime, NULL);
		return *this;
	}
	inline timerclass& stop(){
		gettimeofday(&endtime, NULL);
		ctime =  1000000*(endtime.tv_sec - stime.tv_sec)+
					   (endtime.tv_usec - stime.tv_usec);		
		tottime += ctime;
		stime.tv_sec = endtime.tv_sec;
		stime.tv_usec = endtime.tv_usec;
		return *this;
	}
	inline double get_cur_ms(){
		return (ctime/1000.0);
	}
	inline double get_tot_ms(){
		return (tottime/1000.0);
	}
	inline timerclass& print(){
		cout<<name<<":  "<<(ctime/1000.0)<<" ms ";
		if( tottime != ctime) cout<<(tottime/1000.0)<<" ms";
		cout<<endl;
		return *this;
	}
	inline timerclass& print(const string& msg){
		cout<<name<<" "<<msg<<":  "<<(ctime/1000.0)<<" ms ";
		if( tottime != ctime) cout<<(tottime/1000.0)<<" ms";
		cout<<endl;
		return *this;
	}
};




#endif /*TIMERCLASS_H_*/
