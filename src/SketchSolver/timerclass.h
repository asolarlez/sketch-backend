#ifndef TIMERCLASS_H_
#define TIMERCLASS_H_




#ifndef  _MSC_VER
#include <sys/time.h>
#include <string>
#include <iostream>
#include <chrono>
using namespace std;

using namespace std::chrono;

class timerclass{
	unsigned long long tottime;
	unsigned long long ctime;
	struct timeval stime, endtime;
	const string name;
	public:
	timerclass(){
		tottime = 0;	
		ctime = 0;
	}

	void gettimeofdayme(timeval* tv, timeval* emp) {
		time_point<high_resolution_clock, milliseconds> msc= time_point_cast<milliseconds>(high_resolution_clock::now());
		tv->tv_sec = msc.time_since_epoch().count() / 1000;
		tv->tv_usec = msc.time_since_epoch().count() % 1000;
	}

	timerclass(const string& pname):name(pname){
		tottime = 0;	
		ctime = 0;
	}
	
	inline timerclass& start(){
		tottime = 0;		
		gettimeofdayme(&stime, NULL);
		return *this;
	}
	inline timerclass& restart(){	
		gettimeofdayme(&stime, NULL);
		return *this;
	}
	inline timerclass& stop(){
		gettimeofdayme(&endtime, NULL);
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

#else

#include <tchar.h>
#include <windows.h>
#include <string>
#include <iostream>

using namespace std;

class timerclass{
        unsigned long long tottime;
        unsigned long long ctime;
        unsigned long long units;
        unsigned long long  stime, endtime;
        const string name;
        public:
        timerclass(){
                tottime = 0;
				ctime = 0;
				stime=0;
				endtime=0;
				QueryPerformanceFrequency((LARGE_INTEGER *)&units);
        }
        timerclass(const string& pname):name(pname){
                tottime = 0;
				ctime=0;
				stime=0;
				endtime=0;
                QueryPerformanceFrequency((LARGE_INTEGER *)&units);
        }

        inline timerclass& start(){
                tottime = 0;
                QueryPerformanceCounter((LARGE_INTEGER *)&stime);
                return *this;
        }
        inline timerclass& restart(){
                QueryPerformanceCounter((LARGE_INTEGER *)&stime);
                return *this;
        }
        inline timerclass& stop(){
                QueryPerformanceCounter((LARGE_INTEGER *)&endtime);
                ctime =  ((endtime - stime)*1000000) / units;

                tottime += ctime;
                stime = endtime;
                return *this;
        }
        inline double get_cur_ms(){
                return (ctime/1000.0);
        }
        inline double get_tot_ms(){
                return (tottime/1000.0);
        }
        inline timerclass& print(){
                cout<<name<<":  "<<((ctime)/1000.0)<<" ms ";
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

#endif

#endif /*TIMERCLASS_H_*/
