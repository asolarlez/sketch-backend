#ifndef BasicError_h
#define BasicError_h

#include <string>
#include <iostream>
#include <sstream>

using std::string;

extern string context;

class BasicError
{
public:
  BasicError(const string& ermsg, const string& name){
    std::cerr<<name<<": "<<ermsg<<std::endl;
    std::cerr<<"                          "<<context<<std::endl;
  }
  virtual ~BasicError(){};

};


inline void Warning(const string& msg){
  std::cerr<<"WARNING:  "<<msg<<std::endl;
}

inline void Assert(bool b, const string& msg){
	if(!b){
		throw BasicError(msg, "Error");
	}

}

template<class T>
inline void Assert(bool b, const string& msg, T& msg2){
	if(!b){
    std::stringstream st;
    st<<msg<<" "<<msg2;
		throw BasicError(st.str(), "Error");
	}
}

template<class T1, class T2>
inline void Assert(bool b, const string& msg, T1& msg2, T2& msg3){
	if(!b){
    std::stringstream st;
    st<<msg<<" "<<msg2<<", "<<msg3;
		throw BasicError(st.str(), "Error");
	}
}
#endif

