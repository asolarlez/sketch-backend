#ifndef BasicError_h
#define BasicError_h

#include <stdio.h> // sprintf
#include <string>
#include <iostream>
#include <sstream>


#define Dout( out )     /* out */


using std::string;
using std::stringstream;

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

#define Assert( in, msg) if(!(in)){ stringstream mstr; mstr<<msg;  throw BasicError(mstr.str(), "Error"); }



inline string int2str(int i){
	char tmp[30];
	Assert( i < 999999999, "Went out of bounds");
	sprintf(tmp, "%d", i);
	return string(tmp);
}

#endif

