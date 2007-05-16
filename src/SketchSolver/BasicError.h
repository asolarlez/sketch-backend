#ifndef BasicError_h
#define BasicError_h

#include <string>
#include <iostream>
#include <sstream>


#define Dout( out )     /* out */ 


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

#define Assert( in, msg) if(!(in)){ stringstream mstr; mstr<<msg;  throw BasicError(mstr.str(), "Error"); }

#endif

