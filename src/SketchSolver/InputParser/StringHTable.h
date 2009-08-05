#pragma once

#include <map>

using namespace std;

template<typename T>
class StringHTable
{
	map<string, T> table;
	mutable T lastNode;
	mutable string last;
public:
	StringHTable(void){}
	~StringHTable(void){}
	void add(const char* key, T val){
		last = key;
		table[last] = val;
		lastNode = val;
	}

	bool get(const char* key,T& out)const{
		string t(key);
		if(t==last){
			out= lastNode;
			return true;
		}
		map<string, T>::const_iterator it = table.find(t);
		if(it != table.end()){
			swap(last, t);
			lastNode = it->second;
			out = lastNode;
			return true;
		}else{
			return false;
		}
	}

	bool condAdd(const char* key,T val, T& out){
		
			string t(key);
			if(t==last){
				out = lastNode;
				return true;
			}
			map<string, T>::iterator it = table.find(t);
			swap(last, t);
		
		if(it != table.end()){
			lastNode = it->second;
			out = lastNode;
			return true;
		}else{
			lastNode = val;
			out = lastNode;
			table.insert(it, make_pair(last, val));
			return false;
		}
	}

	void clear(){
		table.clear();
		last = "";
	}
};
