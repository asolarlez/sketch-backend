#include <utility>

//
// Created by Kliment Serafimov on 7/10/22.
//

#ifndef SKETCH_VARNAME_H
#define SKETCH_VARNAME_H


class VarName : public string {
public:
    VarName(string s): string(std::move(s)){}
    VarName(const char s[]): string(s){}
    VarName() = default;
};


#endif //SKETCH_VARNAME_H
