/**************************************************************************************************

Global.h -- (C) Niklas Een, Niklas Sörensson, 2004

Contains types, macros, and inline functions generally useful in a C++ program. 

**************************************************************************************************/


#ifndef Global_h
#define Global_h

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <climits>
#include <cfloat>
#include <new>


//=================================================================================================
// Basic Types & Minor Things:


#ifdef _MSC_VER
typedef __int64     int64;
#define I64_fmt "I64d"
#else
typedef long long   int64;
#define I64_fmt "lld"
#endif
typedef const char  cchar;


template<class T> static inline T min(T x, T y) { return (x < y) ? x : y; }
template<class T> static inline T max(T x, T y) { return (x > y) ? x : y; }

template <bool> struct STATIC_ASSERTION_FAILURE;
template <> struct STATIC_ASSERTION_FAILURE<true>{};
#define TEMPLATE_FAIL STATIC_ASSERTION_FAILURE<false>()


//=================================================================================================
// 'malloc()'-style memory allocation -- never returns NULL; aborts instead:


template<class T> static inline T* xmalloc(size_t size) {
    T*   tmp = (T*)malloc(size * sizeof(T));
    assert(size == 0 || tmp != NULL);
    return tmp; }

template<class T> static inline T* xrealloc(T* ptr, size_t size) {
    T*   tmp = (T*)realloc((void*)ptr, size * sizeof(T));
    assert(size == 0 || tmp != NULL);
    return tmp; }

template<class T> static inline void xfree(T *ptr) {
    if (ptr != NULL) free((void*)ptr); }


//=================================================================================================
// Random numbers:


// Returns a random float 0 <= x < 1. Seed must never be 0.
static inline double drand(double& seed) {
    seed *= 1389796;
    int q = (int)(seed / 2147483647);
    seed -= (double)q * 2147483647;
    return seed / 2147483647; }

// Returns a random integer 0 <= x < size. Seed must never be 0.
static inline int irand(double& seed, int size) {
    return (int)(drand(seed) * size); }


//=================================================================================================
// Time:


#ifdef _MSC_VER
#include <ctime>
static inline double cpuTime(void) {
    return (double)clock() / CLOCKS_PER_SEC; }
#else
#include <sys/time.h>
#include <sys/resource.h>
static inline double cpuTime(void) {
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    return (double)ru.ru_utime.tv_sec + (double)ru.ru_utime.tv_usec / 1000000; }
#endif


//=================================================================================================
// 'vec' -- automatically resizable arrays (via 'push()' method):


// NOTE! Don't use this vector on datatypes that cannot be re-located in memory (with realloc)

template<class T>
class vec {
    T*  data;
    int sz;
    int cap;

    void     init(int size, const T& pad);
    void     grow(int min_cap);

public:
    // Types:
    typedef int Key;
    typedef T   Datum;

    // Constructors:
    vec(void)                   : data(NULL) , sz(0)   , cap(0)    { }
    vec(int size)               : data(NULL) , sz(0)   , cap(0)    { growTo(size); }
    vec(int size, const T& pad) : data(NULL) , sz(0)   , cap(0)    { growTo(size, pad); }
    vec(T* array, int size)     : data(array), sz(size), cap(size) { }      // (takes ownership of array -- will be deallocated with 'xfree()')
   ~vec(void)                                                      { clear(true); }

    // Ownership of underlying array:
    T*       release  (void)           { T* ret = data; data = NULL; sz = 0; cap = 0; return ret; }
    operator T*       (void)           { return data; }     // (unsafe but convenient)
    operator const T* (void) const     { return data; }

    // Size operations:
    int      size   (void) const       { return sz; }
    void     shrink (int nelems)       { assert(nelems <= sz); for (int i = 0; i < nelems; i++) sz--, data[sz].~T(); }
    void     shrink_ (int nelems)      { assert(nelems <= sz); sz -= nelems; }
    void     pop    (void)             { sz--, data[sz].~T(); }
    void     growTo (int size);
    void     growTo (int size, const T& pad);
    void     clear  (bool dealloc = false);
    void     clear_ (void)             { sz = 0; }
    void     capacity (int size) { grow(size); }

    // Stack interface:
    void     push  (void)              { if (sz == cap) grow(sz+1); new (&data[sz]) T()    ; sz++; }
    //void     push  (const T& elem)     { if (sz == cap) grow(sz+1); new (&data[sz]) T(elem); sz++; }
    void     push  (const T& elem)     { if (sz == cap) grow(sz+1); data[sz++] = elem; }
    void     push_ (const T& elem)     { data[sz++] = elem; }
    const T& last  (void) const        { return data[sz-1]; }
    T&       last  (void)              { return data[sz-1]; }

    // Vector interface:
    const T& operator [] (int index) const  { return data[index]; }
    T&       operator [] (int index)        { return data[index]; }

    // Don't allow copying (error prone):
    vec<T>&  operator = (vec<T>& other) { TEMPLATE_FAIL; }
             vec        (vec<T>& other) { TEMPLATE_FAIL; }

    // Duplicatation (preferred instead):
    void copyTo(vec<T>& copy) const { copy.clear(); copy.growTo(sz); for (int i = 0; i < sz; i++) new (&copy[i]) T(data[i]); }
    void moveTo(vec<T>& dest) { dest.clear(true); dest.data = data; dest.sz = sz; dest.cap = cap; data = NULL; sz = 0; cap = 0; }
};

template<class T>
void vec<T>::grow(int min_cap) {
    if (min_cap <= cap) return;
    if (cap == 0) cap = (min_cap >= 16) ? min_cap : 16;
    else          do cap = cap*2 + 16; while (cap < min_cap);
    data = xrealloc(data, cap); }

template<class T>
void vec<T>::growTo(int size, const T& pad) {
    if (sz >= size) return;
    grow(size);
    for (int i = sz; i < size; i++) new (&data[i]) T(pad);
    sz = size; }

template<class T>
void vec<T>::growTo(int size) {
    if (sz >= size) return;
    grow(size);
    for (int i = sz; i < size; i++) new (&data[i]) T();
    sz = size; }

template<class T>
void vec<T>::clear(bool dealloc) {
    if (data != NULL){
        for (int i = 0; i < sz; i++) data[i].~T();
        sz = 0;
        if (dealloc) xfree(data), data = NULL, cap = 0; } }


//=================================================================================================
// Lifted booleans:


class lbool {
    int     value;
    explicit lbool(int v) : value(v) { }

public:
    lbool()       : value(0) { }
    lbool(bool x) : value((int)x*2-1) { }
    int toInt(void) const { return value; }

    bool  operator == (const lbool& other) const { return value == other.value; }
    bool  operator != (const lbool& other) const { return value != other.value; }
    lbool operator ~  (void)               const { return lbool(-value); }

    friend int   toInt  (lbool l);
    friend lbool toLbool(int   v);
};
inline int   toInt  (lbool l) { return l.toInt(); }
inline lbool toLbool(int   v) { return lbool(v);  }

const lbool l_True  = toLbool( 1);
const lbool l_False = toLbool(-1);
const lbool l_Undef = toLbool( 0);


//=================================================================================================
// Relation operators -- extend definitions from '==' and '<'


#ifndef __SGI_STL_INTERNAL_RELOPS   // (be aware of SGI's STL implementation...)
#define __SGI_STL_INTERNAL_RELOPS
template <class T> static inline bool operator != (const T& x, const T& y) { return !(x == y); }
template <class T> static inline bool operator >  (const T& x, const T& y) { return y < x;     }
template <class T> static inline bool operator <= (const T& x, const T& y) { return !(y < x);  }
template <class T> static inline bool operator >= (const T& x, const T& y) { return !(x < y);  }
#endif


//=================================================================================================
#endif
