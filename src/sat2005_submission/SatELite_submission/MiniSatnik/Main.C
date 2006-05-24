/**************************************************************************************************

Main.C -- (C) Niklas Een, Niklas Sörensson, 2004

Read a DIMACS file and apply the SAT-solver to it.

**************************************************************************************************/


#include "Solver.h"
#include <ctime>
#include <unistd.h>
#include <signal.h>
#include <zlib.h>


//=================================================================================================
// Helpers:


// Reads an input stream to end-of-file and returns the result as a 'char*' terminated by '\0'
// (dynamic allocation in case 'in' is standard input).
//
char* readFile(gzFile in)
{
    char*   data = xmalloc<char>(65536);
    int     cap  = 65536;
    int     size = 0;

    while (!gzeof(in)){
        if (size == cap){
            cap *= 2;
            data = xrealloc(data, cap); }
        size += gzread(in, &data[size], 65536);
    }
    data = xrealloc(data, size+1);
    data[size] = '\0';

    return data;
}


//=================================================================================================
// BCNF Parser:


#define CHUNK_LIMIT 1048576

static bool parse_BCNF(cchar* filename, Solver& S)
{
    FILE*   in = fopen(filename, "rb");
    if (in == NULL) fprintf(stderr, "ERROR! Could not open file: %s\n", filename), exit(1);

    char    header[16];
    fread(header, 1, 16, in);
    if (strncmp(header, "BCNF", 4) != 0) fprintf(stderr, "ERROR! Not a BCNF file: %s\n", filename), exit(1);
    if (*(int*)(header+4) != 0x01020304) fprintf(stderr, "ERROR! BCNF file in unsupported byte-order: %s\n", filename), exit(1);

    int      n_vars    = *(int*)(header+ 8);
    //int    n_clauses = *(int*)(header+12);
    int*     buf       = xmalloc<int>(CHUNK_LIMIT);
    int      buf_sz;
    vec<Lit> c;

    for (int i = 0; i < n_vars; i++) S.newVar();

    for(;;){
        int n = fread(&buf_sz, 4, 1, in);
        if (n != 1) break;
        assert(buf_sz <= CHUNK_LIMIT);
        fread(buf, 4, buf_sz, in);

        int* p = buf;
        while (*p != -1){
            int size = *p++;
            c.clear();
            c.growTo(size);
            for (int i = 0; i < size; i++)
                c[i] = toLit(p[i]);
            p += size;

            S.addClause(c);     // Add clause.
            if (!S.okay()){
                xfree(buf); fclose(in);
                return false; }
        }
    }

    xfree(buf);
    fclose(in);

    S.simplifyDB();
    return S.okay();
}


//=================================================================================================
// DIMACS Parser:


static void skipWhitespace(char*& in) {
    while ((*in >= 9 && *in <= 13) || *in == 32)
        in++; }

static void skipLine(char*& in) {
    for (;;){
        if (*in == 0) return;
        if (*in == '\n') { in++; return; }
        in++; } }

static int parseInt(char*& in) {
    int     val = 0;
    bool    neg = false;
    skipWhitespace(in);
    if      (*in == '-') neg = true, in++;
    else if (*in == '+') in++;
    if (*in < '0' || *in > '9') fprintf(stderr, "PARSE ERROR! Unexpected char: %c\n", *in), exit(1);
    while (*in >= '0' && *in <= '9')
        val = val*10 + (*in - '0'),
        in++;
    return neg ? -val : val; }

static void readClause(char*& in, Solver& S, vec<Lit>& lits) {
    int     parsed_lit, var;
    lits.clear();
    for (;;){
        parsed_lit = parseInt(in);
        if (parsed_lit == 0) break;
        var = abs(parsed_lit)-1;
        while (var >= S.nVars()) S.newVar();
        lits.push( (parsed_lit > 0) ? Lit(var) : ~Lit(var) );
    }
}

static bool parse_DIMACS_main(char* in, Solver& S) {
    vec<Lit>    lits;
    for (;;){
        skipWhitespace(in);
        if (*in == 0)
            break;
        else if (*in == 'c' || *in == 'p')
            skipLine(in);
        else{
            readClause(in, S, lits);
            S.addClause(lits);
            if (!S.okay())
                return false;
        }
    }
    S.simplifyDB();
    return S.okay();
}


// Inserts problem into solver. Returns FALSE upon immediate conflict.
//
bool parse_DIMACS(gzFile in, Solver& S) {
    char* text = readFile(in);
    bool ret = parse_DIMACS_main(text, S);
    free(text);
    return ret; }


//=================================================================================================


void printStats(SolverStats& stats, double cpu_time)
{
    reportf("restarts              : %"I64_fmt"\n", stats.starts);
    reportf("conflicts             : %-12"I64_fmt"   (%.0f /sec)\n", stats.conflicts   , stats.conflicts   /cpu_time);
    reportf("decisions             : %-12"I64_fmt"   (%.0f /sec)\n", stats.decisions   , stats.decisions   /cpu_time);
    reportf("propagations          : %-12"I64_fmt"   (%.0f /sec)\n", stats.propagations, stats.propagations/cpu_time);
    reportf("inspects              : %-12"I64_fmt"   (%.0f /sec)\n", stats.inspects    , stats.inspects    /cpu_time);
    reportf("conflict literals     : %-12"I64_fmt"   (%4.2f %% deleted)\n", stats.tot_literals,
           (stats.max_literals - stats.tot_literals)*100 / (double)stats.max_literals);
    reportf("CPU time              : %g s\n", cpu_time);
}

Solver* solver;
static void SIGINT_handler(int signum) {
    printStats(solver->stats, cpuTime());
    reportf("\n");
    reportf("INTERRUPTED\n");
    exit(0); }


//=================================================================================================


int main(int argc, char** argv)
{
    Solver      S;
    bool        st;

    if (argc >= 2 && strlen(argv[1]) >= 5 && strcmp(&argv[1][strlen(argv[1])-5], ".bcnf") == 0)
        st = parse_BCNF(argv[1], S);
    else{
        gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");
        if (in == NULL)
            fprintf(stderr, "ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]),
            exit(1);
        st = parse_DIMACS(in, S);
        gzclose(in);
    }
    FILE* res = (argc >= 3) ? fopen(argv[2], "wb") : NULL;

    if (!st){
        if (res != NULL) fprintf(res, "UNSAT\n"), fclose(res);
        reportf("Trivial problem\n");
        reportf("UNSATISFIABLE\n");
        exit(20);
    }

    S.verbosity = 1;
    solver = &S;
    signal(SIGINT,SIGINT_handler);
    signal(SIGHUP,SIGINT_handler);
    st = S.solve();
    printStats(S.stats, cpuTime());
    reportf("\n");
    reportf(st ? "SATISFIABLE\n" : "UNSATISFIABLE\n");

    if (res != NULL){
        if (st){
            fprintf(res, "SAT\n");
            for (int i = 0; i < S.nVars(); i++)
                if (S.model[i] != l_Undef)
                    fprintf(res, "%s%s%d", (i==0)?"":" ", (S.model[i]==l_True)?"":"-", i+1);
            fprintf(res, " 0\n");
        }else
            fprintf(res, "UNSAT\n");
        fclose(res);
    }

    exit(st ? 10 : 20);     // (faster than "return", which will invoke the destructor for 'Solver')
}
