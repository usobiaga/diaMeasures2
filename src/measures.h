#ifndef measures_h
#define measures_h


#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>  /* required by R */


SEXP rdi(SEXP, SEXP);

void ipd_q(double*, double*, SEXP, SEXP);

/* same as jaccard for binary measures */

double jaccard(double*, int, int);
double dice(double*, int, int);
double cover(double*, int, int);



#endif
