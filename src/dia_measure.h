#ifndef dm_measures_h
#define dm_measures_h

/* from stringdists */
/* • 0: Optimal String Alignment ( "osa" ) */
/* • 1: Levenshtein ( "lv" ) */
/* • 2: Damerau-Levenshtein ( "dl" ) */
/* • 3: Hamming ( "hamming" ) */
/* • 4: Longest Common Substring ( "lcs" ) */
/* • 5: q-gram ( "qgram" ) */
/* • 6: cosine ( "cosine" ) */
/* • 7: Jaccard ( "jaccard" ) */
/* • 8: Jaro-Winkler ( "jw" ) */
/* • 9: Soundex ( "soundex" */

#define OSA 0	       
#define LV 1
#define DL 2
#define HAMMING 3
#define LCS 4
#define RDI 30
#define IPD 31

#define JAC 1
#define DICE 2
#define COVER 3

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>  /* required by R */
#include <stringdist_api.h>

/* computets linguistical distance between linguistic identities */
SEXP dia_measure_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* switch driver calling the requested measure function */
void do_call_measures(double*, double*, int, SEXP, int, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

void call_strdist(double*, double*, int, SEXP, int, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* applies binary measure */
SEXP do_binary(SEXP, SEXP, int, int, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP measure_select(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);


#endif
