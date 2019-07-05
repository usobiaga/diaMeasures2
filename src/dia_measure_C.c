#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>  /* required by R */
#include "dia_measure.h"
#include "measures.h"

SEXP dia_measure_C(SEXP ql, SEXP measure, SEXP binary, SEXP weight, SEXP q, SEXP p,
		   SEXP bt, SEXP useBytes, SEXP nthread, SEXP variableDist){
  SEXP dist, nobs, dist_li, l_i;
  int i, j, z, nl, nq, imeasure, ibinary, ivar_dist, n;
  R_xlen_t nans;

  nq = length(ql);
  nl = length(VECTOR_ELT(ql, 0));
  ivar_dist = LOGICAL(variableDist)[0];
  nans = ivar_dist ? (nq*(nq-1)/2) : (nl*(nl-1)/2);
  
  PROTECT(dist = allocVector(REALSXP, nans));
  PROTECT(nobs = allocVector(REALSXP, nans));
  memset(REAL(dist), 0.0, nans * sizeof(double));
  memset(REAL(nobs), 0.0, nans * sizeof(double));

  imeasure = asInteger(measure);
  ibinary = asInteger(binary);

  if (ivar_dist){ /* if distance between variables is needed */
      PROTECT(l_i = allocVector(VECSXP, 1));
      PROTECT(dist_li = allocVector(VECSXP, nq));

      for (i=0; i < nq; i++){
	SET_VECTOR_ELT(l_i, 0, VECTOR_ELT(ql, i));
	SET_VECTOR_ELT(dist_li, i, dia_measure_C(l_i, measure, binary, weight, q, p, bt,
						 useBytes, nthread, ScalarLogical(0)));
      }
      n = 0;
      for (i=0; i < nq; i++){
	for(j=0; j < i; j++){
	  
	  for(z=0; z < length(VECTOR_ELT(dist_li, 0)); z++){
	    if (ISNAN(REAL(VECTOR_ELT(dist_li, i))[z])) continue;
	    if (ISNAN(REAL(VECTOR_ELT(dist_li, j))[z])) continue;
	    REAL(nobs)[n] += 1.0;
	    REAL(dist)[n] += fabs(REAL(VECTOR_ELT(dist_li, i))[z] - REAL(VECTOR_ELT(dist_li, j))[z]); 
	  }
	  n++;
	}
      }
      UNPROTECT(2);
  } else {
    for (i=0; i<nq; i++){
      do_call_measures(REAL(dist), REAL(nobs), ibinary, VECTOR_ELT(ql, i), imeasure, 
		       weight, q, p, bt, useBytes, nthread); 
    }
  }

  for (i=0; i<nans; i++){
    REAL(dist)[i] = REAL(dist)[i] / REAL(nobs)[i];
  }
  
  UNPROTECT(2);
  return dist;
}



void do_call_measures(double *ans, double *nobs, int binary_index, SEXP qi, int measure, SEXP weight,
		      SEXP q, SEXP p, SEXP bt, SEXP useBytes, SEXP nthread){
  switch(measure){

  case IPD:
    ipd_q(ans, nobs, qi, weight);
    break;

  default:
    call_strdist(ans, nobs, binary_index, qi, measure, weight, q, p, bt, useBytes, nthread);
    break;
    
  };
  
}
  
void call_strdist(double *ans, double *nobs, int binary_index, SEXP qi, int measure, SEXP weight, SEXP q,
		  SEXP p, SEXP bt, SEXP useBytes, SEXP nthread){
  SEXP a, b;
  int nl, j, z, jz;
  
  nl = length(qi);
  jz = 0;
  
  for (j=0; j<nl; j++){
    for (z=0; z<j; z++){
  
      a = VECTOR_ELT(qi, j);
      b = VECTOR_ELT(qi, z);
  
      if (length(a) == 0 || length(b) == 0) {
  
	jz++;
	continue;

      }
      
      nobs[jz] += 1.0; 
      
      if (length(a) == 1 && length(b) == 1){  /* simple case */
    
	ans[jz] += REAL(measure_select(a, b, ScalarInteger(measure), weight, p, 
				       bt, q, useBytes, nthread))[0]; 
    
      } else { /* multiple answer */
    
	ans[jz] += REAL(do_binary(a, b, measure, binary_index, weight, p, bt, q,
				  useBytes, nthread))[0];  
    
      }
      jz++;
  
    }
  }
  
}


SEXP do_binary(SEXP a, SEXP b, int measure, int binary_index, SEXP weight, SEXP q, SEXP p,
	       SEXP bt, SEXP useBytes, SEXP nthread){
  
  SEXP a2, b2, dist, binr;
  int na, nb, i, j, ij;
  R_xlen_t nab;
  
  na = length(a);
  nb = length(b);
  nab = na * nb;
  ij = 0;

  PROTECT(a2 = allocVector(STRSXP, nab));
  PROTECT(b2 = allocVector(STRSXP, nab));
  /* Rprintf("IN BINARY \n"); */
  for (i=0; i<na; i++){
    for(j=0; j<nb; j++){
      SET_STRING_ELT(a2, ij, STRING_ELT(a, i));
      SET_STRING_ELT(b2, ij, STRING_ELT(b, j));
      ij++;
    }}
  
  
  PROTECT(dist = measure_select(a2, b2, ScalarInteger(measure), weight, p, bt, q, useBytes, nthread));
  PROTECT(binr = allocVector(REALSXP, 1));
  
  switch(binary_index){
    
  case JAC: 
    REAL(binr)[0] = jaccard(REAL(dist), na, nb); 
    break;
    
  case DICE:
    REAL(binr)[0] = dice(REAL(dist), na, nb); 
    break;

  case COVER:
    REAL(binr)[0] = cover(REAL(dist), na, nb); 
      
  }

  UNPROTECT(4);
  return binr;
    
}

SEXP measure_select(SEXP a, SEXP b, SEXP measure, SEXP weight, SEXP p, SEXP bt,
		    SEXP q, SEXP useBytes, SEXP nthread){
  SEXP result;
  switch(asInteger(measure)){
    
    /* from stringdist*/
  case OSA:
  case LV:
  case DL:
  case HAMMING:
  case LCS:
    
    PROTECT(result = sd_stringdist(a, b, measure, weight, p, bt, q, useBytes, nthread));
    break;
    
  case RDI:
    PROTECT(result = rdi(a, b));
    break;
  };
  
  UNPROTECT(1);
  return result;
  
}
