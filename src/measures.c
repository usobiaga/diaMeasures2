#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>  /* required by R */
/* #include float.h */

#define STRCMPR(str1, i, str2, j) (strcmp(CHAR(STRING_ELT(str1, i)), CHAR(STRING_ELT(str2, j))))
#define MAX2(nbr1, nbr2) (nbr1 < nbr2 ? nbr2 : nbr1)

/* SEXP lv01(SEXP a, SEXP b, SEXP measure, SEXP weight, SEXP p, SEXP bt, */
/* 	  SEXP q, SEXP useBytes, SEXP nthread){ */

/*   SEXP result; */
/*   int i; */
  
/*   PROTECT(result = sd_stringdist(a, b, measure, weight, p, bt, q, useBytes, nthread)); */
  
/*   for (i=0; i<length(a); i++){ */
/*     REAL(result)[i] = REAL(result)[i] / (double)MAX2(length(VECTOR_ELT(a, i)), */
/* 						     length(VECTOR_ELT(b, i))); */
/*   } */
/*   return (result); */
/* } */


SEXP rdi(SEXP a, SEXP b){

  SEXP result;
  int n, i;

  n = length(a);
  PROTECT(result = allocVector(REALSXP, n));
  memset(REAL(result), 0.0, n * sizeof(double));

  for (i = 0; i<n; i++){
    if (STRCMPR(a, i, b, i) != 0) REAL(result)[i] = 1.0;
    Rprintf("val %d in rdi is: %f \n", i, REAL(result)[i] );
  }
  
  UNPROTECT(1);
  return result;
  
}

SEXP single_arg_R_fun(char* fun, SEXP x){
  SEXP s, t;
  t = s = PROTECT(allocList(2));
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install(fun)); t = CDR(t);
  SETCAR(t,  x);
  UNPROTECT(1);
  return eval(s, R_GlobalEnv);
}


void ipd_q(double *ans, double *nobs, SEXP qi, SEXP weight){

  SEXP unlisted, uans, tax, a, b;
  int n, i, j, z, ij, nuans, nqi;
  double aux;

  /* get the TAX_jk */

  PROTECT(unlisted = single_arg_R_fun("unlist", qi));
  PROTECT(uans = single_arg_R_fun("unique", unlisted));
  nuans = length(uans);
  PROTECT(tax = allocVector(REALSXP, nuans));
  memset(REAL(tax), 0.0, nuans * sizeof(double));
  nqi = length(unlisted);
  Rprintf("nqi is %d \n", nqi);
  Rprintf("qi has a length %d \n", length(qi));
  n = 0;
  
  for (i=0; i<nqi; i++){
    /* if (length(STRING_ELT(unlisted, i)) == 0) continue; */
    n++;
    for(j=0; j<nuans; j++){
      if (STRCMPR(unlisted, i, uans, j) == 0){
	REAL(tax)[j] += 1.0;
	break;
      }
    }
  }

  Rprintf("n is %d \n", n);
  ij = 0;
  for (i=0; i<length(qi); i++){
    for (j=0; j<i; j++){
      
      a = VECTOR_ELT(qi, i);
      b = VECTOR_ELT(qi, j);

  
      if (length(a) == 0 || length(b) == 0){
	ij++;
	continue;
      }
      
      if (STRCMPR(a, 0, b, 0) != 0){
	nobs[ij] += 1.0;
	Rprintf("nobs is %f \n", nobs[ij]);
	Rprintf("ans is %f \n", ans[ij]);
	ij++;
	continue;
      }
      
      for (z=0; z<nuans; z++){
	if (STRCMPR(a, 0, uans, z) == 0){
	  aux = 1.0 - (REAL(tax)[z] - 1.0) / (n*REAL(weight)[0]);
	  ans[ij] += aux;
	  nobs[ij] += aux;
	  Rprintf("nobs is %f \n", nobs[ij]);
	  Rprintf("ans is %f \n", ans[ij]);
	  ij++;
	  break;
	}
      }
    }
  }
  
      
  /* for(j=0; j<nuans; j++){ */
  /*   Rprintf("s in %s . value is %f \n", CHAR(STRING_ELT(uans, j)), REAL(tax)[j]); */
  /* } */
    
  Rprintf(" end  \n");
  /* tabulate */
  UNPROTECT(3);

}

double jaccard(double *dist, int na, int nb){

  int i, aux;
  double r;

  aux = 0;
  for (i=0; i<(na*nb); i++)
    if (dist[i] < 10e-7) aux++;
  
  /* Rprintf("a is %d, na is %d, nb is %d \n", a, na, nb); */
  r = (double)(na + nb - 2.0*aux) / (double)(na + nb - aux);
  /* Rprintf("r is %f \n", r); */
  return r;

}

double dice(double *dist, int na, int nb){

  int i, aux;
  double r;

  aux = 0;
  for (i=0; i<(na*nb); i++)
    if (dist[i] < 10e-7) aux++;

  r = (na + nb - 2.0 * aux) / (double)(na + nb);
  return r;
  
}

double cover(double *dist, int na, int nb){

  int i, j, z, isel, jsel, new;
  SEXP ina, inb;
  double result, cost, ijcost;

  PROTECT(ina = allocVector(INTSXP, na));
  PROTECT(inb = allocVector(INTSXP, nb));
  memset(INTEGER(ina), 0, na * sizeof(int));
  memset(INTEGER(inb), 0, nb * sizeof(int));
  result = 0.0;
  
  while(1){

    cost = DBL_MAX;
    
    for (i = 0; i < na; i++){
      for (j = 0; j < nb; j++){
	
	z = i*nb + j;
	new = 0;
	if ( (INTEGER(ina)[i] + INTEGER(inb)[j]) == 2) continue;
	if (INTEGER(ina)[i] == 0) new += 1;
	if (INTEGER(inb)[j] == 0) new += 1;
	ijcost = (dist[z] / (double)new);
	if (ijcost < cost){
	  cost = ijcost;
	  isel = i; jsel = j;
	}
      }
    }

    if (new == 0) break;

    Rprintf("isel is %d and jsel is %d \n", isel, jsel);
    Rprintf("cost is %f \n ", cost);

    result += dist[isel*nb + jsel];
    INTEGER(ina)[isel] = 1;
    INTEGER(inb)[jsel] = 1;

    Rprintf("value of new: %d \n", new);
    
  }
  
  UNPROTECT(2);
  return result / (double)MAX2(na, nb);
  
  
}

