#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

SEXP split_at(SEXP x, SEXP at, SEXP excl) {
    int up = 0, cuts = 0, i = 0, j, n = LENGTH(x), *cutp, exclude = (asInteger(excl) == 1) ? 1 : 0;
    SEXP res;
    if (TYPEOF(at) != INTSXP) {
	at = PROTECT(coerceVector(at, INTSXP));
	up++;
    }
    if (TYPEOF(x) != REALSXP &&
	TYPEOF(x) != INTSXP &&
	TYPEOF(x) != STRSXP) Rf_error("x must be numeric or character vector");
    cuts = LENGTH(at);
    cutp = INTEGER(at);
    res = PROTECT(allocVector(VECSXP, cuts + 1));
    while (i <= cuts) {
	SEXP v;
	int i0 = 0, i1 = n;
	if (i > 0) i0 = cutp[i - 1] + exclude - 1;
	if (i0 < 0)
	    Rf_error("cutpoint %d is not positive", i);
	if (i < cuts) i1 = cutp[i] - 1;
	if (i1 < 0)
	    Rf_error("cutpoint %d is not positive", i + 1);
	if (i1 < i0) i1 = i0;
	v = allocVector(TYPEOF(x), i1 - i0);
	SET_VECTOR_ELT(res, i, v);
	if (i1 - i0 > 0) {
	    if (TYPEOF(x) == REALSXP)
		memcpy(REAL(v), REAL(x) + i0, (i1 - i0) * sizeof(double));
	    else if (TYPEOF(x) == INTSXP)
		memcpy(INTEGER(v), INTEGER(x) + i0, (i1 - i0) * sizeof(int));
	    else for (j = 0; j < n; j++) SET_STRING_ELT(v, j, STRING_ELT(x, j + i0));
	}
	i++;
    }
    UNPROTECT(up + 1);
    return res;
}
