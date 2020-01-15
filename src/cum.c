#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <math.h>
#include <stdint.h>
#include <string.h>
#include "edges.h"
#include <Rdefines.h>
#include <Rinternals.h>
#include <immintrin.h>
#include "nifty.h"

SEXP na_cumsum(SEXP x, SEXP locf)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP ans;
	int locfp = (*INTEGER_RO(locf) > 0) + (*INTEGER_RO(locf) > 1);

	switch (anstyp) {
	case LGLSXP:
		anstyp = INTSXP;
	case REALSXP:
	case CPLXSXP:
		break;
	case NILSXP:
		return x;
	case INTSXP:
		if (LIKELY(!isFactor(x))) {
			break;
		}
	default:
		error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
		return R_NilValue;
	}
	if (UNLIKELY(!(n = XLENGTH(x)))) {
		return x;
	}
	PROTECT(ans = allocVector(anstyp, n));
	setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	UNPROTECT(1);

	switch (anstyp) {
	case INTSXP: {
		const int *xp = INTEGER_RO(x);
		int *ansp = INTEGER(ans);
		double sum = 0.;
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && xp[i] == NA_INTEGER; i++) {
				ansp[i] = NA_INTEGER;
			}
		}
		for (; i < n; i++) {
			sum += xp[i] != NA_INTEGER ? xp[i] : 0;
			if (UNLIKELY(sum > INT_MAX || sum < 1 + INT_MIN)) {
				/* INT_MIN is NA_INTEGER */
				warning("integer overflow in 'na.cumsum'; use 'na.cumsum(as.numeric(.))'");
				break;
			}
			ansp[i] = xp[i] != NA_INTEGER || locfp
				? (int)sum : NA_INTEGER;
		}
		break;		
	}
	case REALSXP: {
		const double *xp = REAL_RO(x);
		double *ansp = REAL(ans);
		double sum = 0.;
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && R_IsNA(xp[i]); i++) {
				ansp[i] = NA_REAL;
			}
		}
		for (; i < n; i++) {
			sum += !R_IsNA(xp[i]) ? xp[i] : 0.;
			ansp[i] = !R_IsNA(xp[i]) || locfp ? sum : NA_REAL;
		}
		break;
	}	
	case CPLXSXP: {
		const Rcomplex *xp = COMPLEX_RO(x);
		Rcomplex *ansp = COMPLEX(ans);
		Rcomplex sum = {.r = 0., .i = 0.};
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && R_IsNA(xp[i].r); i++) {
				ansp[i] = xp[i];
			}
		}
		for (; i < n; i++) {
			if (LIKELY(!R_IsNA(xp[i].r) && !R_IsNA(xp[i].i))) {
				sum.r += xp[i].r;
				sum.i += xp[i].i;
			}
			ansp[i].r = !R_IsNA(xp[i].r) || locfp ? sum.r : NA_REAL;
			ansp[i].i = !R_IsNA(xp[i].i) || locfp ? sum.i : NA_REAL;
		}
		break;
	}
	}
	return ans;
}

SEXP na_cumprod(SEXP x, SEXP locf)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP ans;
	int locfp = (*INTEGER_RO(locf) > 0) + (*INTEGER_RO(locf) > 1);

	switch (anstyp) {
	case LGLSXP:
	case REALSXP:
	case CPLXSXP:
		break;
	case NILSXP:
		return x;
	case INTSXP:
		if (LIKELY(!isFactor(x))) {
			anstyp = REALSXP;
			break;
		}
	default:
		error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
		return R_NilValue;
	}
	if (UNLIKELY(!(n = XLENGTH(x)))) {
		return x;
	}
	PROTECT(ans = allocVector(anstyp, n));
	setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	UNPROTECT(1);

	switch (TYPEOF(x)) {
	case LGLSXP: {
		const int *xp = LOGICAL_RO(x);
		int *ansp = LOGICAL(ans);
		int prod = 1;
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && xp[i] == NA_LOGICAL; i++) {
				ansp[i] = NA_LOGICAL;
			}
		}
		for (; i < n; i++) {
			prod = xp[i] ? prod : 0;
			ansp[i] = xp[i] != NA_LOGICAL || locfp ? prod : NA_LOGICAL;
		}
		break;
	}
	case INTSXP: {
		const int *xp = INTEGER_RO(x);
		double *ansp = REAL(ans);
		double prod = 1.;
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && xp[i] == NA_INTEGER; i++) {
				ansp[i] = NA_REAL;
			}
		}
		for (; i < n; i++) {
			if (LIKELY(xp[i] != NA_INTEGER)) {
				prod *= (double)xp[i];
				goto assI;
			} else if (locfp) {
			assI:
				ansp[i] = prod;
			} else {
				ansp[i] = NA_REAL;
			}
		}
		break;
	}
	case REALSXP: {
		const double *xp = REAL_RO(x);
		double *ansp = REAL(ans);
		double prod = 1.;
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (; i < n && R_IsNA(xp[i]); i++) {
				ansp[i] = NA_REAL;
			}
		}
		for (; i < n; i++) {
			if (LIKELY(!R_IsNA(xp[i]))) {
				prod *= xp[i];
				goto assR;
			} else if (locfp) {
			assR:
				ansp[i] = prod;
			} else {
				ansp[i] = NA_REAL;
			}
		}
		break;
	}	
	case CPLXSXP: {
		Rcomplex *xp = COMPLEX(x);
		Rcomplex *ansp = COMPLEX(x);
		Rcomplex prod = {.r = 1., .i = 0.};
		R_xlen_t i = 0;

		if (locfp == 1) {
			for (i = 0; i < n && R_IsNA(xp[i].r); i++) {
				ansp[i] = xp[i];
			}
		}
		for (; i < n; i++) {
			if (LIKELY(!R_IsNA(xp[i].r))) {
				double tmp = prod.r;

				prod.r = xp[i].r * prod.r - xp[i].i * prod.i;
				prod.i = xp[i].r * prod.i + xp[i].i * tmp;
				goto assC;
			} else if (locfp) {
			assC:
				ansp[i] = prod;
			} else {
				ansp[i].r = NA_REAL;
				ansp[i].i = NA_REAL;
			}
		}
		break;
	}
	}
	return ans;
}

SEXP na_cummax(SEXP x, SEXP locf)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP ans;
	int locfp = (*INTEGER_RO(locf) > 0) + (*INTEGER_RO(locf) > 1);

	switch (anstyp) {
	case LGLSXP:
	case REALSXP:
		break;
	case NILSXP:
		return x;
	case STRSXP:
	case INTSXP:
		if (LIKELY(!isFactor(x))) {
			break;
		}
	default:
		error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
		return R_NilValue;
	}
	if (UNLIKELY(!(n = XLENGTH(x)))) {
		return x;
	}
	PROTECT(ans = allocVector(anstyp, n));
	setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	UNPROTECT(1);

	switch (anstyp) {
	case LGLSXP:
	case INTSXP: {
		const int *xp = INTEGER_RO(x);
		int *ansp = INTEGER(ans);
		R_xlen_t i = locfp <= 1;
		int max = xp[0];

		if (locfp > 1) {
			for (; i < n && xp[i] == NA_INTEGER; i++);
			if (i < n) {
				max = xp[i];
			}
		}
		for (R_xlen_t j = 0; j < i; j++) {
			ansp[j] = max;
		}
		for (; i < n; i++) {
			max = max > xp[i] ? max : xp[i];
			ansp[i] = xp[i] != NA_INTEGER || locfp ? max : NA_INTEGER;
		}
		break;
	}
	case REALSXP: {
		const double *xp = REAL_RO(x);
		double *ansp = REAL(ans);
		R_xlen_t i = locfp <= 1;
		double max = xp[0];

		if (locfp > 1) {
			for (; i < n && R_IsNA(xp[i]); i++);
			if (i < n) {
				max = xp[i];
			}
		}
		for (R_xlen_t j = 0; j < i; j++) {
			ansp[j] = max;
		}
		for (; i < n; i++) {
			max = max > xp[i] || R_IsNA(xp[i]) ? max : xp[i];
			ansp[i] = !R_IsNA(xp[i]) || locfp ? max : NA_REAL;
		}
		break;
	}
	}
	return ans;
}


SEXP na_cummin(SEXP x, SEXP locf)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP ans;
	int locfp = (*INTEGER_RO(locf) > 0) + (*INTEGER_RO(locf) > 1);

	switch (anstyp) {
	case LGLSXP:
	case REALSXP:
		break;
	case NILSXP:
		return x;
	case STRSXP:
	case INTSXP:
		if (LIKELY(!isFactor(x))) {
			break;
		}
	default:
		error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
		return R_NilValue;
	}
	if (UNLIKELY(!(n = XLENGTH(x)))) {
		return x;
	}
	PROTECT(ans = allocVector(anstyp, n));
	setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	UNPROTECT(1);

	switch (anstyp) {
	case LGLSXP:
	case INTSXP: {
		const int *xp = INTEGER_RO(x);
		int *ansp = INTEGER(ans);
		R_xlen_t i = locfp <= 1;
		/* if xp[0] is NA this will overflow and be big */
		int min = xp[0] - 1;

		if (locfp > 1) {
			for (; i < n && xp[i] == NA_INTEGER; i++);
			if (i < n) {
				min = xp[i] - 1;
			}
		}
		for (R_xlen_t j = 0; j < i; j++) {
			ansp[j] = min + 1;
		}
		for (; i < n; i++) {
			min = min < xp[i] - 1 || xp[i] == NA_INTEGER ? min : xp[i] - 1;
			ansp[i] = xp[i] != NA_INTEGER || locfp ? min + 1 : NA_INTEGER;
		}
		break;
	}
	case REALSXP: {
		const double *xp = REAL_RO(x);
		double *ansp = REAL(ans);
		R_xlen_t i = locfp <= 1;
		double min = xp[0];

		if (locfp > 1) {
			for (; i < n && R_IsNA(xp[i]); i++);
			if (i < n) {
				min = xp[i];
			}
		}
		for (R_xlen_t j = 0; j < i; j++) {
			ansp[j] = min;
		}
		for (; i < n; i++) {
			min = min < xp[i] || R_IsNA(xp[i]) ? min : xp[i];
			ansp[i] = !R_IsNA(xp[i]) || locfp ? min : NA_REAL;
		}
		break;
	}
	}
	return ans;
}
