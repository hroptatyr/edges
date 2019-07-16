#include <math.h>
#include <stdint.h>
#include <string.h>
#include "myR.h"
#include <Rdefines.h>
#include <immintrin.h>

#define _paste(x, y)	x ## y
#define paste(x, y)	_paste(x, y)

#if !defined with
# define with(args...)							\
	for (args, *paste(__ep, __LINE__) = (void*)1;			\
	     paste(__ep, __LINE__); paste(__ep, __LINE__)= 0)
#endif	/* !with */

static int
isna(SEXP x)
{
	switch (TYPEOF(x)) {
	case NILSXP:
		return 1;
	case LGLSXP:
		return LOGICAL_ELT(x, 0) == NA_LOGICAL;
	case INTSXP:
		return INTEGER_ELT(x, 0) == NA_INTEGER;
	case REALSXP:
		return ISNAN(REAL_ELT(x, 0));
	case CPLXSXP:
		with (Rcomplex v = COMPLEX_ELT(x, 0)) {
			return ISNAN(v.r) || ISNAN(v.i);
		}
	case STRSXP:
		return STRING_ELT(x, 0) == NA_STRING;
	default:
		break;
	}
	return -1;
}


SEXP coalesce(SEXP args)
{
	R_xlen_t n;
	SEXP x, ans;

	PROTECT(args = CDR(args));

	x = CAR(args);
	n = XLENGTH(x);

	if (n <= 1) {
		/* find and return first non-NA */
		for (; args != R_NilValue; args = CDR(args), x = CAR(args)) {
			int r;

			if (XLENGTH(x) > 1) {
				error("lengths don't add up");
			} else if ((r = isna(x)) < 0) {
				error("types don't add up");
			} else if (!r) {
				UNPROTECT(1);
				return x;
			}
		}
		PROTECT(ans = allocVector(LGLSXP, 1));
		*LOGICAL(ans) = NA_LOGICAL;
		goto out;
	}

	if (isFactor(x)) {
		/* convert them all to string lists */
		for (SEXP z = args; z != R_NilValue; z = CDR(z)) {
			SETCAR(z, asCharacterFactor(CAR(z)));
		}
		x = CAR(args);
	}

	PROTECT(ans = allocVector(TYPEOF(x), n));
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
		memcpy(DATAPTR(ans), DATAPTR_RO(x), n * sizeof(int));
		break;
	case REALSXP:
		memcpy(DATAPTR(ans), DATAPTR_RO(x), n * sizeof(double));
		break;
	case CPLXSXP:
		memcpy(DATAPTR(ans), DATAPTR_RO(x), n * sizeof(Rcomplex));
		break;
	case STRSXP:
		memcpy(DATAPTR(ans), DATAPTR_RO(x), n * sizeof(SEXP));
		break;
	case RAWSXP:
	case NILSXP:
	default:
		error("unsupported type");
		goto err;
	}

	for (args = CDR(args), x = CAR(args);
	     args != R_NilValue; args = CDR(args), x = CAR(args)) {
		if (TYPEOF(x) != TYPEOF(ans)) {
			error("types don't add up");
			goto err;
		} else if (XLENGTH(x) != n) {
			error("lengths don't add up");
			goto err;
		}

		switch (TYPEOF(x)) {
			R_xlen_t k;
		case LGLSXP:
			for (R_xlen_t j = 0; j < n; j++) {
				LOGICAL(ans)[j] = LOGICAL(ans)[j] != NA_LOGICAL
					? LOGICAL(ans)[j]
					: LOGICAL(x)[j];
			}
			for (R_xlen_t j = k = 0; j < n; j++) {
				k += LOGICAL(ans)[j] != NA_LOGICAL;
			}
			if (k >= n) {
				/* premature finish */
				goto out;
			}
			break;
		case INTSXP:
			for (R_xlen_t j = 0; j < n; j++) {
				INTEGER(ans)[j] = INTEGER(ans)[j] != NA_INTEGER
					? INTEGER(ans)[j]
					: INTEGER(x)[j];
			}
			for (R_xlen_t j = k = 0; j < n; j++) {
				k += INTEGER(ans)[j] != NA_INTEGER;
			}
			if (k >= n) {
				/* premature finish */
				goto out;
			}
			break;
		case REALSXP:
			for (R_xlen_t j = 0; j < n; j++) {
				REAL(ans)[j] = !ISNAN(REAL(ans)[j])
					? REAL(ans)[j]
					: REAL(x)[j];
			}
			for (R_xlen_t j = k = 0; j < n; j++) {
				k += !ISNAN(REAL(ans)[j]);
			}
			if (k >= n) {
				/* premature finish */
				goto out;
			}
			break;
		case CPLXSXP:
			for (R_xlen_t j = 0; j < n; j++) {
				Rcomplex v = COMPLEX(ans)[j];
				COMPLEX(ans)[j] = !ISNAN(v.r) && !ISNAN(v.i)
					? v
					: COMPLEX(x)[j];
			}
			for (R_xlen_t j = k = 0; j < n; j++) {
				Rcomplex v = COMPLEX(ans)[j];
				k += !ISNAN(v.r) && !ISNAN(v.i);
			}
			if (k >= n) {
				/* premature finish */
				goto out;
			}
			break;
		case STRSXP:
			for (R_xlen_t j = 0; j < n; j++) {
				SEXP v = STRING_ELT(ans, j);
				if (v == NA_STRING) {
					SET_STRING_ELT(ans, j, STRING_ELT(x, j));
				}
			}
			for (R_xlen_t j = k = 0; j < n; j++) {
				SEXP v = STRING_ELT(ans, j);
				k += v != NA_STRING;
			}
			if (k >= n) {
				/* premature finish */
				goto out;
			}
			break;
		default:
			error("unsupported type");
			goto err;
		}
	}

out:
	UNPROTECT(2);
	return ans;
err:
	UNPROTECT(2);
	return R_NilValue;
}

SEXP edges(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	if (!n) {
		return allocVector(LGLSXP, 0);
	}

	PROTECT(x);
	PROTECT(ans = allocVector(LGLSXP, n));

	switch (TYPEOF(x)) {
		R_xlen_t k;
	case STRSXP:
		for (k = 0; k < n && STRING_ELT(x, k) == NA_STRING; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		for (R_xlen_t i; k < n; k = i) {
			SEXP r = STRING_ELT(x, k);

			LOGICAL(ans)[k] = TRUE;
			for (i = k + 1; i < n; i++) {
				for (; i < n && STRING_ELT(x, i) == NA_STRING; i++) {
					LOGICAL(ans)[i] = NA_LOGICAL;
				}
				if (i >= n ||
				    strcmp(CHAR(r), CHAR(STRING_ELT(x, i)))) {
					break;
				}
				LOGICAL(ans)[i] = FALSE;
			}
		}
		break;
	case INTSXP:
		for (k = 0; k < n && INTEGER(x)[k] == NA_INTEGER; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		for (R_xlen_t i; k < n; k = i) {
			LOGICAL(ans)[k] = TRUE;
			for (i = k + 1; i < n; i++) {
				for (; i < n && INTEGER(x)[i] == NA_INTEGER; i++) {
					LOGICAL(ans)[i] = NA_LOGICAL;
				}
				if (i >= n || INTEGER(x)[i] != INTEGER(x)[k]) {
					break;
				}
				LOGICAL(ans)[i] = FALSE;
			}
		}
		break;
	case REALSXP:
		for (k = 0; k < n && R_IsNA(REAL(x)[k]); k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		for (R_xlen_t i; k < n; k = i) {
			LOGICAL(ans)[k] = TRUE;
			for (i = k + 1; i < n; i++) {
				for (; i < n && R_IsNA(REAL(x)[i]); i++) {
					LOGICAL(ans)[i] = NA_LOGICAL;
				}
				if (i >= n ||
				    fabs(REAL(x)[i] - REAL(x)[k]) > DBL_EPSILON) {
					break;
				}
				LOGICAL(ans)[i] = FALSE;
			}
		}
		break;
	case LGLSXP:
		for (k = 0; k < n && LOGICAL(x)[k] == NA_LOGICAL; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		for (R_xlen_t i; k < n; k = i) {
			LOGICAL(ans)[k] = TRUE;
			for (i = k + 1; i < n; i++) {
				for (; i < n && LOGICAL(x)[i] == NA_LOGICAL; i++) {
					LOGICAL(ans)[i] = NA_LOGICAL;
				}
				if (i >= n || LOGICAL(x)[i] != LOGICAL(x)[k]) {
					break;
				}
				LOGICAL(ans)[i] = FALSE;
			}
		}
		break;
	case LISTSXP:
	case VECSXP:
	case CPLXSXP:
	case RAWSXP:
	case NILSXP:
	default:
	naught:
		for (k = 0; k < n && REAL(x)[k] == NA_REAL; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		break;
	}

	UNPROTECT(2);
	return ans;
}
