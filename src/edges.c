#include <math.h>
#include <stdint.h>
#include <string.h>
#include "edges.h"
#include <Rdefines.h>
#include <immintrin.h>
#include "nifty.h"

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
	R_xlen_t n = 0;
	SEXPTYPE anstyp;
	SEXP ans;
	SEXP cls;

	/* eliminate NULLs */
	for (SEXP p = args, z = CDR(p); z != R_NilValue; z = CDR(z)) {
		if (CAR(z) == R_NilValue) {
			SETCDR(p, CDR(z));
		} else {
			p = z;
		}
	}

	anstyp = TYPEOF(CAR(args = CDR(args)));
	if (UNLIKELY(anstyp == NILSXP)) {
		return R_NilValue;
	}
	cls = PROTECT(getAttrib(CAR(args), R_ClassSymbol));

	/* obtain maximum length and check types */
	for (SEXP z = args; z != R_NilValue; z = CDR(z)) {
		SEXP x = CAR(z);
		R_xlen_t m = XLENGTH(x);

		n = m <= n ? n : m;

		if (isFactor(x)) {
			SETCAR(z, x = asCharacterFactor(x));
			anstyp = STRSXP;
			cls = R_NilValue;
		} else if (UNLIKELY(TYPEOF(x) != anstyp)) {
			error("types don't add up");
		} else if (!R_compute_identical(cls, getAttrib(x, R_ClassSymbol), 0)) {
			error("classes don't add up");
		}
	}

	if (n <= 1) {
		/* find and return first non-NA */
		for (; args != R_NilValue; args = CDR(args)) {
			SEXP x = CAR(args);
			int r;

			if ((r = isna(x)) < 0) {
				error("unsupported type");
			} else if (!r) {
				UNPROTECT(1);
				return x;
			}
		}
		PROTECT(ans = allocVector(LGLSXP, 1));
		*LOGICAL(ans) = NA_LOGICAL;
		goto out;
	}

	PROTECT(ans = allocVector(anstyp, n));
	with (SEXP x = CAR(args)) {
		size_t m = XLENGTH(x);
		size_t z;

		switch (anstyp) {
		case INTSXP:
		case LGLSXP:
			z = sizeof(int);
			break;
		case REALSXP:
			z = sizeof(double);
			break;
		case CPLXSXP:
			z = sizeof(Rcomplex);
			break;
		case STRSXP:
			z = sizeof(SEXP);
			break;
		case RAWSXP:
		case NILSXP:
		default:
			error("unsupported type");
			goto err;
		}
		memcpy(DATAPTR(ans), DATAPTR_RO(x), z * m);
		if (anstyp != STRSXP) {
			memset((char*)DATAPTR(ans) + (z * m), -1, z * (n - m));
		} else {
			for (size_t j = m; j < n; j++) {
				SET_STRING_ELT(ans, j, NA_STRING);
			}
		}
	}

	for (; args != R_NilValue; args = CDR(args)) {
		SEXP x = CAR(args);

		switch (anstyp) {
		case LGLSXP: {
			int *restrict ansp = LOGICAL(ans);
			const int *xp = LOGICAL(x);
			R_xlen_t m = XLENGTH(x);
			R_xlen_t k = 0;

			#pragma omp parallel for
			for (R_xlen_t j = 0U; j < m; j++) {
				ansp[j] = ansp[j] != NA_LOGICAL
					? ansp[j]
					: xp[j];
			}
			if (LIKELY(m > 1 || CDR(args) != R_NilValue)) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < m; j++) {
					k += ansp[j] != NA_LOGICAL;
				}
				if (k >= n) {
					/* premature finish */
					goto clsout;
				}
			} else if (m == 1) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					ansp[j] = ansp[j] != NA_LOGICAL ? ansp[j] : *xp;
				}
				goto clsout;
			}
			break;
		}
		case INTSXP: {
			int *restrict ansp = INTEGER(ans);
			const int *xp = INTEGER(x);
			R_xlen_t m = XLENGTH(x);
			R_xlen_t k = 0;

			#pragma omp parallel for
			for (R_xlen_t j = 0U; j < m; j++) {
				ansp[j] = ansp[j] != NA_INTEGER
					? ansp[j]
					: xp[j];
			}
			if (LIKELY(m > 1 || CDR(args) != R_NilValue)) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < m; j++) {
					k += ansp[j] != NA_LOGICAL;
				}
				if (k >= n) {
					/* premature finish */
					goto clsout;
				}
			} else if (m == 1) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					ansp[j] = ansp[j] != NA_INTEGER ? ansp[j] : *xp;
				}
				goto clsout;
			}
			break;
		}
		case REALSXP: {
			double *restrict ansp = REAL(ans);
			const double *xp = REAL(x);
			R_xlen_t m = XLENGTH(x);
			R_xlen_t k = 0;

			#pragma omp parallel for
			for (R_xlen_t j = 0; j < m; j++) {
				ansp[j] = !ISNAN(ansp[j])
					? ansp[j]
					: xp[j];
			}
			if (LIKELY(m > 1 || CDR(args) != R_NilValue)) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					k += !ISNAN(ansp[j]);
				}
				if (k >= n) {
					/* premature finish */
					goto clsout;
				}
			} else if (m == 1) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					ansp[j] = !ISNAN(ansp[j]) ? ansp[j] : *xp;
				}
				goto clsout;
			}
			break;
		}
		case CPLXSXP: {
			Rcomplex *restrict ansp = COMPLEX(ans);
			const Rcomplex *xp = COMPLEX(x);
			R_xlen_t m = XLENGTH(x);
			R_xlen_t k = 0;

			#pragma omp parallel for
			for (R_xlen_t j = 0; j < m; j++) {
				Rcomplex v = ansp[j];
				ansp[j] = !ISNAN(v.r) && !ISNAN(v.i)
					? v
					: xp[j];
			}
			if (LIKELY(m > 1 || CDR(args) != R_NilValue)) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					Rcomplex v = ansp[j];
					k += !ISNAN(v.r) && !ISNAN(v.i);
				}
				if (k >= n) {
					/* premature finish */
					goto clsout;
				}
			} else if (m == 1) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					Rcomplex v = ansp[j];
					ansp[j] = !ISNAN(v.r) && !ISNAN(v.i) ? v : *xp;
				}
				goto clsout;
			}
			break;
		}
		case STRSXP: {
			SEXP *restrict ansp = STRING_PTR(ans);
			const SEXP *xp = STRING_PTR_RO(x);
			R_xlen_t m = XLENGTH(x);
			R_xlen_t k = 0;

			#pragma omp parallel for
			for (R_xlen_t j = 0; j < m; j++) {
				ansp[j] = ansp[j] != NA_STRING
					? ansp[j]
					: xp[j];
			}
			if (LIKELY(m > 1 || CDR(args) != R_NilValue)) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					k += ansp[j] != NA_STRING;
				}
				if (k >= n) {
					/* premature finish */
					goto clsout;
				}
			} else if (m == 1) {
				#pragma omp parallel for
				for (R_xlen_t j = 0; j < n; j++) {
					ansp[j] = ansp[j] != NA_STRING ? ansp[j] : *xp;
				}
				goto clsout;
			}
			break;
		}
		default:
			error("unsupported type");
			goto err;
		}
	}

clsout:
	classgets(ans, cls);
out:
	UNPROTECT(2);
	return ans;
err:
	UNPROTECT(2);
	return R_NilValue;
}

SEXP tcoalesce1(SEXP arg, SEXP rev)
{
	SEXPTYPE anstyp = TYPEOF(arg);
	R_xlen_t n;
	SEXP cls;
	SEXP ans;

	if (UNLIKELY(anstyp == NILSXP)) {
		return R_NilValue;
	}
	if (isFactor(arg)) {
		arg = asCharacterFactor(arg);
		anstyp = STRSXP;
	}
	n = XLENGTH(arg);
	PROTECT(ans = allocVector(anstyp, n > 0));
	classgets(ans, getAttrib(arg, R_ClassSymbol));
	UNPROTECT(1);
	if (n <= 0) {
		return ans;
	}
	if (*INTEGER_RO(rev) <= 0) {
		switch (anstyp) {
			R_xlen_t k;
		case STRSXP:
			for (k = 0; k < n && STRING_ELT(arg, k) == NA_STRING; k++);
			k += k < n;
			SET_STRING_ELT(ans, 0, STRING_ELT(arg, k - 1));
			break;
		case INTSXP:
		case LGLSXP:
			for (k = 0; k < n && INTEGER(arg)[k] == NA_INTEGER; k++);
			k += k < n;
			INTEGER(ans)[0] = INTEGER(arg)[k - 1];
			break;
		case REALSXP:
			for (k = 0; k < n && R_IsNA(REAL(arg)[k]); k++);
			k += k < n;
			REAL(ans)[0] = REAL(arg)[k - 1];
			break;
		case CPLXSXP:
			for (k = 0; k < n && R_IsNA(COMPLEX(arg)[k].r); k++);
			k += k < n;
			COMPLEX(ans)[0] = COMPLEX(arg)[k - 1];
			break;
		case LISTSXP:
		case VECSXP:
		case RAWSXP:
		case NILSXP:
		default:
			goto err;
		}
	} else {
		switch (anstyp) {
			R_xlen_t k;
		case STRSXP:
			for (k = n; k > 0 && STRING_ELT(arg, k-1) == NA_STRING; k--);
			k -= k > 0;
			SET_STRING_ELT(ans, 0, STRING_ELT(arg, k));
			break;
		case INTSXP:
		case LGLSXP:
			for (k = n; k > 0 && INTEGER(arg)[k-1] == NA_INTEGER; k--);
			k -= k > 0;
			INTEGER(ans)[0] = INTEGER(arg)[k];
			break;
		case REALSXP:
			for (k = n; k > 0 && R_IsNA(REAL(arg)[k-1]); k--);
			k -= k > 0;
			REAL(ans)[0] = REAL(arg)[k];
			break;
		case CPLXSXP:
			for (k = n; k > 0 && R_IsNA(COMPLEX(arg)[k-1].r); k--);
			k -= k > 0;
			COMPLEX(ans)[0] = COMPLEX(arg)[k];
			break;
		case LISTSXP:
		case VECSXP:
		case RAWSXP:
		case NILSXP:
		default:
			goto err;
		}
	}
	return ans;
err:
	return R_NilValue;
}

SEXP tcoalesceI(SEXP arg, SEXP rev)
{
	R_xlen_t n;
	SEXP cls;
	SEXP ans;

	if (UNLIKELY(TYPEOF(arg) == NILSXP)) {
		return R_NilValue;
	}
	if (isFactor(arg)) {
		arg = asCharacterFactor(arg);
	}
	n = XLENGTH(arg);
	PROTECT(ans = allocVector(INTSXP, n > 0));
	UNPROTECT(1);
	if (n <= 0) {
		return ans;
	}
	if (*INTEGER_RO(rev) <= 0) {
		R_xlen_t k;
		switch (TYPEOF(arg)) {
		case STRSXP:
			for (k = 0; k < n && STRING_ELT(arg, k) == NA_STRING; k++);
			break;
		case INTSXP:
		case LGLSXP:
			for (k = 0; k < n && INTEGER(arg)[k] == NA_INTEGER; k++);
			break;
		case REALSXP:
			for (k = 0; k < n && R_IsNA(REAL(arg)[k]); k++);
			break;
		case CPLXSXP:
			for (k = 0; k < n && R_IsNA(COMPLEX(arg)[k].r); k++);
			break;
		case LISTSXP:
		case VECSXP:
		case RAWSXP:
		case NILSXP:
		default:
			goto err;
		}
		*INTEGER(ans) = (k < n ? k : 0) + 1;
	} else {
		R_xlen_t k;
		switch (TYPEOF(arg)) {
		case STRSXP:
			for (k = n; k > 0 && STRING_ELT(arg, k-1) == NA_STRING; k--);
			break;
		case INTSXP:
		case LGLSXP:
			for (k = n; k > 0 && INTEGER(arg)[k-1] == NA_INTEGER; k--);
			break;
		case REALSXP:
			for (k = n; k > 0 && R_IsNA(REAL(arg)[k-1]); k--);
			break;
		case CPLXSXP:
			for (k = n; k > 0 && R_IsNA(COMPLEX(arg)[k-1].r); k--);
			break;
		case LISTSXP:
		case VECSXP:
		case RAWSXP:
		case NILSXP:
		default:
			goto err;
		}
		*INTEGER(ans) = k ?: n;
	}
	return ans;
err:
	return R_NilValue;
}


SEXP na_locf0(SEXP x, SEXP fbwd)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP cls, lvl;
	SEXP ans;
	int r;

	if (UNLIKELY(anstyp == NILSXP)) {
		return R_NilValue;
	}
	PROTECT(ans = allocVector(anstyp, n = XLENGTH(x)));
	cls = PROTECT(getAttrib(x, R_ClassSymbol));
	if (isFactor(x)) {
		lvl = PROTECT(getAttrib(x, R_LevelsSymbol));
	}

	switch (anstyp) {
		R_xlen_t k;
	case STRSXP:
		for (k = 0; k < n && STRING_ELT(x, k) == NA_STRING; k++) {
			SET_STRING_ELT(ans, k, NA_STRING);
		}
		if (*INTEGER(fbwd) > 0 && k < n) {
			for (R_xlen_t i = 0; i < k; i++) {
				SET_STRING_ELT(ans, i, STRING_ELT(x, k));
			}
		}
		for (R_xlen_t i; k < n; k = i) {
			SET_STRING_ELT(ans, k, STRING_ELT(x, k));
			for (i = k + 1; i < n && STRING_ELT(x, i) == NA_STRING; i++) {
				SET_STRING_ELT(ans, i, STRING_ELT(x, k));
			}
		}
		break;
	case INTSXP:
		for (k = 0; k < n && INTEGER(x)[k] == NA_INTEGER; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		if (*INTEGER(fbwd) > 0 && k < n) {
			for (R_xlen_t i = 0; i < k; i++) {
				INTEGER(ans)[i] = INTEGER(x)[k];
			}
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = INTEGER(x)[k];
			for (i = k + 1; i < n && INTEGER(x)[i] == NA_INTEGER; i++) {
				INTEGER(ans)[i] = INTEGER(x)[k];
			}
		}
		break;
	case REALSXP:
		for (k = 0; k < n && R_IsNA(REAL(x)[k]); k++) {
			REAL(ans)[k] = NA_REAL;
		}
		if (*INTEGER(fbwd) > 0 && k < n) {
			for (R_xlen_t i = 0; i < k; i++) {
				REAL(ans)[i] = REAL(x)[k];
			}
		}
		for (R_xlen_t i; k < n; k = i) {
			REAL(ans)[k] = REAL(x)[k];
			for (i = k + 1; i < n && R_IsNA(REAL(x)[i]); i++) {
				REAL(ans)[i] = REAL(x)[k];
			}
		}
		break;
	case LGLSXP:
		for (k = 0; k < n && LOGICAL(x)[k] == NA_LOGICAL; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		if (*INTEGER(fbwd) > 0 && k < n) {
			for (R_xlen_t i = 0; i < k; i++) {
				LOGICAL(ans)[i] = LOGICAL(x)[k];
			}
		}
		for (R_xlen_t i; k < n; k = i) {
			LOGICAL(ans)[k] = LOGICAL(x)[k];
			for (i = k + 1; i < n && LOGICAL(x)[i] == NA_LOGICAL; i++) {
				LOGICAL(ans)[i] = LOGICAL(x)[k];
			}
		}
		break;
	case LISTSXP:
	case VECSXP:
	case CPLXSXP:
	case RAWSXP:
	case NILSXP:
	default:
		ans = x;
		break;
	}

	classgets(ans, cls);
	if (isFactor(x)) {
		setAttrib(ans, R_LevelsSymbol, lvl);
	}
	UNPROTECT(2 + isFactor(x));
	return ans;
}

SEXP na_nocb0(SEXP x, SEXP lfwd)
{
	SEXPTYPE anstyp = TYPEOF(x);
	R_xlen_t n;
	SEXP cls, lvl;
	SEXP ans;
	int r;

	if (UNLIKELY(anstyp == NILSXP)) {
		return R_NilValue;
	}
	PROTECT(ans = allocVector(anstyp, n = XLENGTH(x)));
	cls = PROTECT(getAttrib(x, R_ClassSymbol));
	if (isFactor(x)) {
		lvl = PROTECT(getAttrib(x, R_LevelsSymbol));
	}

	switch (anstyp) {
		R_xlen_t k;
	case STRSXP:
		for (k = n; k > 0 && STRING_ELT(x, k-1) == NA_STRING; k--) {
			SET_STRING_ELT(ans, k-1, NA_STRING);
		}
		if (*INTEGER(lfwd) > 0 && k > 0) {
			for (R_xlen_t i = n; i > k; i--) {
				SET_STRING_ELT(ans, i-1, STRING_ELT(x, k-1));
			}
		}
		for (R_xlen_t i; k > 0; k = i) {
			SET_STRING_ELT(ans, k-1, STRING_ELT(x, k-1));
			for (i = k-1; i > 0 && STRING_ELT(x, i-1) == NA_STRING; i--) {
				SET_STRING_ELT(ans, i-1, STRING_ELT(x, k-1));
			}
		}
		break;
	case INTSXP:
		for (k = n; k > 0 && INTEGER(x)[k-1] == NA_INTEGER; k--) {
			INTEGER(ans)[k-1] = NA_INTEGER;
		}
		if (*INTEGER(lfwd) > 0 && k > 0) {
			for (R_xlen_t i = n; i > k; i--) {
				INTEGER(ans)[i-1] = INTEGER(x)[k-1];
			}
		}
		for (R_xlen_t i; k > 0; k = i) {
			INTEGER(ans)[k-1] = INTEGER(x)[k-1];
			for (i = k-1; i > 0 && INTEGER(x)[i-1] == NA_INTEGER; i--) {
				INTEGER(ans)[i-1] = INTEGER(x)[k-1];
			}
		}
		break;
	case REALSXP:
		for (k = n; k > 0 && R_IsNA(REAL(x)[k-1]); k--) {
			REAL(ans)[k-1] = NA_REAL;
		}
		if (*INTEGER(lfwd) > 0 && k > 0) {
			for (R_xlen_t i = n; i > k; i--) {
				REAL(ans)[i-1] = REAL(x)[k-1];
			}
		}
		for (R_xlen_t i; k > 0; k = i) {
			REAL(ans)[k-1] = REAL(x)[k-1];
			for (i = k-1; i > 0 && R_IsNA(REAL(x)[i-1]); i--) {
				REAL(ans)[i-1] = REAL(x)[k-1];
			}
		}
		break;
	case LGLSXP:
		for (k = n; k > 0 && LOGICAL(x)[k-1] == NA_LOGICAL; k--) {
			LOGICAL(ans)[k-1] = NA_LOGICAL;
		}
		if (*INTEGER(lfwd) > 0 && k > 0) {
			for (R_xlen_t i = n; i > k; i--) {
				LOGICAL(ans)[i-1] = LOGICAL(x)[k-1];
			}
		}
		for (R_xlen_t i; k > 0; k = i) {
			LOGICAL(ans)[k-1] = LOGICAL(x)[k-1];
			for (i = k-1; i > 0 && LOGICAL(x)[i-1] == NA_LOGICAL; i--) {
				LOGICAL(ans)[i-1] = LOGICAL(x)[k-1];
			}
		}
		break;
	case LISTSXP:
	case VECSXP:
	case CPLXSXP:
	case RAWSXP:
	case NILSXP:
	default:
		ans = x;
		break;
	}

	classgets(ans, cls);
	if (isFactor(x)) {
		setAttrib(ans, R_LevelsSymbol, lvl);
	}
	UNPROTECT(2 + isFactor(x));
	return ans;
}


SEXP edges(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	if (!n) {
		return allocVector(LGLSXP, 0);
	}

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
				if (i >= n || r != STRING_ELT(x, i)) {
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
		for (k = 0; k < n; k++) {
			LOGICAL(ans)[k] = NA_LOGICAL;
		}
		break;
	}

	UNPROTECT(1);
	return ans;
}

SEXP rally(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;
	int r = 0;

	if (!n) {
		return allocVector(INTSXP, 0);
	}

	PROTECT(ans = allocVector(INTSXP, n));

	switch (TYPEOF(x)) {
		R_xlen_t k;
	case STRSXP:
		for (k = 0; k < n && STRING_ELT(x, k) == NA_STRING; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			SEXP s = STRING_ELT(x, k);

			INTEGER(ans)[k] = ++r;
			for (i = k + 1; i < n; i++) {
				for (; i < n && STRING_ELT(x, i) == NA_STRING; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || s != STRING_ELT(x, i)) {
					break;
				}
				INTEGER(ans)[i] = r;
			}
		}
		break;
	case INTSXP:
		for (k = 0; k < n && INTEGER(x)[k] == NA_INTEGER; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = ++r;
			for (i = k + 1; i < n; i++) {
				for (; i < n && INTEGER(x)[i] == NA_INTEGER; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || INTEGER(x)[i] != INTEGER(x)[k]) {
					break;
				}
				INTEGER(ans)[i] = r;
			}
		}
		break;
	case REALSXP:
		for (k = 0; k < n && R_IsNA(REAL(x)[k]); k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = ++r;
			for (i = k + 1; i < n; i++) {
				for (; i < n && R_IsNA(REAL(x)[i]); i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n ||
				    fabs(REAL(x)[i] - REAL(x)[k]) > DBL_EPSILON) {
					break;
				}
				INTEGER(ans)[i] = r;
			}
		}
		break;
	case LGLSXP:
		for (k = 0; k < n && LOGICAL(x)[k] == NA_LOGICAL; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = ++r;
			for (i = k + 1; i < n; i++) {
				for (; i < n && LOGICAL(x)[i] == NA_LOGICAL; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || LOGICAL(x)[i] != LOGICAL(x)[k]) {
					break;
				}
				INTEGER(ans)[i] = r;
			}
		}
		break;
	case LISTSXP:
	case VECSXP:
	case CPLXSXP:
	case RAWSXP:
	case NILSXP:
	default:
		for (k = 0; k < n; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		break;
	}

	UNPROTECT(1);
	return ans;
}

SEXP cumrally(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;
	int r;

	if (!n) {
		return allocVector(INTSXP, 0);
	}

	PROTECT(ans = allocVector(INTSXP, n));

	switch (TYPEOF(x)) {
		R_xlen_t k;
	case STRSXP:
		for (k = 0; k < n && STRING_ELT(x, k) == NA_STRING; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			SEXP s = STRING_ELT(x, k);

			INTEGER(ans)[k] = r = 1;
			for (i = k + 1; i < n; i++) {
				for (; i < n && STRING_ELT(x, i) == NA_STRING; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || s != STRING_ELT(x, i)) {
					break;
				}
				INTEGER(ans)[i] = ++r;
			}
		}
		break;
	case INTSXP:
		for (k = 0; k < n && INTEGER(x)[k] == NA_INTEGER; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = r = 1;
			for (i = k + 1; i < n; i++) {
				for (; i < n && INTEGER(x)[i] == NA_INTEGER; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || INTEGER(x)[i] != INTEGER(x)[k]) {
					break;
				}
				INTEGER(ans)[i] = ++r;
			}
		}
		break;
	case REALSXP:
		for (k = 0; k < n && R_IsNA(REAL(x)[k]); k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = r = 1;
			for (i = k + 1; i < n; i++) {
				for (; i < n && R_IsNA(REAL(x)[i]); i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n ||
				    fabs(REAL(x)[i] - REAL(x)[k]) > DBL_EPSILON) {
					break;
				}
				INTEGER(ans)[i] = ++r;
			}
		}
		break;
	case LGLSXP:
		for (k = 0; k < n && LOGICAL(x)[k] == NA_LOGICAL; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		for (R_xlen_t i; k < n; k = i) {
			INTEGER(ans)[k] = r = 1;
			for (i = k + 1; i < n; i++) {
				for (; i < n && LOGICAL(x)[i] == NA_LOGICAL; i++) {
					INTEGER(ans)[i] = NA_INTEGER;
				}
				if (i >= n || LOGICAL(x)[i] != LOGICAL(x)[k]) {
					break;
				}
				INTEGER(ans)[i] = ++r;
			}
		}
		break;
	case LISTSXP:
	case VECSXP:
	case CPLXSXP:
	case RAWSXP:
	case NILSXP:
	default:
		for (k = 0; k < n; k++) {
			INTEGER(ans)[k] = NA_INTEGER;
		}
		break;
	}

	UNPROTECT(1);
	return ans;
}
