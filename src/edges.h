#include <R.h>
#include <Rinternals.h>

SEXP coalesce(SEXP args);
SEXP tcoalesce1(SEXP arg);
SEXP na_locf0(SEXP x, SEXP fbwd);
SEXP na_nocb0(SEXP x, SEXP lfwd);

SEXP edges(SEXP x);
SEXP rally(SEXP x);
SEXP cumrally(SEXP x);

SEXP na_cum(SEXP args);
SEXP na_cumsum(SEXP x, SEXP locf);
SEXP na_cumprod(SEXP x, SEXP locf);
SEXP na_cummax(SEXP x, SEXP locf);
SEXP na_cummin(SEXP x, SEXP locf);
