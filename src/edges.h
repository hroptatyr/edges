#include <R.h>
#include <Rinternals.h>

SEXP coalesce(SEXP args);
SEXP tcoalesce1(SEXP arg);
SEXP na_locf0(SEXP x, SEXP fbwd);
SEXP na_nocb0(SEXP x, SEXP lfwd);

SEXP edges(SEXP x);
SEXP rally(SEXP x);
SEXP cumrally(SEXP x);
