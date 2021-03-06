#include "edges.h"
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const
R_CallMethodDef callMethods[] = {
	{"Cedges", (DL_FUNC)&edges, -1},
	{"Crally", (DL_FUNC)&rally, -1},
	{"Ccumrally", (DL_FUNC)&cumrally, -1},
	{"Cna.locf0", (DL_FUNC)&na_locf0, -1},
	{"Cna.nocb0", (DL_FUNC)&na_nocb0, -1},
	{"Ctcoalesce1", (DL_FUNC)&tcoalesce1, -1},
	{"CtcoalesceI", (DL_FUNC)&tcoalesceI, -1},

	{"Cna.cumsum", (DL_FUNC)&na_cumsum, -1},
	{"Cna.cumprod", (DL_FUNC)&na_cumprod, -1},
	{"Cna.cummax", (DL_FUNC)&na_cummax, -1},
	{"Cna.cummin", (DL_FUNC)&na_cummin, -1},
	{NULL, NULL, 0}
};

static const
R_ExternalMethodDef externalMethods[] = {
	{"Ccoalesce", (DL_FUNC)&coalesce, -1},
	{NULL, NULL, 0}
};

void attribute_visible R_init_edges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
	R_useDynamicSymbols(info, FALSE);
	return;
}
