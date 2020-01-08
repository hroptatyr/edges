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
