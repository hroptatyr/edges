coalesce <- function(...)
{
	.External(Ccoalesce, ...)
}

edges <- function(x)
{
	.Call(Cedges, x)
}

rally <- function(x)
{
	.Call(Crally, x)
}

cumrally <- function(x)
{
	.Call(Ccumrally, x)
}

na.locf0 <- function(x)
{
	.Call(Cna.locf0, x)
}

na.locf <- function(x)
{
	x[] <- if (!length(dim(x))) {
		na.locf0(x);
	} else if (is.data.frame(x)) {
		lapply(x, na.locf0)
	} else {
		apply(x, length(dim(x)), na.locf0);
	}
	return(x);
}
