coalesce <- function(...)
{
	.External(Ccoalesce, ...)
}

tcoalesce1 <- function(x)
{
	.Call(Ctcoalesce1, x)
}

tcoalesce <- function(...)
{
	x <- list(...)
	if (length(x) == 1L && is.list(x[[1L]])) {
		x <- x[[1L]]
	}
	if (length(x) == 1L) {
		tcoalesce1(x[[1L]])
	} else {
		lapply(x, tcoalesce1)
	}
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

na.locf0 <- function(x, first.backward)
{
	.Call(Cna.locf0, x, first.backward)
}

na.nocb0 <- function(x, last.forward)
{
	.Call(Cna.nocb0, x, last.forward)
}

na.locf <- function(x, first.backward=FALSE)
{
	x[] <- if (!length(dim(x))) {
		na.locf0(x, first.backward);
	} else if (is.data.frame(x)) {
		lapply(x, na.locf0, first.backward=first.backward)
	} else {
		apply(x, length(dim(x)), na.locf0, first.backward=first.backward);
	}
	return(x);
}

na.nocb <- function(x, last.forward=FALSE)
{
	x[] <- if (!length(dim(x))) {
		na.nocb0(x, last.forward);
	} else if (is.data.frame(x)) {
		lapply(x, na.nocb0, last.forward=last.forward)
	} else {
		apply(x, length(dim(x)), na.nocb0, last.forward=last.forward);
	}
	return(x);
}
