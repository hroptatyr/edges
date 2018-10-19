coalesce <- function(...)
{
	.External(Ccoalesce, ...)
}

edges <- function(x)
{
	.Call(Cedges, x)
}
