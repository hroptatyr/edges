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
