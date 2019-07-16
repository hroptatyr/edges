\name{edges}
\alias{edges}
\title{Find edges in vector}
\description{
Return TRUE if the last non-NA item before the currently inspected is different, FALSE if not, NA otherwise.
}
\usage{
edges(x)
}
\arguments{
\item{x}{ A vector. }
}
% \details{
% }
\value{
A vector of logicals of the same length as x.  TRUE indicates an element that is different from the previous, FALSE an element that is identical to the previous and NAs are taken over from the original vector.
}
\seealso{\code{\link{rally}}}
\examples{
edges(c("EUR", "EUR", "GBP", NA, "GBP", "EUR")) # [1] TRUE FALSE TRUE NA FALSE TRUE
}
\keyword{data}
