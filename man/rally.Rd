\name{rally}
\alias{rally}
\title{Find rallies in vector}
\description{
Enumerate rallies in vector.
}
\usage{
rally(x)
}
\arguments{
\item{x}{A vector.}
}
% \details{
% }
\value{
A vector of integers of the same length as x.  Every element of x is mapped to its rally number.

In contrast to \code{factor} which assigns equal numbers to equal elements, \code{rally} considers equal elements interrupted by anything other than NA as belonging to different rallies and hence assigns a new number.
}
\seealso{\code{\link{edges}, \link{factor}}}
\examples{
rally(c("EUR", "EUR", "GBP", NA, "GBP", "EUR")) # [1] 1 1 2 NA 2 3
edges(rally(c("EUR", "EUR", "GBP", NA, "GBP", "EUR"))) == edges(c("EUR", "EUR", "GBP", NA, "GBP", "EUR"))
}
\keyword{data}
