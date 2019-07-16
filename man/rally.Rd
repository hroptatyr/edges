\name{rally}
\alias{rally}
\alias{cumrally}
\title{Find rallies in vector, or cumulative rally lengths in vector}
\description{
Enumerate rallies in vector.
}
\usage{
rally(x)
cumrally(x)
}
\arguments{
\item{x}{A vector.}
}
% \details{
% }
\value{
A vector of integers of the same length as x.  Every element of x is mapped to its rally number, for \code{rally}.

A vector of integers of the same length as x.  Every element of x's rally number is mapped to its cumulative length.

In contrast to \code{factor} which assigns equal numbers to equal elements, \code{rally} considers equal elements interrupted by anything other than NA as belonging to different rallies and hence assigns a new number.

In contrast to \code{data.table::rleid} NAs remain absent from inspection but they do appear as such in the output.
}
\seealso{\code{\link{edges}, \link{factor}}}
\examples{
rally(c("EUR", "EUR", "GBP", NA, "GBP", "EUR")) # [1] 1 1 2 NA 2 3
cumrally(c("EUR", "EUR", "GBP", NA, "GBP", "EUR")) # [1] 1 2 1 NA 2 1
}
\keyword{data}
