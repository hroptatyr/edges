\name{na.cumsum}
\alias{na.cumsum}
\alias{na.cumprod}
\alias{na.cummax}
\alias{na.cummin}
\title{Cumulative Sums, Products, and Extremes}
\description{
Traverse vector and cumulate values across gaps of NAs.
}
\usage{
na.cumsum(x, na.locf=FALSE, first.backward=FALSE)
na.cumprod(x, na.locf=FALSE, first.backward=FALSE)
na.cummax(x, na.locf=FALSE, first.backward=FALSE)
na.cummin(x, na.locf=FALSE, first.backward=FALSE)
}
\arguments{
\item{x}{Vector.}
\item{na.locf}{Whether to carry the last observation forward}
\item{first.backward}{Start cumulation with the empty sum, product, or the first observation}
}
%\details{
%}
\value{
Vector of the same length and type as \code{x} maintaining NAs or replacing them with the last non-NA value.
}
\seealso{
  \code{\link{cumsum}}
  \code{\link{cumprod}}
  \code{\link{cummax}}
  \code{\link{cummin}}
}
\examples{
na.cumsum(c(NA, 1L, NA, 3L, NA, NA))
na.cumsum(c(NA, 1L, NA, 3L, NA, NA), na.locf=TRUE)
}
\keyword{data}
