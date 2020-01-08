\name{na.locf}
\alias{na.locf}
\title{Last Observation Carried Forward}
\description{
Traverse vector repeating the last non-NA value across gaps of NAs.
}
\usage{
na.locf(x, first.backward=FALSE)
}
\arguments{
\item{x}{Vector.}
\item{first.backward}{Carry first observation backward}
}
%\details{
%}
\value{
Vector sans NAs except at the beginning.
}
\seealso{
  \code{\link{na.nocb}}
}
\examples{
na.locf(c(NA, 1L, NA, 3L, NA, NA))
na.locf(c(NA, 1L, NA, 3L, NA, NA), first.backward=TRUE)
}
\keyword{data}
