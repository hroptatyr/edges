\name{na.nocb}
\alias{na.nocb}
\title{Next Observation Carried Backward}
\description{
Traverse vector repeating the next non-NA value across gaps of NAs.
}
\usage{
na.nocb(x, last.forward=FALSE)
}
\arguments{
\item{x}{Vector.}
\item{last.forward}{Carry last observation forward}
}
%\details{
%}
\value{
Vector sans NAs except at the end.
}
\seealso{
  \code{\link{na.nocb}}
}
\examples{
na.nocb(c(NA, 1L, NA, 3L, NA, NA))
na.nocb(c(NA, 1L, NA, 3L, NA, NA), last.forward=TRUE)
}
\keyword{data}
