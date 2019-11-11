\name{na.locf}
\alias{na.locf}
\title{Last Observation Carried Forward}
\description{
Traverse vector repeating the last non-NA value across gaps of NAs.
}
\usage{
na.locf(x)
}
\arguments{
\item{x}{Vector.}
}
%\details{
%}
\value{
Vector sans NAs except at the beginning.
}
\examples{
na.locf(c(NA, 1L, NA, 3L, NA, NA))
}
\keyword{data}
