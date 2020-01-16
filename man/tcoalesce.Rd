\name{tcoalesce}
\alias{tcoalesce}
\title{Coalesce vectors}
\description{
Return the first non-NA value along each vector in a list of vectors.
}
\usage{
tcoalesce(\dots, rev=FALSE)
}
\arguments{
\item{\dots}{A sequence of vectors.}
\item{rev}{Whether to start looking for non-NA values from the back.}
}
\details{
Like ANSI SQL's COALESCE aggregate.
}
\value{
Sequence of vectors of length 1 of the same class as the inputs where for each vector first non-NA value is chosen.
}
\seealso{
  \code{\link{coalesce}}
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L)
z = c(11L, NA, 1L, 14L, NA, NA)
tcoalesce(x, y, z)
tcoalesce(x, y, z, rev=TRUE)
}
\keyword{data}
