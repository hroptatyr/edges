\name{coalesce}
\alias{coalesce}
\title{Coalesce vectors}
\description{
Return the first non-NA value in a list of vectors.
}
\usage{
coalesce(\dots)
}
\arguments{
\item{\dots}{A sequence of vectors of the same class.}
}
\details{
Like ANSI SQL's COALESCE aggregate.
}
\value{
Vector of the same class as the inputs with length equalling the longest of the inputs where for each row the first non-NA value in the input vector sequence is chosen.
}
\seealso{
  \code{\link{tcoalesce}}
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L)
z = c(11L, NA, 1L, 14L, NA, NA)
coalesce(x, y, z)
}
\keyword{data}
