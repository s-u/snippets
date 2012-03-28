\name{split.at}
\alias{split.at}
\title{
  Split vector at given index points
}
\description{
  \code{split.at} takes a vector of indices (cutpoints) at which a data
  vector is to be split into pieces. Given \code{n} cutpoints the result
  is a list of \code{n + 1} vectors of the same type as the data vector.
}
\usage{
split.at(x, cutpoints, exclusive = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data vector to split. Currently only numeric, integer and
    character vectors are supported.}
  \item{cutpoints}{vector of indices at which to cut}
  \item{exclusive}{if \code{TRUE} then the elements at the cutpoint
    indices are removed, otherwise they are retained}
}
\details{
  The \code{cutpoints} are 1-based indices (they are always coerced to
  an integer vector), no other indexing is supported (i.e. use
  \code{which} first if the input is logical).
}
\value{
  List of \code{length(cutpoints) + 1} vectors of the same type as \code{x}.
}
%\references{
%}
\author{
  Simon Urbanek
}
\examples{
foo <- 1:10
where <- c(2,3,6)
split.at(foo, where)
z <- split.at(foo, where, TRUE)
identical(unlist(z), foo[-where])
}
\keyword{manip}
