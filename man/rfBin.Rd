\name{rfBin}
\alias{rfBin}
\title{
  Read binary file
}
\description{
  \code{rfBin} is a front-end to \code{readBin} that reads the entire
  content of a binary file.
}
\usage{
rfBin(filename, what = 1L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{name of the file to read}
  \item{what}{either an integer or real vector defining the payload type}
  \item{\dots}{additional parameters passed to \code{readBin}}
}
%\details{
%}
\value{
  Same as \code{\link{readBin}}
}
%\references{
%}
\author{
  Simon Urbanek
}
%\note{
%}
%\examples{
%}
\keyword{io}
