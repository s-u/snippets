\name{setup.figure}
\alias{setup.figure}
\alias{setup.figure.gridColScheme}
\alias{setup.figure.gridRowScheme}
\title{
Method defining a layout scheme
}
\description{
Layout schemes are defined by the scheme object and the implmentation of
the \code{setup.figure} method for that object.

The purpose of \code{setup.figure} is to set the \code{"fig"} graphical
parameter according to the state represented by the scheme object. This
allows implementation of arbitrary layout schemes.
}
\usage{
setup.figure(scheme)
\method{setup.figure}{gridColScheme}(scheme)
\method{setup.figure}{gridRowScheme}(scheme)
}
\arguments{
  \item{scheme}{scheme defining the current state which is to be
    reflected in the graphics parameters}
}
\details{
  The scheme will contain the enclosing figure region in
  \code{scheme$fig} and it is up to the \code{setup.figure} method
  implementation to use the advancement index \code{scheme$index} to
  determine the appropriate region to set the \code{"fig"} graphics
  parameter. Clearly, the scheme object can contain any additonal
  necessary needed for the method to perform its function.

  For example, the grid layout schemes keep the matrix of the grid in
  the scheme object and use simple modulo operation to determine the
  approriate column and row to set \code{"fig"} accordingly.
}
\value{
  scheme
}
%\references{
%}
\author{Simon Urbanek}
%\note{
%}
%\examples{
%}
\keyword{hplot}
