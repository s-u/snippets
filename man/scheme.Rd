\name{scheme}
\alias{scheme}
\alias{advance.scheme}
\alias{pop.scheme}
\alias{print.scheme}
\title{
Scheme hanlding functions
}
\description{
The following functions manage the stack of layout ``schemes'' created
by corresponding scheme constructors such as \code{\link{mfrow}} or
\code{\link{mfcol}}. Most of them are called by the implementation and
are not expected to be used by the user directly.

\code{advance.scheme} is called when a new plot is about to be drawn and
advances to the next scheme layout according to the stack hierarchy.

\code{pop.scheme} removes the topmost scheme from the stack.
}
\usage{
advance.scheme()
pop.scheme()
\method{print}{scheme}(x, \dots)
}
\arguments{
  \item{x}{scheme to be printed}
  \item{\dots}{additional arguments passed through}
}
%\details{
%}
%\value{
%}
\author{Simon Urbanek}
%\note{
%}
\seealso{
\code{\link{mfrow}}, \code{\link{mfcol}}
}
%\examples{
%}
\keyword{hplot}
