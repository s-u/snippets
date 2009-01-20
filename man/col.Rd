\name{col}
\alias{col.base}
\alias{col.bbr}
\alias{col.bwr}
\alias{col.br}
\alias{col.bw}
\alias{col.q}
\title{
  Flexible quantitative color schemes
}
\description{
  \code{col.base} is the base for all other functions and allows full
  flexibility in defining arbitrary quantitative color schemes.

  \code{col.bbr} provides blue-black-red diverging color scheme
  (suitable for text and borderless glyphs on white background).

  \code{col.bwr} provides blue-white-red diverging color scheme
  (suitable for maps and gryphs with borders).

  \code{col.br} provides blue-red color scheme.

  \code{col.bw} provides black-white color scheme.

  \code{col.q} returns transformed quantity between 0 and 1 for each
  datapoint (suitable for pass-though to other color
  schemes). \code{gray(col.q(...))} has the same effect (except for
  lack of alpha support) as \code{col.bw}.
}
\usage{
col.base(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA, FN)
col.bbr (x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA)
col.bwr (x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA)
col.br  (x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA)
col.bw  (x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA)
col.q   (x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE,
         trans="id", na.col=NA)
}
\arguments{
  \item{x}{data values (treated as numeric vector).}
  \item{alpha}{alpha value. It will be passed directly to the call to
  \code{rgb} except for \code{col.q} where is used to multiply the
  resulting value.}
  \item{lim}{data cut-off limits (range). Values outside this range will be
  clipped to the nearest value in the range.}
  \item{center}{center of the scale (mostly useful to calibrate
  center color of diverging scales).}
  \item{fit}{if set to \code{TRUE} then the data is shifted and scaled
  to fit the entire \code{lim} range.}
  \item{trans}{transformation of the resulting values. It can be
  either a function or one of the character strings \code{"id"},
  \code{"sin"} or \code{"asin"}. The transformation function may not
  return values larger than 1 or smaller than 0.}
  \item{na.col}{color (or value when used in \code{col.q}) to use for
  missing values, will be used as-is (e.g., alpha is not applied).}
  \item{FN}{function accepting three arguments \code{(a, b,
  alpha)}. \code{a} is mapped by a linear function descending from 1 to 0
  between \code{lim[1]} and \code{center}. \code{b} is a mapped by the
  correspondingly increasing function between \code{center} and
  \code{lim[2]}. \code{alpha} is passed from the original function call.}
}
\value{
  \code{col.base} returns the result of the \code{FN} function.

  \code{col.q} returns a vector of numeric values, all other functions
  return a vector of colors as created by the \code{rgb} function.
}
%\details{
%  The functions in this section allow to map data values to colors in
%  a contolled fashion.
%}
%\seealso{
%  \code{\link{.jcall}}, \code{\link{.jnull}}
%}
\examples{
 plot(0:10, rep(0, 11), ylim=c(0,5), cex=3, pch=19, col=col.bbr(0:10, fit=TRUE))
 points(0:10, rep(1,11), cex=3, pch=21, bg=col.br(0:10, fit=TRUE), col=1)
 points(0:10, rep(2,11), cex=3, pch=21, bg=col.bw(0:10, fit=TRUE), col=1)
 points(0:10, rep(3,11), cex=3, pch=21, bg=col.bwr(0:10, fit=TRUE, trans=sqrt),
  col=1)
 points(0:10, rep(4,11), cex=3, pch=21, bg=col.bwr(0:10, fit=TRUE), col=1)
 points(0:10, rep(5,11), cex=3, pch=21, bg=col.bwr(0:10, fit=TRUE,
  trans=function(x) x^2), col=1)
}
\keyword{interface}
