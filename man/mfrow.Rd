\name{mfrow}
\alias{mfrow}
\alias{mfcol}
\title{
  Recursive grid layout functions for graphics
}
\description{
  \code{mfrow} and \code{mfcol} setup a multi-figure layout scheme on a
  grid in the current graphics device. Unlike other layout methods the
  schemes can be used recursively to create complex layouts with very
  simple commands.  
}
\usage{
mfrow(rows, cols, n, asp = 1, add = FALSE, times = NA, fig = if (add) par("fig") else c(0, 1, 0, 1), ...)
mfcol(rows, cols, n, asp = 1, add = FALSE, times = NA, fig = if (add) par("fig") else c(0, 1, 0, 1), ...)
}
\arguments{
  \item{rows}{either a numeric vector of length one defining the number
    of equally spaced rows or a vector specifying the relative height of
    each row.}
  \item{cols}{either a numeric vector of length one defining the number
    of equally spaced columns or a vector specifying the relative width of
    each row.}
  \item{n}{if \code{rows} and \code{cols} are both not specified, then
    \code{n} specifies the minimal number of cells that the layout
    should contain. In that case \code{rows} and \code{cols} are
    computed such that their product is at least \code{n} and the
    resulting aspect ratio of the figures is as close to \code{asp} as
    possible.}
  \item{asp}{numeric, desired aspect ratio of the figures when \code{n}
    is used to specify the grid}
  \item{add}{logical, if \code{TRUE} then the layout scheme is added to
    the layout stack, allowing recursive layouts. Otherwise the
    currently layout is replaced with the new grid layout.}
  \item{times}{number of times this layout should be applied or
    \code{NA} for unlimited. Any number larger than 1e6 is interpreted
    as \code{NA}.}
  \item{fig}{boundaries of the figure that will be split using this
    layout scheme.}
  \item{\dots}{additional arguments that will be passed to
    \code{\link{par}} when a new figure is setup.}
}
\details{
  \code{mfrow} and \code{mfcol} have a similar effect as the
  corresponding graphics parameters, but they are more flexible
  (allowing individual withd and heights) and construct a scheme that
  can be used recursively with \code{add = TRUE}.

  Note that the scheme layout method is incompatible with all other
  layout methods that are not based on the scheme stack, such as
  \code{par(mfrow)} or \code{\link{layout}}. It can be to a degree
  combined with \code{\link{split.screen}} with \code{add = TRUE} if
  used as a sub-layout thereof since \code{screen} uses a co-operative
  manipulation of figures. However, in that case you should make sure
  that the stack layout does not overflow (i.e., you plot only at most
  as many plots as there are figures) or it will clear the device.
}
\value{
  object of the class \code{scheme}.
}
\note{
  All layout functions require R 2.13.0 or higher! If plots seemingly
  don't move from the first figure then you either have old version or
  other code has removed the neessary "before.plot.new" hook.
}
%\references{
%}
%\author{
%}
%\seealso{
%}
%\examples{
%}
\keyword{hplot}
