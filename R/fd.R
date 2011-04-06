fd <- function(x, ...) UseMethod("fd")

fd.matrix <- function(x, add=FALSE, vals=FALSE, at.x, at.y, axes=TRUE, frame.plot = FALSE, main = NULL, sub = NULL, xlab = NULL, ylab= NULL, zmax=max(x, na.rm=TRUE), xlim, ylim, asp = 1, panel.first = NULL, panel.last = NULL, ann = par("ann"), col="grey", border="black", ...) {
  localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
  localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
  localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
  localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
  force(zmax)
  m <- x
  x <- colnames(m)
  if (is.null(x)) x <- seq.int(ncol(m))
  if (missing(at.x)) at.x <- seq.int(x)
  y <- rownames(m)
  if (is.null(y)) y <- seq.int(nrow(m))
  if (missing(at.y)) at.y <- seq.int(y)
  if (missing(xlim)) xlim <- range(c(at.x + 0.5, at.x - 0.5))
  if (missing(ylim)) ylim <- range(c(at.y + 0.5, at.y - 0.5))

  ## go from dense to sparse
  xp <- rep(seq.int(ncol(m)), each=nrow(m))
  yp <- rep(seq.int(nrow(m)), rep=ncol(m))
  d <- data.frame(x=xp, y=yp, ct=as.vector(m), col=rep(col, length.out=length(xp)), stringsAsFactors=FALSE)
  d <- d[!is.na(d$ct),]
  d <- d[d$ct > 0,]
  d$r <- sqrt(d$ct) / sqrt(zmax) / 2

  # if (isTRUE(vals)) text(d$x, d$y, paste(format(100 * d$ct / sapply(d$x, function(x) sum(d$ct[d$x == x])), digits=3, drop0trailing=T),"%",sep=''), cex=v.cex)

  if (!add) {
    plot.new()
    localWindow(xlim, ylim, "", asp, ...)
    panel.first
  }
  
  rect(d$x - d$r, d$y - d$r, d$x + d$r, d$y + d$r, col=d$col, border=border, ...)

  if (!add) {
    panel.last
    if (axes) {
      localAxis(at = at.x, labels=x, side = 1, ...)
      localAxis(at = at.y, labels=y, side = 2, ...)
    }
    if (frame.plot)
      localBox(...)
    if (ann)
      localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }
  invisible(d)
}
