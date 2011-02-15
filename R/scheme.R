.scheme.env <- new.env(parent=emptyenv())

mfrow <- function(rows, cols, n, asp=1, add=FALSE, times=NA, fig = if(add) par("fig") else c(0,1,0,1), ...)
  .mfgrid(rows, cols, n, asp, add, times, fig, "gridRowScheme", ...)

mfcol <- function(rows, cols, n, asp=1, add=FALSE, times=NA, fig = if(add) par("fig") else c(0,1,0,1), ...)
  .mfgrid(rows, cols, n, asp, add, times, fig, "gridColScheme", ...)

.mfgrid <- function(rows, cols, n, asp=1, add=FALSE, times=NA, fig = if(add) par("fig") else c(0,1,0,1), subclass, ...) {
  if (missing(rows) && missing(cols) && !missing(n)) {
    t <- 1:as.integer(sqrt(n)) # compute all canidate sizes
    t <- unique(c(t, rev(as.integer(n / t + 0.999999))))
    din <- par("din") # adjust for device and figure aspect
    tasp <- rev(t) / t * din[1] / din[2] * (fig[2] - fig[1]) / (fig[4] - fig[3])
    a <- which.min(abs(log(tasp) - log(asp))) # find the closest ratio
    cols <- t[a]
    rows <- rev(t)[a]    
  }
  if (length(rows) == 1L) rows <- rep(1, rows)
  if (length(cols) == 1L) cols <- rep(1, cols)
  rows <- rows / sum(rows)
  cols <- cols / sum(cols)
  dev.id <- paste(names(dev.cur()), dev.cur(), sep='/')
  if (isTRUE(times > 1e6)) times <- NA
  if (!isTRUE(add)) {
    .scheme.env[[dev.id]] <- NULL
    .scheme.env[[paste(dev.id,"reset")]] <- TRUE
    fig <- c(0,1,0,1)
    par(mfrow=c(1,1))
  } else fig <- par("fig")

  pars <- list(...)
  if (!length(pars)) pars <- NULL
  scheme <- structure(list(rows=rows, cols=cols, crows = 1 - cumsum(c(0,rows)), ccols=cumsum(c(0,cols)),
                           times=times, pars=pars, name=paste(dev.id, "[", length(.scheme.env[[dev.id]]) + 1L, "]: ", subclass, " (", length(cols), " x ", length(rows), ")", sep=''),
                           fig=fig, index=1L, n=length(rows) * length(cols)),
                      class=c(subclass,"scheme"))
  .scheme.env[[dev.id]][[length(.scheme.env[[dev.id]]) + 1L]] <- scheme
  # init is TRUE if we setup a new scheme so that advance is not desired until next figure
  .scheme.env[[paste(dev.id,"init")]] <- TRUE
  # although the setup is done by the plot.new callback
  # it is important to setup the fig parameter now because
  # of subsequent calls to scheme routines before plot.new
  setup.figure(scheme)
  invisible(scheme)
}

# The returned value is the updated scheme
# setup should NOT make any advances as it may be called more than once for the same figure
# it should only set the fig parameter accordingly, no other side-effects are expected
setup.figure <- function(scheme) {
  if (!is.null(scheme$pars)) par(scheme$pars)
  UseMethod("setup.figure")
}

setup.figure.gridScheme <- function(scheme, row, col) {
  if (missing(row)) row <- scheme$row
  if (missing(col)) col <- scheme$col
  pf <- scheme$fig
  w <- pf[2] - pf[1]
  h <- pf[4] - pf[3]
  fig <- c(scheme$ccols[col], scheme$ccols[col + 1L],
           scheme$crows[row + 1L], scheme$crows[row])
  par(fig = c(pf[1] + fig[1] * w, pf[1] + fig[2] * w, pf[3] + fig[3] * h, pf[3] + fig[4] * h))
  scheme
}

setup.figure.gridRowScheme <- function(scheme) {
  index <- scheme$index
  cols <- length(scheme$cols)
  col <- 1L + ((index - 1L) %% cols)
  row <- 1L + as.integer((index - 1L) / cols)
  setup.figure.gridScheme(scheme, row, col)
}

setup.figure.gridColScheme <- function(scheme) {
  index <- scheme$index
  rows <- length(scheme$rows)
  row <- 1L + ((index - 1L) %% rows)
  col <- 1L + as.integer((index - 1L) / rows)
  setup.figure.gridScheme(scheme, row, col)
}

advance.scheme <- function() {
  dev.id <- paste(names(dev.cur()), dev.cur(), sep='/')
  schemes <- .scheme.env[[dev.id]]
  si <- length(schemes)
  if (si) {
    scheme <- schemes[[si]]
    if (!inherits(scheme, "scheme")) {
      warning("corrupted scheme, removing from stack")
      .scheme.env[[dev.id]] <- if (si > 1L) schemes[1:(si - 1L)] else NULL
      return(NULL)
    }
    scheme$index <- scheme$index + 1L
    if (scheme$index > scheme$n) { # end of scheme, reinstall?
      scheme$times <- scheme$times - 1L
      if (isTRUE(scheme$times < 1L)) { # times expired
        # remove from the stack
        .scheme.env[[dev.id]][[si]] <- NULL
        # and run advance one level up
        return(advance.scheme())
      }
      # re-install requested - need to go one level up
      .scheme.env[[dev.id]][[si]] <- NULL
      advance.scheme()
      scheme$fig <- par("fig")
      # check the length - if the parent quit, don't re-install
      if (length(.scheme.env[[dev.id]]) != si - 1L) return(NULL)
      scheme$index <- 1L
    }
    # prevent clearing of the device unless the next figure is first at root
    if (!isTRUE(.scheme.env[[paste(dev.id,"reset")]]))
      tryCatch(par(new = TRUE), error = function(e) TRUE)
    # ok, we're ready
    scheme <- setup.figure(scheme)
    # set state only after advancing in case there is an error
    .scheme.env[[dev.id]][[si]] <- scheme
  } else {
    # advance on the root = reset the plot
    par(fig = c(0,1,0,1))
    .scheme.env[[paste(dev.id,"reset")]] <- TRUE
    NULL
  }
}

pop.scheme <- function() {
  dev.id <- paste(names(dev.cur()), dev.cur(), sep='/')
  schemes <- .scheme.env[[dev.id]]
  si <- length(schemes)
  if (si) {
    .scheme.env[[dev.id]][[si]] <- NULL
    if (si > 1L) .scheme.env[[dev.id]][[si - 1L]] else NULL
  } else NULL
}

print.scheme <- function(x, ...)
  cat(x$name, ", index = ", x$index, "\n", sep='')

# currently we are the only module initializing stuff ...
.onLoad <- .First.lib <- function(libname, pkgname)
  setHook("before.plot.new", function() {
    dev.id <- paste(names(dev.cur()), dev.cur(), sep='/')
    if (length(.scheme.env[[dev.id]])) {
      if (!isTRUE(.scheme.env[[paste(dev.id,"init")]])) advance.scheme()
      .scheme.env[[paste(dev.id,"reset")]] <- FALSE
      .scheme.env[[paste(dev.id,"init")]] <- FALSE
    }
  })

