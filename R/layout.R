add.layout = function(x, y, w, h, rs = 0.01, as = 0.22, l = NULL) {
  add.one = function(l, x, y, w, h) {
    r = 0
    a = 0
    while (TRUE) {
      cx = x + sin(a) * r
      cy = y + cos(a) * r
      # cat("pos: ",x,",",y," par:",a,",",r," res:")
      # print(table(cx + w < l$x | x > l$x + l$w | cy + h < l$y | y > l$y + l$h))
      if (all(cx + w < l$x | cx > l$x + l$w | cy + h < l$y | cy > l$y + l$h)) {
        l$x = c(l$x, cx)
        l$y = c(l$y, cy)
        l$w = c(l$w, w)
        l$h = c(l$h, h)
        return(l)
      }
      r = r + rs
      a = a + as
    }
  }

  ml = max(c(length(x),length(y),length(w),length(h)))
  x = rep(x, length.out = ml)
  y = rep(y, length.out = ml)
  w = rep(w, length.out = ml)
  h = rep(h, length.out = ml)

  if (is.null(l)) {
    if (ml == 0L) return(NULL)
    l = structure(list(x=x[1], y=y[1], w=w[1], h=h[1]), class="box.layout")
    if (ml == 1L) return(l)
    x = x[-1L]
    y = y[-1L]
    w = w[-1L]
    h = h[-1L]
    ml = ml - 1L
  }
  for (i in  seq.int(ml))
    l <- add.one(l, x[i], y[i], w[i], h[i])
  l
}

add.labels = function(x, y, txt, w, h, adj = 0.5, mar = 0.1, ...) {
  if (missing(w)) w = strwidth(txt)
  if (missing(h)) h = strheight(txt)
  if (!missing(mar)) {
    w = w * (1 + mar)
    h = h * (1 + mar)
  }
  dx = adj[1] * w
  dy = ifelse(length(adj) > 1, adj[2], adj[1]) * h
  bx = x - dx
  by = y - dy
  l = add.layout(bx, by, w, h, ...)
  if (is.null(l$lx)) { ## lx not there so we're dealing with box.layout
    if (length(l$x) != length(dx))
      stop("add.labels() cannot be mixed with a previous output of add.layout()")
    l$lx = l$x + dx
    l$ly = l$y + dy
  } else { ## lx is there so it's label.layout - need to add lx,ly
    if (length(l$lx) + length(dx) != length(l$x))
      stop("sizes mismatch in the layout object - labels layout is inconsistent")
    l$lx = c(l$lx, l$x[-(1:length(l$lx))] + dx)
    l$ly = c(l$ly, l$y[-(1:length(l$ly))] + dy)
  }
  class(l) = c("label.layout", "box.layout")
  l
}
