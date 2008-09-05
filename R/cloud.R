cloud <- function(w, col, yspace=1.3, xspace=0.01, minh=0, ...) {
    if (missing(col)) col <- "#000000" # grey(0.8-(w-min(w))/((max(w)-min(w)))
    omar <- par("mar")
    par(mar=c(0,0,0,0))
    plot(0:1,0:1,type='n',axes=FALSE)
    x=0; y=0.95; xch=minh;
    cm=3/max(w)
    .<-lapply(1:length(w), function(i) {
     cex=w[i]*cm
     ctw=strwidth(names(w[i]),cex=cex)
     cth=strheight(names(w[i]),cex=cex)
     if (cth > xch) xch <<- cth
     if (x+ctw > 0.98) { x<<-0; y<<-y-(yspace*xch); xch<<-minh }
     text(x,y,names(w[i]),cex=cex,adj=c(0,0.5),col=col[i]) # grey(0.8-cex/4))
     x <<- x + ctw + xspace
    })
    par(omar)
    invisible(TRUE)
}
