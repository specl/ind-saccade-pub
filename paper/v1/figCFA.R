
require(plotrix)
blank <- function(xlim=c(0,1),ylim=c(0,1)){
  par(mar = rep(0, 4))
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)}

obsNode <- function(x, y,  lab, w=.07, h=.05, cex=1) {
  mapply(function(x, y, lab) {
    rect(x-w, y-h, x+w, y+h, 
         col="white", border="black", lwd=1.5)
    text(x, y, lab, cex=cex)
  }, x, y, lab)
  invisible(NULL)
}

latentNode <- function(x, y, lab, rx=.08, ry=.05, cex=1) {
  mapply(function(x, y, lab) {
    draw.ellipse(x, y, a=rx, b=ry, col="white", border="black", lwd=1.5)
    text(x, y, lab, cex=cex)
  }, x, y, lab)
  invisible(NULL)
}

arr <- function(x0, y0, x1, y1, ...)
  arrows(x0, y0, x1, y1, length=.08, lwd=1.5, ...)

arrCurve <- function(x0, y0, x1, y1, bend=.15, n=100, ...) {
  mx <- (x0+x1)/2 + bend*(y1-y0)
  my <- (y0+y1)/2 - bend*(x1-x0)
  t  <- seq(0, 1, length=n)
  bx <- (1-t)^2*x0 + 2*(1-t)*t*mx + t^2*x1
  by <- (1-t)^2*y0 + 2*(1-t)*t*my + t^2*y1
  lines(bx, by, lwd=1.5, ...)
  arrows(bx[n-1], by[n-1], bx[n], by[n], length=.08, lwd=1.5) }

drawICMCFA=function(){
  blank(ylim=c(0,.4))
  # Positions
  x=(1:4)/5
  mid=c((x[1]+x[2])/2,(x[3]+x[4])/2)
  y=c(.1,.3)
  # Arrows
  X0=x
  X1=rep(mid,each=2)
  Y0=rep(y[1],4)
  Y1=rep(y[2],4)
  arr(X0,Y0,X1,Y1)
  arrCurve(mid[1],y[2],mid[2],y[2],bend=-.15)
  # Obs
  lab=c(paste0("A",1:2),paste0("P",1:2))
  obsNode(x,y=y[1],lab=lab,w=.08)
  # latent
  lab=c("Anti","Pro")
  latentNode(mid,y=y[2],lab=lab)}

#drawICMCFA()