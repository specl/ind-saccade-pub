drawFrame <- function(cx, cy, half, col) {
  par(mar = c(0, 0, 1, 0))
  plot(c(0,1), 0:1, type = "n", axes = FALSE)
  rect(cx - half, cy - half, cx + half, cy + half, col = col)
}

addFrameLabels <- function(cx, cy, half, upper, lower) {
  text(cx + half, cy + half, upper, adj = -0.15)
  text(cx - half, cy - half, lower, adj =  1.15)
}

drawTrial1 <- function(half= 0.10,col= "antiquewhite"){

  addCue <- function(x, y, offsets = c(0.04, 0.02), 
                     lengths = c(0.01, 0.01)) {
    for (i in seq_along(offsets)) {
      o <- offsets[i]; l <- lengths[i]
      segments(x + o, y + o, x + o + l, y + o + l)}}  

  addTarget <- function(x, y, symbol, nudge = 0.05, size = 0.8) {
    text(x - nudge, y - nudge, symbol, cex = size)}  

    n=8
  base <- (1:n)/(n+1)
  cx=base*1
  cy <- rev(base)
  drawFrame(cx, cy, half, col)
  text(cx[1], cy[1], "+")
  addCue(cx[3], cy[3])
  addTarget(cx[5], cy[5], "M")
  addTarget(cx[6], cy[6], "#")
  addTarget(cx[7], cy[7], "@")
  upper=paste(as.character(round(1000*c(37,37,22,1,0,3,3,0)/75,1)),"ms")
  upper[8]=""
  upper[5]="Adjusted"
  lower <- rep("", n)
  lower[3] <- "Cue"
  lower[5] <- "Target"
  lower[6] <- "First Mask"
  lower[7] <- "Second Mask"
  addFrameLabels(cx, cy, half, upper, lower)
}

drawTrial2 <- function(half= 0.10,col= "antiquewhite"){

  addTarget <- function(x, y, txt, del=.046,...) {
    text(x=x-del,y=y,txt,adj=c(.5,.5),...)}  
  
  addBase <- function(x, y,del=.046,width=.025,cue=F){
    text(x, y, "+",adj=c(.5,.5))
    rect(x-width-del,y-width,x+width-del,y+width,lty=2)
    rect(x-width+del,y-width,x+width+del,y+width,lty=2)
    if (cue) rect(x-width-del,y-width,x+width-del,y+width,lty=1,lwd=5)
  }

  n=7
  base <- (1:n)/(n+1)
  cx=base*1
  cy <- rev(base)
  drawFrame(cx, cy, half, col)
  addBase(cx[1:6],cy[1:6])
  addBase(cx[2],cy[2],cue=T)
  addTarget(cx[4],cy[4],"S")
  addTarget(cx[5],cy[5],"#")
  addTarget(cx[6],cy[6],"@")
  upper=paste0(as.character(
    round(1000*c(100,2,0,16,16,16,0)/165,1)),"ms")
  upper[7]=""
  upper[3]="Adjusted"
  lower <- rep("", n)
  lower[2] <- "Cue"
  lower[4] <- "Target"
  lower[5] <- "First Mask"
  lower[6] <- "Second Mask"
  addFrameLabels(cx, cy, half, upper, lower)
}


