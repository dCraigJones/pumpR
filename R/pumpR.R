# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Header ------------------------------------------------------------------

#setwd("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-33S West Grid - Lenox to Timuquana/2017-07-10 Lenox MPS 10p")
#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

#library(RColorBrewer)


# Standard JEA Colors
JEA.Dark <- rgb(t(matrix(c(20, 43, 108)/255)))
JEA.Blue <- rgb(t(matrix(c(0, 106, 151)/255)))
JEA.Green <- rgb(t(matrix(c(65, 173, 73)/255)))
JEA.Orange <- rgb(t(matrix(c(244, 199, 33)/255)))
JEA.Grey <- rgb(t(matrix(c(109, 110, 113)/255)))

# Pipe Headloss Formulas --------------------------------------------------

Le.CV <- 60
Le.GV <- 8
Le.90d <- 20
Le.45d <- 16
Le.22d <- 13
Le.11d <- 10
Le.tee.thru <- 20
Le.tee.branch <- 60

ke <- function(D,C){return(10.44/C^1.85/D^4.87)}

keq <- function(k1, k2) {
  k1_ <- (1/k1)^(1/1.85)
  k2_ <- (1/k2)^(1/1.85)
  k_ <- (k1_+k2_)^1.85
  return(1/k_)
}


# Planning Formulas -------------------------------------------------------


PF <- function(GPD) {
  MGD <- GPD/1e6

  PF <- (18+sqrt(10*MGD))/(4+sqrt(10*MGD))

  return(PF)

}

PHF <- function(GPD) {
  PF <- PF(GPD)
  PHF <- GPD*PF/1440

  return(PHF)

}

ADF <- function(GPM) {
  tmp <- uniroot(function(x) PHF(x)-GPM,c(0,1E9))$root/1e6

  return(tmp)
}

v <- function(Q_gpm, D_inches) {
  v_ <- 0.408*Q_gpm/D_inches^2

  return(v_)

}


# DRAW functions ----------------------------------------------------------


Draw.System.Curve <- function(k, S, linecolor="red", linetype=1){
  Q <- seq(0,100000,10)
  HL <- k*Q^1.85+S
  lines(Q,HL,lwd=2, lty=linetype, col=linecolor)

}

Draw.Pump <- function(PumpCurve, numPumps = 1, color = JEA.Blue, linetype=1){

  for (count in 1:numPumps) {
    lines(PumpCurve[,1]*count, PumpCurve[,2], lwd=2, lty=linetype, col=color)
  }

}

Merge.Pumps.VFD <- function (
  pump1 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump2 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump3 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump4 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump5 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump6 = matrix(c(0,1,0,1),ncol=2, byrow=F)
) {

  H <- seq(200,0,-1)
  Q1 <- approx(pump1[,2], pump1[,1], xout=H)$y
  Q2 <- approx(pump2[,2], pump2[,1], xout=H)$y
  Q3 <- approx(pump3[,2], pump3[,1], xout=H)$y
  Q4 <- approx(pump4[,2], pump4[,1], xout=H)$y
  Q5 <- approx(pump5[,2], pump5[,1], xout=H)$y
  Q6 <- approx(pump6[,2], pump6[,1], xout=H)$y

  Q1[is.na(Q1)]=0
  Q2[is.na(Q2)]=0
  Q3[is.na(Q3)]=0
  Q4[is.na(Q4)]=0
  Q5[is.na(Q5)]=0
  Q6[is.na(Q6)]=0

  Q1 <- Q1
  Q2 <- Q1+Q2
  Q3 <- Q2+Q3
  Q4 <- Q3+Q4
  Q5 <- Q4+Q5
  Q6 <- Q5+Q6

  Q1 <- Q1*if(is.null(colnames(pump1))) 0 else 1
  Q2 <- Q2*if(is.null(colnames(pump2))) 0 else 1
  Q3 <- Q3*if(is.null(colnames(pump3))) 0 else 1
  Q4 <- Q4*if(is.null(colnames(pump4))) 0 else 1
  Q5 <- Q5*if(is.null(colnames(pump5))) 0 else 1
  Q6 <- Q6*if(is.null(colnames(pump6))) 0 else 1

  P1 <- matrix(c(Q1,H), ncol=2, byrow=F)
  P2 <- matrix(c(Q2,H), ncol=2, byrow=F)
  P3 <- matrix(c(Q3,H), ncol=2, byrow=F)
  P4 <- matrix(c(Q4,H), ncol=2, byrow=F)
  P5 <- matrix(c(Q5,H), ncol=2, byrow=F)
  P6 <- matrix(c(Q6,H), ncol=2, byrow=F)

  P1.min <- match(min(P1[P1[,1]>0,1]), P1[,1])-1
  P1.max <- match(max(P1[,1]), P1[,1])
  P2.min <- match(min(P2[P2[,1]>0,1]), P2[,1])-1
  P2.max <- match(max(P2[,1]), P2[,1])
  P3.min <- match(min(P3[P3[,1]>0,1]), P3[,1])-1
  P3.max <- match(max(P3[,1]), P3[,1])
  P4.min <- match(min(P4[P4[,1]>0,1]), P4[,1])-1
  P4.max <- match(max(P4[,1]), P4[,1])
  P5.min <- match(min(P5[P5[,1]>0,1]), P5[,1])-1
  P5.max <- match(max(P5[,1]), P5[,1])
  P6.min <- match(min(P6[P6[,1]>0,1]), P6[,1])-1
  P6.max <- match(max(P6[,1]), P6[,1])


  if(!is.na(P1.min)) P1 <- P1[P1.min:P1.max,]
  if(!is.na(P2.min)) P2 <- P2[P2.min:P2.max,]
  if(!is.na(P3.min)) P3 <- P3[P3.min:P3.max,]
  if(!is.na(P4.min)) P4 <- P4[P4.min:P4.max,]
  if(!is.na(P5.min)) P5 <- P5[P5.min:P5.max,]
  if(!is.na(P6.min)) P6 <- P6[P6.min:P6.max,]

  Draw.VFD(P1)
  Draw.VFD(P2)
  Draw.VFD(P3)
  Draw.VFD(P4)
  Draw.VFD(P5)
  Draw.VFD(P6)

}

Merge.Pumps <- function (
  pump1 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump2 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump3 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump4 = matrix(c(0,0,0,0),ncol=2, byrow=F),
  pump5 = matrix(c(0,0,0,0),ncol=2, byrow=F),
  pump6 = matrix(c(0,0,0,0),ncol=2, byrow=F)
) {

  H <- seq(200,0,-1)

  if(max(pump1)==0) {Q1 <- NA} else {Q1 <- approx(pump1[,2], pump1[,1], xout=H)$y}
  if(max(pump2)==0) {Q2 <- NA} else {Q2 <- approx(pump2[,2], pump2[,1], xout=H)$y}
  if(max(pump3)==0) {Q3 <- NA} else {Q3 <- approx(pump3[,2], pump3[,1], xout=H)$y}
  if(max(pump4)==0) {Q4 <- NA} else {Q4 <- approx(pump4[,2], pump4[,1], xout=H)$y}
  if(max(pump5)==0) {Q5 <- NA} else {Q5 <- approx(pump5[,2], pump5[,1], xout=H)$y}
  if(max(pump6)==0) {Q6 <- NA} else {Q6 <- approx(pump6[,2], pump6[,1], xout=H)$y}

  Q1[is.na(Q1)]=0
  Q2[is.na(Q2)]=0
  Q3[is.na(Q3)]=0
  Q4[is.na(Q4)]=0
  Q5[is.na(Q5)]=0
  Q6[is.na(Q6)]=0

  Q1 <- Q1
  Q2 <- Q1+Q2
  Q3 <- Q2+Q3
  Q4 <- Q3+Q4
  Q5 <- Q4+Q5
  Q6 <- Q5+Q6

  Q1 <- Q1*if(is.null(colnames(pump1))) 0 else 1
  Q2 <- Q2*if(is.null(colnames(pump2))) 0 else 1
  Q3 <- Q3*if(is.null(colnames(pump3))) 0 else 1
  Q4 <- Q4*if(is.null(colnames(pump4))) 0 else 1
  Q5 <- Q5*if(is.null(colnames(pump5))) 0 else 1
  Q6 <- Q6*if(is.null(colnames(pump6))) 0 else 1

  P1 <- matrix(c(Q1,H), ncol=2, byrow=F)
  P2 <- matrix(c(Q2,H), ncol=2, byrow=F)
  P3 <- matrix(c(Q3,H), ncol=2, byrow=F)
  P4 <- matrix(c(Q4,H), ncol=2, byrow=F)
  P5 <- matrix(c(Q5,H), ncol=2, byrow=F)
  P6 <- matrix(c(Q6,H), ncol=2, byrow=F)

  P1.min <- match(min(P1[P1[,1]>0,1]), P1[,1])-1
  P1.max <- match(max(P1[,1]), P1[,1])
  P2.min <- match(min(P2[P2[,1]>0,1]), P2[,1])-1
  P2.max <- match(max(P2[,1]), P2[,1])
  P3.min <- match(min(P3[P3[,1]>0,1]), P3[,1])-1
  P3.max <- match(max(P3[,1]), P3[,1])
  P4.min <- match(min(P4[P4[,1]>0,1]), P4[,1])-1
  P4.max <- match(max(P4[,1]), P4[,1])
  P5.min <- match(min(P5[P5[,1]>0,1]), P5[,1])-1
  P5.max <- match(max(P5[,1]), P5[,1])
  P6.min <- match(min(P6[P6[,1]>0,1]), P6[,1])-1
  P6.max <- match(max(P6[,1]), P6[,1])


  if(!is.na(P1.min)) P1 <- P1[P1.min:P1.max,]
  if(!is.na(P2.min)) P2 <- P2[P2.min:P2.max,]
  if(!is.na(P3.min)) P3 <- P3[P3.min:P3.max,]
  if(!is.na(P4.min)) P4 <- P4[P4.min:P4.max,]
  if(!is.na(P5.min)) P5 <- P5[P5.min:P5.max,]
  if(!is.na(P6.min)) P6 <- P6[P6.min:P6.max,]

  #
  # lines(P1, lwd=2, lty=1, col=JEA.Blue)
  # lines(P2, lwd=2, lty=1, col=JEA.Blue)
  # lines(P3, lwd=2, lty=1, col=JEA.Blue)
  # lines(P4, lwd=2, lty=1, col=JEA.Blue)
  # lines(P5, lwd=2, lty=1, col=JEA.Blue)
  # lines(P6, lwd=2, lty=1, col=JEA.Blue)
}

Text.Highlight <- function(Q,H,text,highlight_color="white", text_color="red", TextSize=0.75) {

  lims <- par("usr")
  scale <- dev.size()
  scale <- scale[1]/scale[2]


  Q_offset <- (lims[2]-lims[1])*0.001
  H_offset <- (lims[4]-lims[3])*0.001*scale

  text(Q+Q_offset, H-H_offset, text, col=highlight_color, pos=4, font=2, cex=TextSize)
  text(Q+Q_offset, H+H_offset, text, col=highlight_color, pos=4, font=2, cex=TextSize)
  text(Q-Q_offset, H-H_offset, text, col=highlight_color, pos=4, font=2, cex=TextSize)
  text(Q-Q_offset, H+H_offset, text, col=highlight_color, pos=4, font=2, cex=TextSize)
  text(Q, H, text, col=text_color, pos=4, font=2, cex=TextSize)

}

User.Text.Highlight <- function(text,pos=4,highlight_color="white", text_color="red", TextSize=0.75) {
  loc <- locator(1)

  Q <- loc$x
  H <- loc$y

  lims <- par("usr")
  scale <- dev.size()
  scale <- scale[1]/scale[2]


  Q_offset <- (lims[2]-lims[1])*0.001
  H_offset <- (lims[4]-lims[3])*0.001*scale

  text(Q+Q_offset, H-H_offset, text, col=highlight_color, pos=pos, font=2, cex=TextSize)
  text(Q+Q_offset, H+H_offset, text, col=highlight_color, pos=pos, font=2, cex=TextSize)
  text(Q-Q_offset, H-H_offset, text, col=highlight_color, pos=pos, font=2, cex=TextSize)
  text(Q-Q_offset, H+H_offset, text, col=highlight_color, pos=pos, font=2, cex=TextSize)
  text(Q, H, text, col=text_color, pos=pos, font=2, cex=TextSize)

}

Draw.Point <- function (Q, H, color="red") {
  Fill <- 2/3

  lims <- par("usr")
  scale <- dev.size()
  scale <- scale[1]/scale[2]


  L.Q <- (lims[2]-lims[1])*0.025
  L.H <- (lims[4]-lims[3])*0.025*scale

  lines(c(Q-L.Q,Q),c(H,H), lwd=2, col=color)
  lines(c(Q,Q),c(H,H-L.H), lwd=2, col=color)
  xx <- c(Q-L.Q*Fill,Q,Q, Q-L.Q*Fill)
  yy <- c(H,H,H-L.H*Fill,H)
  polygon(xx,yy, col=color, density=30, border=NA, angle=-45)
}


Draw.Label.Point <- function (Q, H, color="red", DPLabel="Capacity") {
  Draw.Point(Q, H, color)
  Text_ <- paste0(DPLabel,"\n",round(Q,-1), " GPM @ ", round(H,0), " FT TDH")
  Text.Highlight(Q,H,Text_,,color)
}

Draw.Graph <- function(Max.Q=15000, Major.Q=1500,Minor.Q=250, Max.H=250,Major.H=50,Minor.H=10,AWRMP=FALSE,MGD=FALSE, TDH=TRUE){
  #Max.Q <- 15000
  #Max.H <- 250

  #Major.Q <- 1500
  #Minor.Q <- 250
  #Major.H <- 50
  #Minor.H <- 10

  Max.P <- floor(Max.H/2.31/10)*10
  Major.P <- max(floor(Major.H/2.31/10),1)*10
  Minor.P <- max(floor(Minor.H/2.31),1)

  MGD.Step <- 10

  Max.C <- floor(Max.Q/694.44/MGD.Step)*MGD.Step
  Major.C <- max(floor(Major.Q/694.44/MGD.Step),1)*MGD.Step
  Minor.C <- max(floor(Minor.Q/694.44/2),1)*2

  #Station.Address <- "Rampart Rd - 0 BPS"
  #  Pump.Information <- ""#"Flygt N3301.185; 85 HP; 1185 RPM; 624mm"


  # Initialize graph
  par(mar=c(6,5,3,3.25)) #Btm, Left, Top, Right

  plot(0, 0, type="n", axes=FALSE, frame.plot=TRUE, xlim=c(0,Max.Q), ylim=c(0,Max.H), xaxs="i", yaxs="i", xlab=NA, ylab=NA)

  if(TDH)
    mtext("Head (FT TDH)",2, line=2 )
  else
    mtext("Head (FT)",2, line=2)


  # Draw minor axes
  abline(h=seq(0,Max.H, Minor.H), col="grey85", lty=2)
  abline(v=seq(0,Max.Q, Minor.Q), col="grey85", lty=2)

  # Draw major axes
  abline(h=seq(0,Max.H,Major.H), col="grey75")
  abline(v=seq(0,Max.Q,Major.Q), col="grey75")

  # Axis labels
  axis(2, at=seq(000,Max.H,Minor.H),  tcl=-0.25, lwd=1, labels=NA) #minor ticks
  axis(2, at=seq(000,Max.H,Major.H), cex.axis=0.85, lwd=2) #major ticks

  axis(1, at=seq(0,Max.Q,Minor.Q),  tcl=-0.25, lwd=1, labels=NA) #minor ticks
  axis(1, at=seq(0,Max.Q,Major.Q), cex.axis=0.85, lwd=2, labels=c(0,prettyNum(seq(Major.Q,Max.Q,Major.Q), big.mark=","))) #major ticks


  axis(4, at=seq(0,Max.H, Minor.P*2.31),  tcl=-0.325, lwd=1, labels=NA)
  axis(4, at=seq(0,Max.H, Major.P*2.31), cex.axis=0.85, lwd=2, labels=seq(0,Max.P,Major.P)) #major ticks
  mtext("(PSI)", side=4, line=2)

  if(MGD)
  {
    #Secondary X-axis
    axis(1, at=seq(0,Max.C*694.44,Minor.C*694.44),  tcl=-0.25, lwd=1, line=3, labels=NA) #minor ticks
    axis(1, at=seq(0,Max.C*694.44,Major.C*694.44), cex.axis=0.85, tcl=-.5,lwd=2, line=3, labels=c(0,prettyNum(seq(MGD.Step,Max.C,Major.C), big.mark=","))) #major ticks
    mtext("(MGD)",1,line=4,at=-Minor.Q)
    mtext("(GPM)",1,line=1,at=-Minor.Q)
  }
  else {mtext("Flow (GPM)",1,line=3)}


  box(which="plot", lty="solid", lwd=2)

  # title(Station.Address)
  # title(sub=Pump.Information, font.sub=3, cex.sub=0.85)

  if(AWRMP) {
    mtext("This document is for JEA planning purposes only and should not be used for design.",side=3, line=0, cex=0.50, font=3)
    mtext(Sys.Date(),side=3, line=2, cex=0.5, adj=1)
  }

}

Draw.VFD <- function(PumpCurve, numPumps = 1, color = JEA.Blue, linetype=1){
  color.light = paste0(color,"33")

  pump <- as.data.frame(PumpCurve)

  for (count in 1:numPumps) {
    pump.old <- pump

    PumpCurve[,1] <- pump[,1]*count #number of pumps
    colnames(PumpCurve) <- c("Q", "H")
    pump <- as.data.frame(PumpCurve)

    n <- length(VFD(PumpCurve,60)[,1])
    pp <- VFD(PumpCurve,35)
    pp <- rbind(pp,VFD(PumpCurve,35)[n,])
    pp <- rbind(pp,VFD(PumpCurve,40)[n,])
    pp <- rbind(pp,VFD(PumpCurve,45)[n,])
    pp <- rbind(pp,VFD(PumpCurve,50)[n,])
    pp <- rbind(pp,VFD(PumpCurve,55)[n,])
    pp <- rbind(pp,VFD(PumpCurve,60)[n,])
    pp <- rbind(pp,VFD(PumpCurve,60)[n:1,])
    pp <- rbind(pp,VFD(PumpCurve,35)[1,])

    polygon(pp, col=color.light, border=NA)
    lines(VFD(PumpCurve,60), lwd=2, lty=linetype, col=color)
    lines(VFD(PumpCurve,35), lwd=2, lty=linetype, col=color)

    pump <- pump.old
  }

}


# COMBINE functions -------------------------------------------------------


Combine.Pumps <- function(pump1, pump2) {
  MaxH <- 300

  H <- seq(300,0,-1)

  Q1 <- approx(pump1[,2], pump1[,1], xout=H)$y
  Q2 <- approx(pump2[,2], pump2[,1], xout=H)$y

  Q <- Q1 + Q2

  pump <- matrix(c(Q,H), ncol=2,byrow=FALSE)
  colnames(pump) <- c("Q","H")

  return(pump)
}

Derate.Pump <- function(pump,k,S) {
  pump.Q <- pump[,1]
  pump.H <- pump[,2] - k*pump.Q^1.85 - S

  dpump <- matrix(c(pump.Q, pump.H), ncol=2, byrow=FALSE)
  colnames(dpump) <- c("Q", "H")

  return(dpump)
}

Get.Intercept <- function(pump,k,S) {
  dpump <- Derate.Pump(pump,k,S)
  Q <- Get.Flow(dpump,0)
  H <- Get.Head(pump,Q)

  point <- t(as.matrix(c(Q,H)))
  colnames(point) <- c("Q", "H")

  return(c(Q,H))
}

Get.2p.System.Curve <- function(Q,H,S) {return((H-S)/Q^1.85)}

Two.Pump.System <- function(pump1, k1, S1, pump2, k2, S2, kT, ST) {

  dp1 <- Derate.Pump(pump1, k1, S1)
  dp2 <- Derate.Pump(pump2, k2, S2)
  dpT <- Combine.Pumps(dp1, dp2)
  H <- Get.Intercept(dpT, kT, ST)[2]
  Q1 <- Get.Flow(dp1, H)
  Q2 <- Get.Flow(dp2, H)
  H1 <- Get.Head(pump1,Q1)
  H2 <- Get.Head(pump2,Q2)
  M1 <- kT*Q2^1.85
  M2 <- kT*Q1^1.85
  S1 <- S1 + M1
  S2 <- S2 + M2
  rk1 <- Get.2p.System.Curve(Q1, H1, S1)
  rk2 <- Get.2p.System.Curve(Q2, H2, S2)

  summary <- matrix(c(Q1, H1, rk1, S1, Q2, H2, rk2, S2), ncol=4, byrow=TRUE)
  colnames(summary) <- c("Q", "H", "k", "S")
  rownames(summary) <- c("pump1", "pump2")

  return(summary)
}

Add.Pumps <- function (
  pump1 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump2 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump3 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump4 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump5 = matrix(c(0,1,0,1),ncol=2, byrow=F),
  pump6 = matrix(c(0,1,0,1),ncol=2, byrow=F)
) {

  H <- seq(300,0,-1)
  Q1 <- approx(pump1[,2], pump1[,1], xout=H)$y
  Q2 <- approx(pump2[,2], pump2[,1], xout=H)$y
  Q3 <- approx(pump3[,2], pump3[,1], xout=H)$y
  Q4 <- approx(pump4[,2], pump4[,1], xout=H)$y
  Q5 <- approx(pump5[,2], pump5[,1], xout=H)$y
  Q6 <- approx(pump6[,2], pump6[,1], xout=H)$y

  Q1[is.na(Q1)]=0
  Q2[is.na(Q2)]=0
  Q3[is.na(Q3)]=0
  Q4[is.na(Q4)]=0
  Q5[is.na(Q5)]=0
  Q6[is.na(Q6)]=0

  Q1 <- Q1
  Q2 <- Q1+Q2
  Q3 <- Q2+Q3
  Q4 <- Q3+Q4
  Q5 <- Q4+Q5
  Q6 <- Q5+Q6

  Q1 <- Q1*if(is.null(colnames(pump1))) 0 else 1
  Q2 <- Q2*if(is.null(colnames(pump2))) 0 else 1
  Q3 <- Q3*if(is.null(colnames(pump3))) 0 else 1
  Q4 <- Q4*if(is.null(colnames(pump4))) 0 else 1
  Q5 <- Q5*if(is.null(colnames(pump5))) 0 else 1
  Q6 <- Q6*if(is.null(colnames(pump6))) 0 else 1

  P1 <- matrix(c(Q1,H), ncol=2, byrow=F)
  P2 <- matrix(c(Q2,H), ncol=2, byrow=F)
  P3 <- matrix(c(Q3,H), ncol=2, byrow=F)
  P4 <- matrix(c(Q4,H), ncol=2, byrow=F)
  P5 <- matrix(c(Q5,H), ncol=2, byrow=F)
  P6 <- matrix(c(Q6,H), ncol=2, byrow=F)

  #   P1.min <- match(min(P1[P1[,1]>0,1]), P1[,1])-1
  #   P1.max <- match(max(P1[,1]), P1[,1])
  #   P2.min <- match(min(P2[P2[,1]>0,1]), P2[,1])-1
  #   P2.max <- match(max(P2[,1]), P2[,1])
  #   P3.min <- match(min(P3[P3[,1]>0,1]), P3[,1])-1
  #   P3.max <- match(max(P3[,1]), P3[,1])
  #   P4.min <- match(min(P4[P4[,1]>0,1]), P4[,1])-1
  #   P4.max <- match(max(P4[,1]), P4[,1])
  #   P5.min <- match(min(P5[P5[,1]>0,1]), P5[,1])-1
  #   P5.max <- match(max(P5[,1]), P5[,1])
  #   P6.min <- match(min(P6[P6[,1]>0,1]), P6[,1])-1
  #   P6.max <- match(max(P6[,1]), P6[,1])
  #
  #
  #   if(!is.na(P1.min)) P1 <- P1[P1.min:P1.max,]
  #   if(!is.na(P2.min)) P2 <- P2[P2.min:P2.max,]
  #   if(!is.na(P3.min)) P3 <- P3[P3.min:P3.max,]
  #   if(!is.na(P4.min)) P4 <- P4[P4.min:P4.max,]
  #   if(!is.na(P5.min)) P5 <- P5[P5.min:P5.max,]
  #   if(!is.na(P6.min)) P6 <- P6[P6.min:P6.max,]

  return(P4)
  #
  #   lines(P1, lwd=2, lty=1, col=JEA.Blue)
  #   lines(P2, lwd=2, lty=1, col=JEA.Blue)
  #   lines(P3, lwd=2, lty=1, col=JEA.Blue)
  #   lines(P4, lwd=2, lty=1, col=JEA.Blue)
  #   lines(P5, lwd=2, lty=1, col=JEA.Blue)
  #   lines(P6, lwd=2, lty=1, col=JEA.Blue)
}


# GET/SET/NEW functions -----------------------------------------------------------

Get.Head <- function(PumpCurve, Q) {
  if(sum(PumpCurve[,1]>0)){
    ret <- approx(PumpCurve, xout=Q)$y}
  else {
    ret <- 0}


  return(ret)
}

Get.Flow <- function(PumpCurve, H) {approx(PumpCurve[,2], PumpCurve[,1], xout=H, rule=2)$y}

New.Pump <- function (QH, n=1) {
  PumpCurve <- matrix(data=QH, ncol=2, byrow = TRUE)
  PumpCurve[,1] <- PumpCurve[,1]*n #number of pumps
  colnames(PumpCurve) <- c("Q", "H")
  pump <- as.data.frame(PumpCurve)

  return(pump)
}

VFD <- function(pumpc = pump, Hz) {
  tmpQ <- pumpc[,1]*Hz/60
  tmpH <- pumpc[,2]*(Hz/60)^2

  tmp <- cbind(tmpQ, tmpH)
  colnames(tmp) <- c("Q", "H")

  return(tmp)
}

Draw.System.Envelope <- function(sc, linetype=1, color=JEA.Grey, max.Q = 5000) {

  color.light = paste0(color,"22")

  Q <- seq(0,max.Q,10)
  H1 <- sc[1,1]*Q^1.85+sc[1,2]
  H2 <- sc[2,1]*Q^1.85+sc[2,2]
  xx <- c(Q,rev(Q))
  yy <- c(H1,rev(H2))

  polygon(xx,yy, col=color.light, border=NA)
  lines(Q,H1, lwd=2, lty=linetype, col=color)
  lines(Q,H2, lwd=2, lty=linetype, col=color)
}

Draw.Flow <- function(pump,Q) {
  H <- Get.Head(pump,Q)
  lines(c(0,Q,Q), c(H,H,0), lwd=2, col="orange")
}

# Graph Template -------------------------------------------

Draw.Legend <- function(position="bottomleft",LineText, LineColor, LineType, PointSymbol=rep(NA,length(LineType)), LineWeight=rep(2,length(LineType))) {
  legend(position
         , LineText#, c("A: ROOSEVELT BV 1013' N OF 120TH ST (87049)", "A corrected to B location (C=90)", "B: GOLDEN WINGS RD 283' W OF NORMAN ST (242321)", "A corrected to furthest node (C=135)")
         , lwd=LineWeight
         , lty=LineType#c(1,2,1,2)
         , col=LineColor#c(JEA.Green, JEA.Green, JEA.Blue, "red")
         , inset=c(0.05,0.05)
         , seg.len = 4
         , pch=PointSymbol
         , cex=0.8
         , y.intersp=0.8
         , bty = "n"
         , box.col=rgb(1,1,1,0.75)
         , bg=rgb(1,1,1,0.75)
         , horiz=FALSE#TRUE
         , text.font=2
  )
}

# LEGACY functions --------------------------------------------------------

VFD_Envelope <- function(numPumps = 1, color = JEA.Blue, linetype=1){
  color.light = paste0(color,"33")

  for (count in 1:numPumps) {
    pump.old <- pump

    PumpCurve[,1] <- pump[,1]*count #number of pumps
    colnames(PumpCurve) <- c("Q", "H")
    pump <- as.data.frame(PumpCurve)

    n <- length(VFD(PumpCurve,60)[,1])
    pp <- VFD(PumpCurve,35)
    pp <- rbind(pp,VFD(PumpCurve,35)[n,])
    pp <- rbind(pp,VFD(PumpCurve,40)[n,])
    pp <- rbind(pp,VFD(PumpCurve,45)[n,])
    pp <- rbind(pp,VFD(PumpCurve,50)[n,])
    pp <- rbind(pp,VFD(PumpCurve,55)[n,])
    pp <- rbind(pp,VFD(PumpCurve,60)[n,])
    pp <- rbind(pp,VFD(PumpCurve,60)[n:1,])
    pp <- rbind(pp,VFD(PumpCurve,35)[1,])

    polygon(pp, col=color.light, border=NA)
    lines(VFD(PumpCurve,60), lwd=2, lty=linetype, col=color)
    lines(VFD(PumpCurve,35), lwd=2, lty=linetype, col=color)

    pump <- pump.old
  }

}


Get.System.Curve <- function(Q,TDH,min=0.05, max=0.95, min.flow=10) {
  H <- TDH[Q<min.flow]
  H[H<0] <- NA
  H <- na.omit(H)

  S <- quantile(H,max)
  if(is.na(S)) {S <- 0}
  k_ <- (TDH-S)/(Q^1.85)
  k_[Q<min.flow] <- NA
  k_ <- na.omit(k_)

  S.max <- S
  k.max <- quantile(k_,0.95)

  S <- quantile(H,min)
  if(is.na(S)) {S <- 0}
  k_ <- (TDH-S)/(Q^1.85)
  k_[Q<min.flow] <- NA
  k_ <- na.omit(k_)

  S.min <- S
  k.min <- quantile(k_,min)

  tbl <- matrix(c(k.min,S.min,k.max,S.max), nrow=2, byrow=T)
  colnames(tbl) <- c("k", "S")
  rownames(tbl) <- c("min","max")

  return(tbl)
}

MAF <- function(SC, MaxPSI = 60) {
  Q <- ((MaxPSI*2.31-SC[2,2])/SC[2,1])^(1/1.85)

  return(Q)

}


