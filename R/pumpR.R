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


#' calculates the unit hydraulic gradient
#'
#' @param D Pipe diameter, in inches
#' @param C Hazen-Williams friction factor, unitless
#'
#' @return the hydraulic gradient per linear foot.
#' @export
#'
#' @examples
#' # Calculate the k for 1000 LF of 12-inch diameter pipe with C = 120
#' 1000*ke(12,120)
ke <- function(D,C){return(10.44/C^1.85/D^4.87)}


#' Calculate the equivalent hydraulic gradient (k) for parallel pipes
#'
#' @param k1 hydraulic gradient for a pipe segment
#' @param k2 hydraulic gradient for a pipe segment
#'
#' @return the equivalent hydraulic gradient (k) for parallel pipes
#' @export
#'
#' @examples
#' # Find the equiv k for 1,000 LF of twin 12-inch pipes at C=130
#' 2000*keq(ke(12,130),ke(12,130))
keq <- function(k1, k2) {
  k1_ <- (1/k1)^(1/1.85)
  k2_ <- (1/k2)^(1/1.85)
  k_ <- (k1_+k2_)^1.85
  return(1/k_)
}


# Planning Formulas -------------------------------------------------------


#' Calculates the peaking factor for a given ADF, in GPD, using the 10-state peaking factor method
#'
#' @param GPD Average Daily Flow (ADF), in GPD
#'
#' @return peaking factor, unitless
#' @export
#'
#' @examples
#' PF(1e6)
PF <- function(GPD) {
  MGD <- GPD/1e6

  PF <- (18+sqrt(10*MGD))/(4+sqrt(10*MGD))

  return(PF)

}


#' Peak Hour Flow (PHF)
#'
#' @param GPD Average Daily Flow (ADF), in GPD
#'
#' @return Peak Hour Flow (PHF), in GPM, using the 10-state peaking factor method
#' @export
#'
#' @examples
#' PHF(1e6)
PHF <- function(GPD) {
  PF <- PF(GPD)
  PHF <- GPD*PF/1440

  return(PHF)

}


#' Calculate Average Daily Flow (ADF) from Peak Hour Flow (PHF)
#'
#' @param GPM Peak Hour Flow (PHF), in GPM
#'
#' @return Average Daily Flow (ADF), in MGD, using 10-state peaking factor method
#' @export
#'
#' @examples
#' ADF(1000)
ADF <- function(GPM) {
  tmp <- uniroot(function(x) PHF(x)-GPM,c(0,1E9))$root/1e6

  return(tmp)
}

#' Calculate pipe velocity
#'
#' @param Q_gpm Pipe flow, in GPM
#' @param D_inches Pipe diameter, in inches
#'
#' @return pipe velocity, in FPS
#' @export
#'
#' @examples
#' v(1000,12)
v <- function(Q_gpm, D_inches) {
  v_ <- 0.408*Q_gpm/D_inches^2

  return(v_)

}


# DRAW functions ----------------------------------------------------------


#' Draws a system curve
#'
#' @param k hydraulic gradient
#' @param S static head
#' @param linecolor optional
#' @param linetype optional
#'
#' @return
#' @export
#'
#' @examples
#' Draw.Graph()
#' Draw.System.Curve(5000*ke(6,120),10)
Draw.System.Curve <- function(k, S, linecolor="red", linetype=1){
  Q <- seq(0,100000,10)
  HL <- k*Q^1.85+S
  lines(Q,HL,lwd=2, lty=linetype, col=linecolor)

}

#' Draw Pump Curve
#'
#' @param PumpCurve pump object from New.Pump()
#' @param numPumps Number of parallel pumps to draw
#' @param color optional
#' @param linetype optional
#'
#' @return
#' @export
#'
#' @examples
Draw.Pump <- function(PumpCurve, numPumps = 1, color = JEA.Blue, linetype=1){

  for (count in 1:numPumps) {
    lines(PumpCurve[,1]*count, PumpCurve[,2], lwd=2, lty=linetype, col=color)
  }

}

#'  Draws Text to Screen
#'
#' @param Q Flow, GPM
#' @param H Head, FT
#' @param text Text to display
#' @param highlight_color optional
#' @param text_color optional
#' @param TextSize optional
#'
#' @return
#' @export
#'
#' @examples
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

#' Draw duty point to graph
#'
#' @param Q Flow, GPM
#' @param H Head, FT
#' @param color optional
#'
#' @return
#' @export
#'
#' @examples
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


#' Draw Duty Point to Graph with label
#'
#' @param Q Flow, GPM
#' @param H Head, FT
#' @param color optional
#' @param DPLabel optional
#'
#' @return
#' @export
#'
#' @examples
Draw.Label.Point <- function (Q, H, color="red", DPLabel="Capacity") {
  Draw.Point(Q, H, color)
  Text_ <- paste0(DPLabel,"\n",round(Q,-1), " GPM @ ", round(H,0), " FT TDH")
  Text.Highlight(Q,H,Text_,,color)
}

#' Draws the framework for the pumpR style H-Q plot
#'
#' @param Max.Q   Maximum Flow (GPM)
#' @param Major.Q Major Flow Axis (GPM)
#' @param Minor.Q Minor Flow Axis (GPM)
#' @param Max.H   Maximum Head (FT)
#' @param Major.H Major Head Axis (FT)
#' @param Minor.H Minor Head Axis (FT)
#' @param AWRMP   Boolean, Add Planning Caveat to plot
#' @param MGD     Boolean, Add addition flow axis for MGD
#' @param TDH     Boolean, set y-axis label to FT TDH (TRUE) or FT (FALSE)
#'
#' @return None
#' @export
#' @seealso Draw.Pump
#' @examples
#' Draw.Graph(1000,200,50,250,50,10)
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

#' Draw Pump with VFD envelope
#'
#' @param PumpCurve pump object from New.Pump()
#' @param numPumps Number of parallel pumps to draw
#' @param color optional
#' @param linetype optional
#'
#' @return
#' @export
#'
#' @examples
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

# GET/SET/NEW functions -----------------------------------------------------------

#' get Head at Flow
#'
#' @param PumpCurve pump object from New.Pump()
#' @param Q Flow, GPM
#'
#' @return Head, in FT
#' @export
#'
#' @examples
Get.Head <- function(PumpCurve, Q) {
  if(sum(PumpCurve[,1]>0)){
    ret <- approx(PumpCurve, xout=Q)$y}
  else {
    ret <- 0}


  return(ret)
}

#' Get Flow from Head
#'
#' @param PumpCurve pump object from New.Pump()
#' @param H Head, in FT
#'
#' @return Flow, in GPM
#' @export
#'
#' @examples
Get.Flow <- function(PumpCurve, H) {approx(PumpCurve[,2], PumpCurve[,1], xout=H, rule=2)$y}

#' New Pump Object
#'
#' @param QH vector of flow and head by row
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#' New.Pump(c(
#'   0, 100,
#'  50,  80,
#' 100,  60,
#' 150,  40,
#' 200,  20
#' ))
New.Pump <- function (QH, n=1) {
  PumpCurve <- matrix(data=QH, ncol=2, byrow = TRUE)
  PumpCurve[,1] <- PumpCurve[,1]*n #number of pumps
  colnames(PumpCurve) <- c("Q", "H")
  pump <- as.data.frame(PumpCurve)

  return(pump)
}

#' Adjut pump curve for VFD speed
#'
#' @param pumpc pump object from New.Pumps
#' @param Hz VFD Speed, in Hz
#'
#' @return
#' @export
#'
#' @examples
VFD <- function(pumpc = pump, Hz) {
  tmpQ <- pumpc[,1]*Hz/60
  tmpH <- pumpc[,2]*(Hz/60)^2

  tmp <- cbind(tmpQ, tmpH)
  colnames(tmp) <- c("Q", "H")

  return(tmp)
}

#' Draw Enevlope of min.max system curve
#'
#' @param sc System Curve Object from Get.System.Curve()
#' @param linetype optional
#' @param color optional
#' @param max.Q optional
#'
#' @return
#' @export
#'
#' @examples
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

#' Draw Design Flow/Head lines (ICM Style)
#'
#' @param pump pump object from New.Pumps()
#' @param Q Target Flow, in GPM
#'
#' @return
#' @export
#'
#' @examples
Draw.Flow <- function(pump,Q) {
  H <- Get.Head(pump,Q)
  lines(c(0,Q,Q), c(H,H,0), lwd=2, col="orange")
}

# Graph Template -------------------------------------------

#' Draw Legend
#'
#' @param position location on graph, such as "top", "bottomleft", etc...
#' @param LineText Vector of text to be displayed
#' @param LineColor Vector of Line Colors
#' @param LineType Vector of Line Types
#' @param PointSymbol Vector of symbols (see pch)
#' @param LineWeight vector of Line weights
#'
#' @return
#' @export
#'
#' @examples
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

#' Calc System Curve
#'
#' @param Q Vector of flows
#' @param TDH Vectors of heads
#' @param min optional, minima quantile
#' @param max optional, minima quantile
#' @param min.flow min.flow for dynamic headloss calculation
#'
#' @return
#' @export
#'
#' @examples
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

#' Maximum Allowable Flow
#'
#' @param SC System curve object
#' @param MaxPSI maximum allowable Pressure
#'
#' @return
#' @export
#'
#' @examples
MAF <- function(SC, MaxPSI = 60) {
  Q <- ((MaxPSI*2.31-SC[2,2])/SC[2,1])^(1/1.85)

  return(Q)

}


