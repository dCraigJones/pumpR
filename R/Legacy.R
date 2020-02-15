
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
