
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
