### R code from vignette source 'LSselect.Rnw'

###################################################
### code chunk number 1: LSselect.Rnw:21-22
###################################################
options(continue = " ", digits = 5)


###################################################
### code chunk number 2: LSselect.Rnw:40-42
###################################################
require("NMOF")
set.seed(112233)


###################################################
### code chunk number 3: LSselect.Rnw:64-69
###################################################
na <- 500L
C <- array(0.6, dim = c(na, na)); diag(C) <- 1
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols, Vols) * C


###################################################
### code chunk number 4: LSselect.Rnw:72-79
###################################################
OF <- function(x, data) {
    xx <- as.logical(x)
    w <- x/sum(x)
    res <-  crossprod(w[xx], data$Sigma[xx, xx])
    res <- tcrossprod(w[xx], res)
    res
}


###################################################
### code chunk number 5: LSselect.Rnw:82-87
###################################################
OF2 <- function(x, data) {
    xx <- as.logical(x); w <- 1/sum(x)
    res <- sum(w * w * data$Sigma[xx, xx])
    res
}


###################################################
### code chunk number 6: LSselect.Rnw:90-98
###################################################
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$na, data$nn, replace = FALSE)
    xn[p] <- abs(xn[p] - 1L)
    ## reject infeasible solution
    if((sum(xn) > data$Ksup) || (sum(xn) < data$Kinf))
        xc else xn
}


###################################################
### code chunk number 7: LSselect.Rnw:108-113
###################################################
data <- list(Sigma = Sigma,
              Kinf = 30L,
              Ksup = 60L,
                na = na,
                nn = 1L)


###################################################
### code chunk number 8: LSselect.Rnw:119-123
###################################################
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE)
assets <- sample.int(na, card0, replace = FALSE)
x0 <- numeric(na)
x0[assets] <- 1L


###################################################
### code chunk number 9: LSselect.Rnw:130-136
###################################################
## settings
algo <- list(x0 = x0,
      neighbour = neighbour,
             nS = 5000L,
    printDetail = FALSE,
       printBar = FALSE)


###################################################
### code chunk number 10: LSselect.Rnw:139-144
###################################################
system.time(sol1 <- LSopt(OF, algo, data))
sqrt(sol1$OFvalue)
par(ylog = TRUE, bty = "n", las = 1, tck = 0.01)
plot(sqrt(sol1$Fmat[,2L]),
     type = "l", xlab = "", ylab = "Portfolio volatility")


###################################################
### code chunk number 11: LSselect.Rnw:150-157
###################################################
nRuns <- 5L
allRes <- restartOpt(LSopt, n = nRuns, OF, algo = algo, data = data)
allResOF <- numeric(nRuns)
for (i in seq_len(nRuns))
    allResOF[i] <- sqrt(allRes[[i]]$OFvalue)
par(bty = "n")
plot(ecdf(allResOF), main = "Portfolio volatility")


