### R code from vignette source 'portfolio.Rnw'

###################################################
### code chunk number 1: portfolio.Rnw:21-31
###################################################
options(continue = "  ", digits = 3, width = 60, useFancyQuotes = FALSE,
        max.print = 1000, width = 65)
pv <- packageVersion("NMOF")
pv <- gsub("(.*)[.](.*)", "\\1-\\2", pv)
if (!requireNamespace("quadprog", quietly = TRUE))
    trackingPortfolio <- mvFrontier <-
    mvPortfolio <- minvar <- function(...) {
        cat("package", sQuote("quadprog"), " is required")
        invisible(NULL)
    }


###################################################
### code chunk number 2: portfolio.Rnw:52-53
###################################################
library("NMOF")


###################################################
### code chunk number 3: portfolio.Rnw:58-72
###################################################
var <- structure(
    c(0.000988087100677907, -0.0000179669410403153, 0.000368923882626859,
      0.000208303611101873, 0.000262742052359594, -0.0000179669410403153,
      0.00171852167358765, 0.0000857467457561209, 0.0000215059246610556,
      0.0000283532159921211, 0.000368923882626859, 0.0000857467457561209,
      0.00075871953281751, 0.000194002299424151, 0.000188824454515841,
      0.000208303611101873, 0.0000215059246610556, 0.000194002299424151,
      0.000265780633005374, 0.000132611196599808, 0.000262742052359594,
      0.0000283532159921211, 0.000188824454515841, 0.000132611196599808,
      0.00025948420130626),
    .Dim = c(5L, 5L),
    .Dimnames = list(c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE"),
                     c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE")))



###################################################
### code chunk number 4: var
###################################################
var


###################################################
### code chunk number 5: minvar
###################################################
minvar(var, wmin = 0, wmax = 0.5)


###################################################
### code chunk number 6: portfolio.Rnw:100-103
###################################################
minvar(var,
       wmin = c(0.1, 0, 0, 0, 0), ## enforce at least 10% weight in CBK.DE
       wmax = 0.5)


###################################################
### code chunk number 7: portfolio.Rnw:106-109
###################################################
minvar(var, wmin = -Inf, wmax = Inf)   ## no bounds
minvar(var, wmin = -Inf, wmax = 0.45)  ## no lower bounds
minvar(var, wmin =  0.1, wmax = Inf)   ## no upper bounds


###################################################
### code chunk number 8: minvar-group-constraints
###################################################
## group 1 consists of asset 1 only,   and must have weight [0.25,0.30]
## group 2 consists of assets 4 and 5, and must have weight [0.10,0.20]
minvar(var, wmin = 0, wmax = 0.40,
       groups = list(1, 4:5),
       groups.wmin = c(0.25, 0.1),
       groups.wmax = c(0.30, 0.2))


###################################################
### code chunk number 9: portfolio.Rnw:123-129
###################################################
## group A consists of asset 1 only,   and must have weight [0.25,0.30]
## group B consists of assets 4 and 5, and must have weight [0.10,0.20]
minvar(var, wmin = 0, wmax = 0.40,
       groups = c("A", "none", "none", "B", "B"),
       groups.wmin = c(A = 0.25, B = 0.1),
       groups.wmax = c(A = 0.30, B = 0.2))


###################################################
### code chunk number 10: mv-data
###################################################
vols <- c(0.10, 0.15, 0.20, 0.22)  ## expected vols
m    <- c(0.06, 0.12, 0.09, 0.07)  ## expected mean returns
const_cor <- function(rho, na) {
    C <- array(rho, dim = c(na, na))
    diag(C) <- 1
    C
}
var <- diag(vols) %*% const_cor(0.5, length(vols)) %*% diag(vols)


###################################################
### code chunk number 11: mv-example-calls
###################################################
mvPortfolio(m, var, min.return = 0.08, wmax = 1)
mvPortfolio(m, var, min.return = 0.10, wmax = 1)
mvPortfolio(m, var, min.return = 0.12, wmax = 1)


###################################################
### code chunk number 12: frontier-plot
###################################################
if (requireNamespace("quadprog")) {
    wmin <- 0
    wmax <- 1
    p1 <- mvFrontier(m, var, wmin = wmin, wmax = wmax, n = 50)

    ## with a 'risk-free' asset rf
    rf <- 0.02
    p2 <- mvFrontier(m, var, wmin = wmin, wmax = wmax, n = 50, rf = rf)

    par(las = 1, bty = "n", tck = 0.001, ps = 8)
    plot(p1$volatility, p1$return, pch = 19, cex = 0.5, type = "o",
         xlab = "Expected volatility",
         ylab = "Expected return")
    lines(p2$volatility, p2$return, col = grey(0.5))
    abline(v = 0, h = rf)
} else
    plot(1)


###################################################
### code chunk number 13: portfolio.Rnw:211-220
###################################################
ns <- 120
R <- randomReturns(na = 1 + 10,  ## first asset is the benchmark
                   ns = ns,
                   sd = 0.03,
                   mean = 0.005,
                   rho = 0.7)

var <- cov(R)
trackingPortfolio(var, wmax = 0.4)


###################################################
### code chunk number 14: portfolio.Rnw:232-239
###################################################
ns <- 5000  ## number of scenarios
na <- 20    ## nunber of assets
R <- randomReturns(na, ns, sd = 0.01, rho = 0.5)
if (requireNamespace("Rglpk")) { ## example requires "Rglpk" package
    sol <- minCVaR(R, q = 0.1)
} else
    message("Package ", sQuote("Rglpk"), " not available")


