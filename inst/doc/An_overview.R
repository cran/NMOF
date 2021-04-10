### R code from vignette source 'An_overview.Rnw'

###################################################
### code chunk number 1: An_overview.Rnw:23-25
###################################################
library("NMOF")
options(continue = "  ", digits = 5, max.print = 20, width = 85)


###################################################
### code chunk number 2: An_overview.Rnw:119-120
###################################################
showExample()


###################################################
### code chunk number 3: An_overview.Rnw:128-129 (eval = FALSE)
###################################################
## vignette(package = "NMOF")  ## display vignette titles


###################################################
### code chunk number 4: An_overview.Rnw:131-133
###################################################
x <- vignette(package = "NMOF")
cat(paste(strwrap(x$results[,"Title"], exdent = 2), collapse = "\n"))


###################################################
### code chunk number 5: An_overview.Rnw:153-156 (eval = FALSE)
###################################################
## install.packages("NMOF",
##                  repos = c("http://enricoschumann.net/R",
##                            getOption("repos")))


###################################################
### code chunk number 6: An_overview.Rnw:179-186 (eval = FALSE)
###################################################
## test.rep <- readLines(system.file("unitTests/test_results.txt",
##                                   package = "NMOF"))
## nt <- gsub(".*\\(([0-9]+) checks?\\).*", "\\1",
##            test.rep[grep("\\(\\d+ checks?\\)", test.rep)])
## cat("Package version  ", gsub("(.*)[.]([0-9]+)$", "\\1-\\2",
##                             packageVersion("NMOF")), "\n",
##     "Number of tests: ", sum(as.numeric(nt)), sep = "")


###################################################
### code chunk number 7: An_overview.Rnw:214-217 (eval = FALSE)
###################################################
## library("utils")
## bug.report("[NMOF] Unexpected behaviour in function XXX",
##             maintainer("NMOF"), package = "NMOF")


