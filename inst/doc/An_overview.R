### R code from vignette source 'An_overview.Rnw'

###################################################
### code chunk number 1: An_overview.Rnw:24-28
###################################################
library("NMOF")
options(continue = "  ", digits = 5, max.print = 20, width = 85)
pv <- packageVersion("NMOF")
pv <- gsub("(.*)[.](.*)", "\\1-\\2", pv)


###################################################
### code chunk number 2: An_overview.Rnw:123-124
###################################################
showExample()


###################################################
### code chunk number 3: An_overview.Rnw:132-133 (eval = FALSE)
###################################################
## vignette(package = "NMOF")  ## display vignette titles


###################################################
### code chunk number 4: An_overview.Rnw:135-137
###################################################
x <- vignette(package = "NMOF")
cat(paste(strwrap(x$results[,"Title"], exdent = 2), collapse = "\n"))


###################################################
### code chunk number 5: An_overview.Rnw:157-160 (eval = FALSE)
###################################################
## install.packages("NMOF",
##                  repos = c("http://enricoschumann.net/R",
##                            getOption("repos")))


###################################################
### code chunk number 6: An_overview.Rnw:183-190 (eval = FALSE)
###################################################
## test.rep <- readLines(system.file("unitTests/test_results.txt",
##                                   package = "NMOF"))
## nt <- gsub(".*\\(([0-9]+) checks?\\).*", "\\1",
##            test.rep[grep("\\(\\d+ checks?\\)", test.rep)])
## cat("Package version  ", gsub("(.*)[.]([0-9]+)$", "\\1-\\2",
##                             packageVersion("NMOF")), "\n",
##     "Number of tests: ", sum(as.numeric(nt)), sep = "")


###################################################
### code chunk number 7: An_overview.Rnw:218-221 (eval = FALSE)
###################################################
## library("utils")
## bug.report("[NMOF] Unexpected behaviour in function XXX",
##             maintainer("NMOF"), package = "NMOF")


