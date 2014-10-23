### R code from vignette source 'An_overview.Rnw'

###################################################
### code chunk number 1: An_overview.Rnw:22-24
###################################################
require("NMOF")
options(continue = " ", digits = 5, max.print = 10, width = 85)


###################################################
### code chunk number 2: An_overview.Rnw:108-109
###################################################
showExample()


###################################################
### code chunk number 3: An_overview.Rnw:117-118 (eval = FALSE)
###################################################
## vignette(package = "NMOF")  ## display vignette titles


###################################################
### code chunk number 4: An_overview.Rnw:120-122
###################################################
x <- vignette(package = "NMOF")
cat(paste(strwrap(x$results[,"Title"], exdent = 2), collapse = "\n"))


###################################################
### code chunk number 5: An_overview.Rnw:143-144 (eval = FALSE)
###################################################
## file.show(system.file("NMOFex/NMOFman.R", package = "NMOF"))


###################################################
### code chunk number 6: An_overview.Rnw:151-158
###################################################
test.rep <- readLines(system.file("unitTests/report.txt", 
                                  package = "NMOF"))
nt <- gsub(".*\\(([0-9]+) checks?\\).*", "\\1",
           test.rep[grep("\\(\\d+ checks?\\)", test.rep)])
cat("Package version  ", gsub("(.*)[.]([0-9]+)$", "\\1-\\2",
                            packageVersion("NMOF")), "\n",
    "Number of tests: ", sum(as.numeric(nt)), sep = "")


###################################################
### code chunk number 7: An_overview.Rnw:183-186 (eval = FALSE)
###################################################
## require("utils")
## bug.report("[NMOF] Unexpected behaviour in function XXX", 
##             maintainer("NMOF"), package = "NMOF")


