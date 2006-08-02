###################################################
### chunk number 1: setup
###################################################
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
set.seed(290875)
options(prompt = "R> ", width = 63, # digits = 4,
    SweaveHooks = list(leftpar = function()
        par(mai = par("mai") * c(1, 1.05, 1, 1))))
HSAURpkg <- require("HSAUR")
if (!HSAURpkg) stop("cannot load package ", sQuote("HSAUR"))
rm(HSAURpkg)
### </FIXME> hm, R-2.4.0 --vanilla seems to need this
a <- Sys.setlocale("LC_ALL", "C")
### </FIXME>


###################################################
### chunk number 2: pre-vignette eval=FALSE
###################################################
## vignette("Ch_introduction_to_R", package = "HSAUR")


###################################################
### chunk number 3: pre-vignette eval=FALSE
###################################################
## edit(vignette("Ch_introduction_to_R", package = "HSAUR"))


