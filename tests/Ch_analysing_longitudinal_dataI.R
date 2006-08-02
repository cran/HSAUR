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
### chunk number 2: ALDI-setup
###################################################
library("Matrix")
library("lme4")


###################################################
### chunk number 3: ALDI-plot-BtheB
###################################################
data("BtheB", package = "HSAUR")
layout(matrix(1:2, nrow = 1))
ylim <- range(BtheB[,grep("bdi", names(BtheB))], na.rm = TRUE)
boxplot(subset(BtheB, treatment == "TAU")[,grep("bdi", names(BtheB))],
        main = "Treated as usual", ylab = "BDI",
        xlab = "Time (in months)", names = c(0, 2, 4, 6, 8), ylim = ylim)
boxplot(subset(BtheB, treatment == "BtheB")[,grep("bdi", names(BtheB))],
        main = "Beat the Blues", ylab = "BDI", xlab = "Time (in months)",
        names = c(0, 2, 4, 6, 8), ylim = ylim)


###################################################
### chunk number 4: ALDI-long-BtheB
###################################################
data("BtheB", package = "HSAUR")
BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject",
                      varying = c("bdi.2m", "bdi.4m", "bdi.6m", "bdi.8m"),
                      direction = "long")
BtheB_long$time <- rep(c(2, 4, 6, 8), rep(nobs, 4))


###################################################
### chunk number 5: ALDI-showlong-BtheB
###################################################
subset(BtheB_long, subject %in% c("1", "2", "3"))


###################################################
### chunk number 6: ALDI-fit-BtheB
###################################################
library("lme4")
BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug + length +
                  (1 | subject), data = BtheB_long,
                   method = "ML", na.action = na.omit)
BtheB_lmer2 <- lmer(bdi ~ bdi.pre + time + treatment + drug + length +
                  (time | subject), data = BtheB_long,
                   method = "ML", na.action = na.omit)
anova(BtheB_lmer1, BtheB_lmer2)


###################################################
### chunk number 7: ALDI-summary-BtheB
###################################################
summary(BtheB_lmer1)


