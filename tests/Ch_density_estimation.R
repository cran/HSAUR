###################################################
### chunk number 1: setup
###################################################
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
set.seed(290875)
options(prompt = "R> ", continue = "+  ",
    width = 63, # digits = 4,
    SweaveHooks = list(leftpar = function()
        par(mai = par("mai") * c(1, 1.05, 1, 1))))
HSAURpkg <- require("HSAUR")
if (!HSAURpkg) stop("cannot load package ", sQuote("HSAUR"))
rm(HSAURpkg)
### </FIXME> hm, R-2.4.0 --vanilla seems to need this
a <- Sys.setlocale("LC_ALL", "C")
### </FIXME>
book <- TRUE
refs <- cbind(c("AItR", "SI", "CI", "ANOVA", "MLR", "GLM",
                "DE", "RP", "SA", "ALDI", "ALDII", "MA", "PCA",
                "MDS", "CA"), 1:15)
ch <- function(x, book = TRUE) {
    ch <- refs[which(refs[,1] == x),]
    if (book) {
        return(paste("Chapter~\\\\ref{", ch[1], "}", sep = ""))
    } else {
        return(paste("Chapter~\\\\ref{", ch[2], "}", sep = ""))
    }
}


###################################################
### chunk number 2: DE-setup
###################################################
x <- library("KernSmooth")
x <- library("flexmix")
x <- library("boot")


###################################################
### chunk number 3: DE-faithful-density
###################################################
data("faithful", package = "datasets")
x <- faithful$waiting
layout(matrix(1:3, ncol = 3))
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
     probability = TRUE, main = "Gaussian kernel",
     border = "gray")
lines(density(x, width = 12), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
     probability = TRUE, main = "Rectangular kernel",
     border = "gray")
lines(density(x, width = 12, window = "rectangular"), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
     probability = TRUE, main = "Triangular kernel",
     border = "gray")
lines(density(x, width = 12, window = "triangular"), lwd = 2)
rug(x)


###################################################
### chunk number 4: DE-CYGOB1-contour
###################################################
library("KernSmooth")
data("CYGOB1", package = "HSAUR")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
        xlab = "log surface temperature",
        ylab = "log light intensity")


###################################################
### chunk number 5: DE-CYGOB1-persp
###################################################
persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
      xlab = "log surface temperature",
      ylab = "log light intensity",
      zlab = "estimated density",
      theta = -35, axes = TRUE, box = TRUE)


###################################################
### chunk number 6: DE-faithful-optim
###################################################
logL <- function(param, x) {
    d1 <- dnorm(x, mean = param[2], sd = param[3])
    d2 <- dnorm(x, mean = param[4], sd = param[5])
    -sum(log(param[1] * d1 + (1 - param[1]) * d2))
}
startparam <- c(p = 0.5, mu1 = 50, sd1 = 3, mu2 = 80, sd2 = 3)
opp <- optim(startparam, logL, x = faithful$waiting,
             method = "L-BFGS-B",
             lower = c(0.01, rep(1, 4)),
             upper = c(0.99, rep(200, 4)))
opp


###################################################
### chunk number 7: DE-faithful-optim-print
###################################################
print(opp[names(opp) != "message"])


###################################################
### chunk number 8: DE-attach-mclust
###################################################
library("mclust")


###################################################
### chunk number 9: DE-faithful-mclust
###################################################
library("mclust")
mc <- Mclust(faithful$waiting)
mc


###################################################
### chunk number 10: DE-faithful-mclust-mu
###################################################
mc$parameters$mean


###################################################
### chunk number 11: DE-faithful-mclust-para
###################################################
sqrt(mc$parameters$variance$sigmasq)


###################################################
### chunk number 12: DE-faithful-flexmix
###################################################
library("flexmix")
fl <- flexmix(waiting ~ 1, data = faithful, k = 2)


###################################################
### chunk number 13: DE-faithful-flexmix-parameters
###################################################
parameters(fl, component = 1)
parameters(fl, component = 2)


###################################################
### chunk number 14: DE-faithful-2Dplot
###################################################
opar <- as.list(opp$par)
rx <- seq(from = 40, to = 110, by = 0.1)
d1 <- dnorm(rx, mean = opar$mu1, sd = opar$sd1)
d2 <- dnorm(rx, mean = opar$mu2, sd = opar$sd2)
f <- opar$p * d1 + (1 - opar$p) * d2
hist(x, probability = TRUE, xlab = "Waiting times (in min.)",
     border = "gray", xlim = range(rx), ylim = c(0, 0.06),
     main = "")
lines(rx, f, lwd = 2)
lines(rx, dnorm(rx, mean = mean(x), sd = sd(x)), lty = 2,
      lwd = 2)
legend(50, 0.06, lty = 1:2, bty = "n",
       legend = c("Fitted two-component mixture density",
                  "Fitted single normal density"))


###################################################
### chunk number 15: DE-faithful-boot
###################################################
library("boot")
fit <- function(x, indx) {
    a <- Mclust(x[indx], minG = 2, maxG = 2)$parameters
    if (a$pro[1] < 0.5)
        return(c(p = a$pro[1], mu1 = a$mean[1],
                               mu2 = a$mean[2]))
    return(c(p = 1 - a$pro[1], mu1 = a$mean[2],
                               mu2 = a$mean[1]))
}


###################################################
### chunk number 16: DE-faithful-bootrun
###################################################
bootparafile <- file.path(.find.package("HSAUR"), "cache", "DE-bootpara.rda")
if (file.exists(bootparafile)) {
    load(bootparafile)
} else {
    bootpara <- boot(faithful$waiting, fit, R = 1000)
}


###################################################
### chunk number 17: DE-faithful-p-ci
###################################################
boot.ci(bootpara, type = "bca", index = 1)


###################################################
### chunk number 18: DE-faithful-mu1-ci
###################################################
boot.ci(bootpara, type = "bca", index = 2)


###################################################
### chunk number 19: DE-faithful-mu2-ci
###################################################
boot.ci(bootpara, type = "bca", index = 3)


###################################################
### chunk number 20: DE-bootplot
###################################################
bootplot <- function(b, index, main = "") {
    dens <- density(b$t[,index])
    ci <- boot.ci(b, type = "bca", index = index)$bca[4:5]
    est <- b$t0[index]
    plot(dens, main = main)
    y <- max(dens$y) / 10
    segments(ci[1], y, ci[2], y, lty = 2)
    points(ci[1], y, pch = "(")
    points(ci[2], y, pch = ")")
    points(est, y, pch = 19)
}


###################################################
### chunk number 21: DE-faithful-boot-plot
###################################################
layout(matrix(1:2, ncol = 2))
bootplot(bootpara, 2, main = expression(mu[1]))
bootplot(bootpara, 3, main = expression(mu[2]))


