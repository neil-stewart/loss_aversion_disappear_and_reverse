rm(list=ls())
options(scipen=10)
library(lattice)
library(latticeExtra)

data <- read.csv("experiment_3_raw.csv",as.is=T)
data$subno <- as.factor(data$subno)

#### Fit GLMs

loss.aversion <- function(data) {
	m1 <- glm(resp ~ gain + loss, data=data, family=binomial)
	data.frame(id=data$subno[1],condition=data$condition[1], converged=m1$converged, deviance=deviance(m1), max.residual=max(abs(residuals(m1))), intercept=coef(m1)["(Intercept)"], beta.loss=coef(m1)["loss"], beta.gain=coef(m1)["gain"])
}
fits <- by(data=data, data$subno, loss.aversion)

######
######

fits <- do.call(rbind, fits)
fits <- fits[order(fits$id),]
fits$plot.slope <- with(fits, -beta.loss/beta.gain)
fits$plot.intercept <- with(fits, -intercept/beta.gain)

# Deviance
histogram(~deviance, data=fits)
deviance.cut <- quantile(fits$deviance,probs=c(.95))
fits <- subset(fits,deviance < deviance.cut)

# Slope
fits.negative.slope <- subset(fits, plot.slope < 0)
fits <- subset(fits,plot.slope > 0)

####

fits$la <- abs(fits$beta.loss)/abs(fits$beta.gain)
with(fits,by(fits$la,fits$condition,median))

# Inferrential statistics

library(reshape2)

x <- colsplit(fits[,2],pattern="\\.",names=c("gains","losses"))
fits$gains <- x$gains
fits$losses <- x$losses
fits$losses <- paste(fits$losses,"0",sep = "")

m1 <- lm(log(la)~as.factor(gains) + as.factor(losses), data=fits)
summary(m1)

# Heteroskedasticity

library(lmtest)
library(sandwich)

bptest(m1)
vcovHC(m1)
coeftest(m1,vcov.= vcovHC)
