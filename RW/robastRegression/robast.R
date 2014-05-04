setwd("D:/RW/stan/rats/")

library(MASS, quietly=TRUE)

# MM inference
brain_standerd <- (Animals$brain - mean(Animals$brain))/sd(Animals$brain)
body_standerd <- (Animals$body- mean(Animals$body))/sd(Animals$body)

model_mm <- rlm(brain_standerd ~ body_standerd, Animals, method="MM")
summary(model_mm)

# plot
#with(Animals, plot(body, brain, main=name))
plot(body_standerd, brain_standerd, main=name)
abline(model_mm)