rm(list=ls())
k <- 0.7
n <- 50
x <- sample(40:70, n, rep=T)
y <- k*x + rnorm(n, sd=5)
plot(x, y, xlim=c(20,90))
abline(a=0, b=k, col="green")

model <- lm(y~x)
abline(model, col="red")

newx <- seq(20,90)
prd <- predict(model,
               newdata=data.frame(x=newx),
               interval=c("confidence"), 
               level=0.95,
               type="response")
prd <- data.frame(prd)
lines(newx, prd$lwr, col="orange", lty=2)
lines(newx, prd$upr, col="orange", lty=2)
