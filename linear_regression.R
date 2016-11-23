# Linear regressio example
rm(list=ls())
# Theoretical model: y=k*x+m
b <- 0.7
a <- 1.3
sd <- 2
n <- 50
x <- sample(40:70, n, rep=T)
y <- b*x + a + rnorm(n, sd=3)
plot(x, y, xlim=c(0,90), ylim=c(0,60))
abline(a=a, b=b, col="green")

# Linear regression
(model <- lm(y~x))
abline(model, col="red")

newx <- seq(0,90)
prd <- predict(model,
               newdata=data.frame(x=newx),
               interval=c("confidence"), 
               level=0.95,
               type="response")
prd <- data.frame(prd)
lines(newx, prd$lwr, col="orange", lty=2)
lines(newx, prd$upr, col="orange", lty=2)

