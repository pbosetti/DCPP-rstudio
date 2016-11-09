# Discrete distributions

n <- 100
x <- 1:20
hist(rbinom(n, 10, 0.5), right=T, freq=F)
lines(dbinom(x, 10, 0.5), typ="h", col="red", lw=3)


plot(dpois(x, 10), typ="h")


plot(dgeom(x, 0.1), typ="h")
plot(pgeom(x, 0.1))


curve(pgeom(x,0.1), xlim=c(0,20), typ="h", n=21)


v <- runif(100, 10, 25)
chisq.test(v, p=dnorm(1:100, 10, 25), resc=T)

ks.test(v, punif)
