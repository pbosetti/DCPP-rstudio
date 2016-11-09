# How to build Q-Q Plots
#------------------------
# generate 8 N(15,4) values
m   <- 15
sd  <- 2
n   <- 8
y   <- round(rnorm(n,m,sd),2)

# sort y vector
y.s <- sort(y)

# generate "plotting positions"
y.p <- ppoints(n)  #(1:n - 3/8) / (n + 1 - 2*3/8)
# this is an empirical formula for avoiding inf

# compute the corresponding theoretical quantiles, i.e the
# values on a N(0,1) corresponding to cumulative probabilities
# given in plotting positions:
t.q <- qnorm(y.p)

# plot results. By reference, the built-in qqnorm function
# is also used
qqnorm(y, xlab="Theoretical Quantiles (q(f))", ylab="Sample Quantiles (y)", main="Normal Q-Q Plot")
points(y.s~t.q, col="red", pch=3)
# qqline() plots a line passing through the 1st and 3rd quartile
# of the nominal N(mu,sigma^2) distribution
qqline(y)
grid()
