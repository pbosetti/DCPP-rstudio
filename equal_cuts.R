# testing power supply, is normal distribution adequate?
# average voltage
m <- 5.04
# standard deviation
s <- 0.08
# sample size
N <- 100
# number of cuts
nc <- 8
# Equally probable cuts
(cuts<-qnorm(seq(0, 1, len=nc+1), m=m, s=s))
curve(pnorm(x, m=m, s=s), m-3*s, m+3*s)
abline(v=cuts, col="green")

# Expected and observed counts
ei <- rep(N * (1/nc), 8)
oi <- c(12, 14, 12, 13, 12, 11, 12, 14)

# Test statistic and p-value
chisq <- sum((oi - ei)**2 / ei)
pchisq(chisq, df=nc-2-1, low=F)
