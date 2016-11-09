# T-test example
# Two samples, two sides test

# Cean up the environment (remove all previously defined variables)
rm(list=ls())

# n1 and n2 are sample sizes (number of elements)
n1 <- 10
n2 <- 12
# n is the total number of DoF
n <- n1 + n2 -2

# Generate two samples s1 and s2
s1 <- rnorm(n1, 5, 0.1)
s2 <- rnorm(n2, 5.3, 0.1)

# Calculate the pooled variance
# Note: sd() gives the standard deviation, var() gives its 
# square, or variance
sp <- ((n1-1)*var(s1) + (n2-1)*var(s2)) / n

# Calculate the test statistic
# Note: surrounding an assignment with parentheses also 
# prints out the result
(t0 <- (mean(s1)-mean(s2))/(sqrt(sp*(1/n1+1/n2))))

# Evaluate the p-value: it is the cumulative probability
# upper-tail of T distribution for abs(t0) with n DoF, times 2 because
# it is a two-sides test:
pt(abs(t0), n, low=F) * 2

# Compare with the built-in t.test() function:
(test <- t.test(s1,s2, alt="two.sided", var.equal=T, conf=0.99))
