# Goodness of fit test
# MANUAL version

# generate the data
count <- 200
data <- rnorm(count, 15, 2)
data.m <- mean(data)
data.s <- sd(data)

# plot histogram and collect bins
(data.h <- hist(data, freq=F))

# data$counts are Oi (Observed values), we need to calculate the Ei's for the same
# bins, as defined in data$breaks
data.b <- data.h$breaks
data.b[1] <- -Inf
data.b[length(data.b)] <- Inf
ei <- diff(pnorm(data.b, data.m, data.s)) * count
plot(ei)
points(data.h$counts, col="red")

# or, manually:
chi <- sum(((data.h$counts - ei)**2)/ei)
# Find p-value, 10 bins, two parameters:
pchisq(chi, df=7)

# In one step:
ks.test(data, "pnorm", mean=data.m, sd=data.s)

# for normality only:
shapiro.test(data)
