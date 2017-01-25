# Quality Control Curves
# 
library(qcc)

rm(list=ls())
data("orangejuice")

attach(orangejuice)
q <- qcc(D[trial], type="np", sizes = size[trial])

tuning <- setdiff(which(trial), q$violations$beyond.limits)
q <- qcc(D[tuning], type="np", sizes = size[tuning], newdata=D[!trial], newsizes=size[!trial])

q <- qcc(D[sample>33], type="np", sizes = size[sample>33])

detach(orangejuice)

# Variable group size
data <- read.table("https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/p_data.dat", h=T)
qcc(data$D, type="p", sizes=data$size)

# Count of defects - c type charts

data("circuit")
attach(circuit)
q1 <- qcc(x[trial], sizes = size[trial], type = "c")
tuning <- setdiff(which(trial), q1$violations$beyond.limits)

q1 <- qcc(x[tuning], sizes = size[tuning], type = "c", 
          newdata=x[!trial], newsizes = size[!trial])
detach(circuit)


# Variable data: Xbar-R charts

data("pistonrings")
attach(pistonrings)
d1 <- qcc.groups(data=diameter[trial], sample=sample[trial])
q1 <- qcc(data=d1, type="R")
q2 <- qcc(data=d1, type="xbar", center=74)  

d2 <- qcc.groups(data=diameter[!trial], sample=sample[!trial])
q3 <- qcc(data=d1, type="R", newdata = d2)
q4 <- qcc(data=d1, type="xbar", newdata = d2)

process.capability(q2, spec.limits = c(74-0.05, 74+0.05))

q5 <- qcc(data=d1, type="S", newdata = d2)

ewma(data=d1, newdata = d2, center=74, lambda=0.1)
detach(pistonrings)
