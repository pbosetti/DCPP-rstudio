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
