# Usage of chi square test for test of independence
# The MASS library has a dataframe named 'survey' that reports results
# for a survey among students of the University of Massachusetts

rm(list=ls())
library(MASS)
head(survey)

# We want to check if smoking habit is independent of excercise level
# Group data in a contingency table:
(tbl <- table(survey$Smoke, survey$Exer))

# Use the embedded formula. THE NULL HYPOTHESIS IS THAT THE TWO VARIABLES
# ARE INDEPENDENT
chisq.test(tbl)

# Warning is due to small numbers in table. We collect 'None' and 'Some'
(ctbl <- cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]))
chisq.test(ctbl)
