rm(list=ls())
# Load data from the Internet using URL rather than filename
df <- read.table("https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/diet.dat", header=T)

# Build the LINEAR MODEL
# Tilda is Windows alt-gr 0126 (on the number keyboard)
#          Mac     alt-5
df.lm <- lm(cTime~diet, data=df)
# Contents of an object:
attributes(df.lm)

# Normality check
qqnorm(df.lm$residuals)
qqline(df.lm$residuals)
shapiro.test(df.lm$residuals)

# Trends/Patterns
plot(df.lm$residuals, xlab="Index", ylab="Residuals")
plot(df.lm$fitted, df.lm$residuals, xlab="Fitted Values", ylab="Residuals")

# ANOVA
anova(df.lm)

# Chart
boxplot(cTime~diet, data=df)

# Alternative interface:
df.aov <- aov(cTime~diet, data=df)
summary(df.aov)

# Tukey's test
plot(TukeyHSD(df.aov))

