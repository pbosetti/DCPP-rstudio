rm(list=ls())
# Load data from the Internet using URL rather than filename
df <- read.table("https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/rats.dat", header=T)

boxplot(Life~Poison, data=df, xlab="Poison", ylab="Life")
boxplot(Life~Treat, data=df, xlab="Treatment", ylab="Life")

interaction.plot(df$Poison, df$Treat, df$Life)
interaction.plot(df$Treat, df$Poison, df$Life)

df.lm <- lm(Life~Treat*Poison, data=df)
anova(df.lm)

# Model Adequacy Check:
qqnorm(df.lm$residuals)
qqline(df.lm$residuals)
plot(df.lm$fitted.values, df.lm$residuals)

df.lm1 <- lm(log(Life)~Treat*Poison, data=df)
qqnorm(df.lm1$residuals)
qqline(df.lm1$residuals)
plot(df.lm1$fitted.values, df.lm1$residuals)

df.lm2 <- lm(1/Life~Treat*Poison, data=df)
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals)
plot(df.lm2$fitted.values, df.lm2$residuals)

anova(df.lm2)
# remove interaction
df.lm3 <- lm(1/Life ~ Treat + Poison, data=df)
anova(df.lm3)
qqnorm(df.lm3$residuals)
qqline(df.lm3$residuals)
plot(df.lm3$fitted.values, df.lm3$residuals)
shapiro.test(df.lm3$residuals)
