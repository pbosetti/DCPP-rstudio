# Cutting tool life Design

rm(list=ls())

prepare <- function(factors=list(A=1:3, B=1:3), k=4, runorder=TRUE, file="") {
  factors$Rep = 1:k
  df.g = expand.grid(factors)
  n    = nrow(df.g)
  key  = data.frame(r=runif(n, 0, 1), s=1:n)
  key  = key[order(key$r),]$s
  df.h = data.frame(
    RunOrder  = key,
    StdOrder  = 1:n
  )
  df.t = data.frame(Yield=rep(NA, n))
  df = cbind(df.h, df.g, df.t)
  if (runorder) {
    df = df[order(df$RunOrder),]
  }
  if (file != "") {
    write.table(df, file, col.names = T, row.names = F, quote = F, sep = "\t")
  }
  return(df)
}

df <- prepare(
  factors=list(Angle=c(15, 20, 25), Speed=c(125, 150, 175)), 
  k=2, runorder = F
)
df$Yield = c(
  -2, 0, -1, -3,
  1, 5, 2, 4,
  0, -1, 2, 0,
  0, 3, 6, 3,
  6, -1
)
sum(df$Yield)
df$Angle <- as.factor(df$Angle)
df$Speed <- as.factor(df$Speed)
df.lm <- lm(Yield~Angle*Speed*I(Angle^2)*I(Speed^2), data = df)
qqnorm(df.lm$residuals)
qqline(df.lm$residuals)
plot(df$Run, df.lm$residuals, ylab="Residuals", xlab="Run Order")
plot(df.lm$fitted.values, df.lm$residuals, , ylab="Residuals", xlab="Fitted Values")
plot(df$Angle, df.lm$residuals)
plot(df$Speed, df.lm$residuals)

anova(df.lm)
df.lm <- lm(Yield~Angle*Speed + I(Angle^2) + Angle:I(Speed^2) + I(Angle^2):I(Speed^2), data=df)
anova(df.lm1)

# Response surface
library(lattice)
Ar <- seq(20,25,0.1)
Sr <- seq(125,175,1)
g <- expand.grid(Angle = Ar, Speed = Sr)
g$Life <- predict(df.lm,data.frame(Angle=g$Angle,Speed=g$Speed))
wireframe(Life~Angle*Speed,data=g,
          shade = T,
          light.source = c(10,0,10),
          drape = F,
          scales=list(arrows= FALSE)
)
contourplot(Life~Angle*Speed, data=g, cuts=10, region=T)



