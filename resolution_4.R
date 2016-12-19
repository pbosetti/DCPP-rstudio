# Manufacturing for ICs
# A = Aperture setting
# B = Exposure time
# C = Develop time
# D = Mask dimension
# E = Etch time
# Response: Yield (ICs/h)
# 
# 2^(5-1) FFP -> I=ABCDE => E=ABCD

lvl <- c(-1, 1)
df <- expand.grid(A=lvl, B=lvl, C=lvl, D=lvl)
attach(df)
df$E <- A*B*C*D
detach(df)
df$A <- as.factor(df$A)
df$B <- as.factor(df$B)
df$C <- as.factor(df$C)
df$D <- as.factor(df$D)
df$E <- as.factor(df$E)

# ... usual randomization, test execution, data collection...

df$Yield <- c(
  8, 9, 34, 52, 16, 22, 45, 60,
  6, 10, 30, 50, 15, 21, 44, 63
)
sum(df$Yield) # => 485

df.lm <- lm(Yield~A*B*C*D*E, data=df)

n <- length(df.lm$effects)
effects <- as.vector(df.lm$effects[2:n])
qn <- qqnorm(effects, datax=T, ylim=c(-70, 30))
text(qn$x, qn$y, lab=names(df.lm$effects)[2:n], pos=4)
qqline(effects, datax=T)

df.lm <- lm(Yield~A*B+C, data=df)
anova(df.lm)
