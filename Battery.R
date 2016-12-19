# Battery Life Design

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

# Generate the datafile with 
# prepare(list(A=c(-1,0,+1), B=c(-1,0,+1), k=, runorder = T, file="data_file.txt")
# then do the experiments in the given order and fill the datafile with Yields
# Then load it back in R
df <- read.table("data_file.txt", h=T)
df <- df[order(df$StdOrder),]
df <- prepare(list(Temp=c(15, 70, 125), Mat=c("1", "2", "3")), k=4, runorder = F)

df.lm <- lm(Yield~Temp*Mat, data=df)

# Check residuals
qqnorm(df.lm$residuals)
qqline(df.lm$residuals)
plot(df$Run, df.lm$residuals, ylab="Residuals", xlab="Run Order")
plot(df.lm$fitted.values, df.lm$residuals, , ylab="Residuals", xlab="Fitted Values")
plot(df$Temp, df.lm$residuals)
plot(as.numeric(df$Mat), df.lm$residuals)

anova(df.lm)

df.lm1 <- lm(Yield~Mat + Temp, data=df)
# Check residuals
qqnorm(df.lm1$residuals)
qqline(df.lm1$residuals)
plot(df$Run, df.lm1$residuals, ylab="Residuals", xlab="Run Order")
plot(df.lm1$fitted.values, df.lm1$residuals, , ylab="Residuals", xlab="Fitted Values")
plot(df$Temp, df.lm1$residuals)
plot(as.numeric(df$Mat), df.lm1$residuals)


