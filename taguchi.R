R <- c(0.5,1.5)
Rn <- c(-0.03, 0.03)
L <- c(0.5,5.5)
Ln <- c(-0.15, 0.15)

factors <- list(R=c(0.5,1.5), L=c(0.5,5.5))
noise   <- list(Rn=c(-0.03, 0.03), Ln=c(-0.15, 0.15))
                
taguchi_table <- function(factors=list(), noises=list()) {
  grd <- expand.grid(c(noises,factors))
  grd$case <- gl(2^length(factors),2^length(noises))
  return(grd)
}

sn_min <- function(x) -10*log10(1/length(x) * (sum(x^2)))
sn_max <- function(x) -10*log10(1/length(x) * (sum(1/(x^2))))
sn_t <-function(x,target=4) -10*log10(1/length(x) * (sum((x-mean(x))^2 + (mean(x)-target)^2)))

(tbl <- taguchi_table(factors, noise))
tbl$V <- mapply(function(r, l){2*pi*r^2*l}, tbl$R+tbl$Rn, tbl$L+tbl$Ln)
by(tbl$V, tbl$case, sn_t, 4)
by(tbl$V, tbl$case, sn_max)
by(tbl$V, tbl$case, sn_min)
