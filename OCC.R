n_levels <- c(2,3,4,5,10,20,50,100)
ty       <- 0.7

for (n in n_levels) {
  if (n==2) new_plot=F else new_plot=T
  curve(1-power.t.test(n,x,type="two.sample")$power,0,6,add=new_plot,
        ylim=c(0,1),
        xlab="d",
        ylab="P(Type II error), or (1-Power)",
        main="Operating Characteristic Curves\nTwo samples, @5%"
  )
  tx<- (power.t.test(n,pow=1-ty,type="two.sample")$d)
  symbols(x=tx, y=ty, sq=c(0.3), add=T, inches=F, bg="white", fg=NULL)
  text(x=tx, y=ty, lab=n)
  ty<-ty-0.08
}
grid(col="darkgray")


ty<-0.8
for (n in n_levels) {
  if (n==2) new_plot=F else new_plot=T
  curve(1-power.t.test(n,x,type="one.sample")$power,0,6,add=new_plot,
        ylim=c(0,1),
        xlab="d",
        ylab="P(Type II error), or (1-Power)",
        main="Operating Characteristic Curves\nOne sample, @5%"
  )
  
  tx<- (power.t.test(n,d=NULL,pow=1-ty,type="one.sample")$d)
  symbols(x=tx, y=ty, sq=c(0.3), add=T, inches=F, bg="white", fg=NULL)
  text(x=tx, y=ty, lab=n)
  ty<-ty-0.08
}
grid(col="darkgray")
