rm(list=ls())

N<-30
xc1<-rnorm(N, mean = 2, sd = 1)
xc2<-rnorm(N, mean = 4, sd = 1)

yaxis<-matrix(0, nrow = N)

plot(xc1, yaxis, col='red', xlim=c(0, 10), ylim = c(0, 1))
par(new=T)
plot(xc2, yaxis, col='blue', xlim=c(0, 10), ylim = c(0, 1))

xgrid<-seq(0, 6, 0.1)
ygrid<-1*(xgrid>=3)

par(new = T)
plot(xgrid, ygrid, col='black', type='l', xlim = c(0, 6), ylim = c(0, 1))
