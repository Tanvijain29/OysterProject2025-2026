options("install.lock"=FALSE)
install.packages("demogR")
library(popbio)
ages = c("Age 1", "Age 2", "Age 3", "Age 4")
A= matrix(c(0,0.9,1.4,1.1,0.7,0,0,0,0,0.6,0,0,0,0,0.3,0),nrow=4,byrow=T,dimnames=list(ages,ages))
N0=c(20,40,21,9)
projA=pop.projection(A,N0,10)
projA
eigen.analysis(A)
