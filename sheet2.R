#Q2
library("ConvergenceConcepts")

genYn <- function(n){
  yn <- (0.5)^(1:n)*rnorm(n,2,9)
  return(yn)
}
check.convergence(2000,500,genYn,mode="p")

genTn <- function(n){
  tn <- cumsum((0.5)^(1:n)*rnorm(n,2,9))-2
  return (tn)
}

check.convergence(3000,600,genTn,mode="p")

genAn <- function(n){
  an <- cumsum((0.5)^(1:n)*rnorm(n,2,9))/cumsum(rep(1,n))
  return (an)
}


check.convergence(3000,600,genAn,mode="as")

##########################################################
#Q3
ques <- function(n){
  rn <- runif(n,min=0,max=1)
  m<- max(rn)
  return (m-1)
}
check.convergence(2000,500,ques,mode="p")
check.convergence(2000,500,ques,mode="as")

##########################################################
#Q4
ques <- function(n){
  rn <- runif(n,min=0,max=1)
  return (rn/n)
}
check.convergence(2000,500,ques,mode="p")

#######################################################
#Q5
ques <- function(n){
  rn <- rnorm(n, mean = 0, sd = 1)
  return (rn)
}
check.convergence(2000,500,ques,mode = "r")  # stationary

############################################################
#Q6
b0 <- 0
b1 <- 0.1
sig <- 1

ques <- function(n){
  ans <- c(1:n)
  rn <- rnorm(n, mean = 0, sd = 1)
  for(i in 1:n){
    ans[i] = b0+b1*i+rn[i]
  }
  return (ans)
}
check.convergence(2000,500,ques,mode = "r") #not stationary

# zt


ques <- function(n){
  ans <- c(1:n)
  rn <- rnorm(n, mean = 0, sd = 1)
  for(i in 1:n){
    ans[i] = b0+rn[i]
  }
  return (ans)
}
check.convergence(2000,500,ques,mode = "r")  #stationary

#######################################################
