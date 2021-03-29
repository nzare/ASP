# Let X1, X2, · · · be i.i.d. continuous random variables with N(2, 9) distribution. 
# Defining Yn = (0.5)^n Xn, n = 1,2,···. 
# Also defining Tn and An to be the sum and the average, respectively, of Y1,Y2,··· ,Yn. 
# Checking if Yn →(p) 0, Tn →(p) 2, An →(p) 0 and Tn →(d) N(2,3)
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
# Let X1,X2,··· be U(0,1) i.i.d. random variables. 
# Defining Mn = max{X1,··· ,Xn}. 
# Proving that Mn →(p) 1, and Mn →(as) 1.
ques <- function(n){
  rn <- runif(n,min=0,max=1)
  m<- max(rn)
  return (m-1)
}
check.convergence(2000,500,ques,mode="p")
check.convergence(2000,500,ques,mode="as")

##########################################################
# Suppose we choose at random n numbers from interval [0, 1] with uniform distribution.
# Let Xn be the random variable describing nth choice, then showing that Xn obeys WLLN.
ques <- function(n){
  rn <- runif(n,min=0,max=1)
  return (rn/n)
}
check.convergence(2000,500,ques,mode="p")

#######################################################
# Generating sample path of Gaussian white noise GWN(0,1) and 
# checking if it is stationary time series.
ques <- function(n){
  rn <- rnorm(n, mean = 0, sd = 1)
  return (rn)
}
check.convergence(2000,500,ques,mode = "r")  # stationary

############################################################
# Generating the sample of Yt = β0 + β1t + εt, εt ∼ GWN(0,σ2), t = 0,1,··· for
# β0 = 0, β1 = 0.1 and σ2 = 1. 
# Checking if these are stationary time series.
# Also, after choosing a simple transformation Zt = Yt − β1t and 
# checking is resulting time series is stationary.

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
