#Q4
k <-10
n <- 40
p <- 1/2
trials <-10000

gamble<-function(k,n,p){
  stake<-k
  while(stake>0 & stake<n){
    bet <-sample(c(-1,1),1,prob=c(1-p,p))
    stake<-stake+bet
    
  }
  if(stake==0){
    return (1)
  }
  else return (0)
}

simlist <-replicate(trials,gamble(k,n,p))
mean(simlist)
####################################################################
#Q2
trials<-10000
cards<-function(){
  count<-1
  rn<-sample(1:52,1)
  while(rn%%13!=1){
    rn<-sample(1:52,1)
    count<-count+1
  }
  return (count)
}

simlist <-replicate(trials,cards())
mean(simlist)
var(simlist)

#####################################################################
#Q1
trials <- 10000
p<-1/2

#For expected value of no of heads
heads<-function(p){
  a<-sample(c(0,1),1,prob=c(1-p,p))
  b<-sample(c(0,1),1,prob=c(1-p,p))
  c<-sample(c(0,1),1,prob=c(1-p,p))
  return (a+b+c)
}
simlist <-replicate(trials,heads(p))  
mean(simlist)

#For probability no of heads=1
prob_heads<-function(p){
  a<-sample(c(0,1),1,prob=c(1-p,p))
  b<-sample(c(0,1),1,prob=c(1-p,p))
  c<-sample(c(0,1),1,prob=c(1-p,p))
  if(a+b+c==1){
    return (1)
  }
  else{
    return (0)
  }
}
simlist <-replicate(trials,prob_heads(p))  
mean(simlist)

#for biased coin
p<-3/4
simlist <-replicate(trials,heads(p))  
mean(simlist)
simlist <-replicate(trials,prob_heads(p))  
mean(simlist)
#################################################################################

#Q7
val_y<-function(){
  x = runif(1, min=0, max=1)
  y = runif(1, min=0, max=x)
  return (y)
}
simlist <-replicate(trials,val_y())  
var(simlist)


#################################################################################
#Q3

#(a)
k<-30
trials<-10000
t<-20
bus<-function(k,t){
  val<-rexp(1, 1/k)
  if(val<t){
    return (1)
  }
  else{
    return (0)
  }
}
simlist <-replicate(trials,bus(k,t))  
mean(simlist)

#(b) Exact probability
pexp(20,rate=1/30)

#####################################################################################
#Q8
trials<-10000
accident <-function(){
  rn=runif(1,min=0,max=3)
  return (rpois(1,rn))
}
simlist <-replicate(trials,accident())  
mean(simlist)
var(simlist)

#####################################################################################
#Q5
trials <-10000
insurance<-function(){
  a <- rexp(1, rate = 1/500)
  if(a>100)
    return (a-100)
  else
    return (0)
}
simlist <-replicate(trials,insurance())
mean(simlist)
sd(simlist)
###################################################################################
#Q6
trials <- 10000
simlist <- numeric (trials)
toppings <- c("pepper", "pepperoni", "pineapple", "chicken")
for (i in 1:trials){
  pineapple <- 0
  pepperoni <- 0
  while (pineapple==0) {
    pick <- sample(toppings,1)
    if(pick=="pepperoni") pepperoni <- pepperoni + 1
    if(pick== "pineapple") pineapple <- 1
  }
  simlist[i] <-pepperoni
}
mean(simlist)
