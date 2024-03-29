# The Canadian forest fire weather index is widely as means of to estimate the risk of wildfire. 
# The Ontario Ministry of Natural Resources uses the index to classify each day’s risk of forest fire as either nil, low, moderate, high or extreme. 
# Given Transition probability matrix for the five state Markov chain for the daily changes in the index.
# Calculating long term likelihood of risk for a typical day in early summer.
markov <- function(init,P,n,states){
  ans <- c(1:n+1)
  count <- 1
  num <- length(states)
  for(value in init){
    if(value==1){
      ans[1] <- states[count]
      break
    }
    else{
      count <- count+1
    }
  }
  
  for(i in 1:n){
      a <- sample(1:num,size = 1,prob = P[count,])
      ans[i+1] <- states[a]
      count <- a
  }
  return (ans)
}


P <- matrix(c(0.575,0.118,0.172 ,0.109,0.026,0.453 ,0.243 ,0.148 ,0.123 ,0.033,0.104, 0.343 ,0.367 ,0.167 ,0.019,0.015 ,0.066 ,0.318, 0.505 ,0.096, 0,.060 ,0.149 ,0.567 ,0.224),
            nrow = 5,byrow = TRUE)
states <- c("Nil", "Low", "Moderate", "High", "Extreme")
rownames(P) <- states
colnames(P) <- states
init <- c(1,0,0,0,0)
sim <- replicate(10000,markov(init,P,10,states)[11])
table(sim)/10000

######################################################################################3
# University administrators have developed a Markov model to simulate graduation rates at their school. 
# Student might drop out, repeat a year or move on to the next year. 
# Student have a 3% chance of repeating a year. 
# First years and second years have a 6% of dropping out. 
# For third years and fourth years the drop out rate is 4%. 
# Simulating the long term probability that a new student graduates. 
# Further, modelling it as absorbing chain and 
# finding the probability of drop and graduating for each year students.

markov <- function(init,P,n,states){
  count<-1
  num<- length(states)
  ans <- c(1:n+1)
  for(value in init){
    if(value==1){
      ans[1] <- states[count]
      break
    }
    else{
      count <- count +1
    }
  }
  for(i in 1:n){
    count <- sample(1:num,size = 1,prob = P[count,])
    ans[i+1] <- states[count]
  }
  return (ans)
}


init <- c(0,1,0,0,0,0)
P <- matrix(c(1,0,0,0,0,0,0.06,0.03,0.91,0,0,0,0.06,0,0.03,0.91,0,0,0.04,0,0,0.03,0.93,0,0.04,0,0,0,0.03,0.93,0,0,0,0,0,1),nrow = 6,byrow = TRUE)
states <- c("Drop","First","Second","Third","Fourth","Grad")
rownames(P) <- states
colnames(P) <- states

sim <- replicate(10000,markov(init,P,10,states)[11])
table(sim)/10000

#absorbing chain
markov <- function(init,P,states){
  num<- length(states)
  a <- sample(1:num,size = 1,replace = TRUE,prob = P[init,])
  while(a!=5 && a!=6){
    a <- sample(1:num,size = 1,replace = TRUE,prob = P[a,])
  }
  return (a)
  
}
P <- matrix(c(0.03,0.91,0,0,0.06,0,0,0.03,0.91,0,0.06,0,0,0,0.03,0.93,0.04,0,0,0,0,0.03,0.04,0.93,0,0,0,0,1,0,0,0,0,0,0,1),nrow = 6,byrow = TRUE)
states <- c("First","Second","Third","Fourth","Drop","Grad")
rownames(P) <- states
colnames(P) <- states

for(i in 1:4){
  sim <- replicate(10000,markov(i,P,states))
  print((table(sim)/10000))
}
##########################################################################################
# After work, angel goes to the gym and either does aerobics, weights, yoga or goes for jogging. 
# Each day Angle decides her workout routine based on what she did the previous day according to the Markov transition matrix given.
# Simulating the long term probability that she goes for jogging and 
# Comparing this with stationary distribution.

markov <- function(init,P,n,states){
  ans <- c(1:n+1)
  count <- init
  num<- length(states)
  ans[1] <- states[init]
  for(i in 1:n){
    a <- sample(1:num,size = 1,replace = TRUE,prob = P[count,])
    ans[i+1] <- states[a]
    count <- a
  }
  return (ans)
  
}
P <- matrix(c(0.1,0.2,0.4,0.3,0.4,0,0.4,0.2,0.3,0.3,0,0.4,0.2,0.1,0.4,0.3),
            nrow = 4,byrow = TRUE)
states <- c("Aerobics","Jogging" ,"Weights" ,"Yoga")
rownames(P) <- states
colnames(P) <- states
for(i in 1:4){
  sim <- replicate(10000,markov(i,P,10,states)[11])
  print((table(sim)/10000))
}

#######################################################################################
# Finding μ11 from given transition matrix.

markov <- function(init,P,states){
  num<- length(states)
  a <- sample(1:num,size = 1,replace = TRUE,prob = P[init,])
  steps <- 1
  while(a!=init){
    a <- sample(1:num,size = 1,replace = TRUE,prob = P[a,])
    steps <- steps+1
  }
  return (steps)
  
}
P <- matrix(c(0,1,0,1/2,0,1/2,1/3,1/3,1/3),
            nrow = 3,byrow = TRUE)
states <- c("one","two" ,"three" )
rownames(P) <- states
colnames(P) <- states

sim <- replicate(10000,markov(1,P,states))
mean(sim)
#####################################################################################
# A biased coin comes up head with probability 2/3 and tails with probability 1/3. 
# The coin is repeatedly flipped. 
# Simulating to find the total numbers average number of flips needed, until the pattern HTHTH first appears.

markov <- function(P,states,init,n){
  
  count <-1
  a<-init
  num <- length(states)
  while(a!=n){
    a <- sample(1:num,size = 1,prob = P[a,])
    count<-count+1
  }
  return (count)
}

P<- matrix(c(1/3,2/3,0,0,0,0,0,2/3,1/3,0,0,0,1/3,0,0,2/3,0,0,0,2/3,0,0,1/3,0,1/3,0,0,0,0,2/3,
             0,0,0,0,0,1),nrow = 6,byrow = TRUE)
states <- c("T","H","HT","HTH","HTHT","HTHTH")
rownames(P)<-states
colnames(P)<-states
sim <- replicate(10000,markov(P,states,1,6))
mean(sim)

##################################################################################
# Some winter days in Minnesota it seems like the snow will never stop. 
# A Minnesotan’s view of winter might be described by the given transition matrix for a weather Markov chain, representing rain, snow and clear.
# Finding the fraction of days on which it will snow.

markov <- function(init,P,n,states){
  count<-1
  num<- length(states)
  ans <- c(1:n+1)
  for(value in init){
    if(value==1){
      ans[1] <- states[count]
      break
    }
    else{
      count <- count +1
    }
  }
  for(i in 1:n){
    count <- sample(1:num,size = 1,prob = P[count,])
    ans[i+1] <- states[count]
  }
  return (ans)
}
P <- matrix(c(0.2,0.6,0.2,0.1,0.8,0.1,0.1,0.6,0.3),nrow=3,byrow = TRUE)
init <- c(0,1,0)
states <- c("rain","snow","clear")
rownames(P) <- states
colnames(P) <- states
sim <- replicate(10000,markov(init,P,10,states)[11])
print((table(sim)/10000)["snow"])

#####################################################################################
# Danny’s daily lunch choices are modeled by a Markov chain with transition probability matrix
# On Sunday, Danny chooses lunch uniformly at random. 
# Finding the probability that he chooses sushi on the following Wednesday and Friday, and pizza on Sunday.

markov <- function(P,states){
  num <- length(states)
  a <- sample(1:num,size = 1,prob = c(1/4,1/4,1/4,1/4))
  sun <- states[a]
  for(i in 1:3){
    a<-sample(1:num,1,prob = P[a,]) 
  }
  wed<-states[a]
  for(i in 1:2){
    a<-sample(1:num,1,prob = P[a,]) 
  }
  fri <- states[a]
  if(sun=="Pizza" && wed=="Sushi" && fri=="Sushi"){
    return (1)
  }
  else{
    return (0)
  }
  
}

P <- matrix(c(0,0.5,0.5,0,0.5,0,0.5,0,0.4,0,0,0.6,0,0.2,0.6,0.2),
            nrow = 4,byrow = TRUE)
states <- c("Burrito" ,"Falafel" ,"Pizza" ,"Sushi" )
rownames(P) <- states
colnames(P) <- states

sim <- replicate(10000,markov(P,states))
mean(sim)

#########################################################################################
# Gambler’s ruin: A gambler starts with $2 and plays a game where the chance of winning each round is 60%. 
# The gambler either wins or loses $1 on each round. 
# The game stops when the gambler either gains $5 or goes bust. 
# Modelling this problem as absorbing Markov chain and 
# finding the probability that the gambler is eventually ruined.

markov <- function(init,P,states){
  num<- length(states)
  a <- sample(1:num,size = 1,replace = TRUE,prob = P[init,])
  while(a!=5 && a!=6){
    a <- sample(1:num,size = 1,replace = TRUE,prob = P[a,])
  }
  return (states[a])
  
}

P <- matrix(c(0,0.6,0,0,0.4,0,0.4,0,0.6,0,0,0,0,0.4,0,0.6,0,0,0,0,0.4,0,0,0.6,0,0,0,0,1,0,0,0,0,0,0,1),
            nrow = 6,byrow = TRUE)
states <- c("one", "two", "three", "four","zero","five")
rownames(P) <- states
colnames(P) <- states
sim <- replicate(10000,markov(2,P,states))
print((table(sim)/10000))
#########################################################################################
# A coin is flipped repeatedly until three heads in a row appears. 
# Using Markov chain model to find the expected number of flips needed.

markov <- function(P,states,init,n){
  
  count <-1
  a<-init
  num <- length(states)
  while(a!=n){
    a <- sample(1:num,size = 1,prob = P[a,])
    count<-count+1
  }
  return (count)
}
P<- matrix(c(1/2,1/2,0,0,1/2,0,1/2,0,1/2,0,0,1/2,0,0,0,1),nrow = 4,byrow = TRUE)
states <- c("0H","1H","2H","3H")
rownames(P)<-states
colnames(P)<-states
sim <- replicate(10000,markov(P,states,1,4))
mean(sim)

########################################################################################