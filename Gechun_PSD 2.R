#Problem set 2 
#Gechun Lin
#1. 
for (i in 1:7) {print(i^3)}

#2.
set.seed(14)
for (i in 1:1000) {
  dices <- sample(seq(1:6), size=2, replace=T)
  print(dices)
  sum <- sum(dices)
  if(i==1 & sum >=8) {
    print("The average number of dice casts per game is 1")
    break
  } else if(i!=1 & (dices[1]==2 | dices[1]==6 | dices[2]==2 | dices[2]==6)){
      print(paste("The average number of dice casts per game is", i))
      break}
}

#3.
#download the data
GSS <- read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
#build the function
vote.choice <- function(x) {
  if(x=="Trump") {
    a <- length(GSS$pres16[GSS$pres16=="Trump"])
    return(paste(c("The number of participants who voted for Trump is"), a))
  }
  else if(x=="Clinton") {
    b <- length(GSS$pres16[GSS$pres16=="Clinton"])
    return(paste(c("The number of participants who voted for Clinton is"), b))
  }
  else if(x=="Other") {
    c <- length(GSS$pres16[GSS$pres16!="Trump" & GSS$pres16!="Clinton"])
    return(paste(c("The number of participants who voted for other is"), c))
  }
  else {
    print(c("Please enter either ‘Trump’ ‘Clinton’ or ‘Other’ into the function to return a valid response."))
    }
}
#test the function
vote.choice("Trump")


#4.
#download the data
install.packages('fivethirtyeight')
library(fivethirtyeight)
cabinet_turnover
#construct a dataset "presidentdays"
president <- c("Carter","Reagan","Bush 41","Clinton","Bush 43","Obama","Trump")
days <- c(1461,2922,1461,2922,2922,2922,1105)
presidentdays <- data.frame(president, days)
#build the function
appoint <- function(y) {
  appointeedays.average <- mean(cabinet_turnover$days[cabinet_turnover$president==y]) 
  a <- appointeedays.average/presidentdays$days[presidentdays$president==y]
  return(a)
}
#test the function
appoint("Carter")

#5. 
#bulid the function
congress_stats <- function(z) {
  if(z=="congress") {
    average.age.era <- list()
    era <- unique(congress_age$congress)
  for (i in era) {
    average.age.era[[i]] <- mean(congress_age$age[congress_age$congress==i])
  }
  average.age <- unlist(average.age.era)
  df.1 <- data.frame(average.age, era)
  return(df.1)} else if(z=="state") {
  average.age.state <- list()
  state <- unique(congress_age$state)
  for (i in state) {
    average.age.state[[i]] <- mean(congress_age$age[congress_age$state==i])
  }
  average.age <- unlist(average.age.state)
  df.2 <- data.frame(average.age, state)
  return(df.2)}
}
#test the function
congress_stats("congress")
congress_stats("state")

