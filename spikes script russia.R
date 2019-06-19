###Trying out Rozenas method of fraud detection (manipulation of vote-shares)
##Russia 2016
rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/2016 Russia election data full.csv")
electoral$temp.id <- as.numeric(as.factor(electoral$region.name))  #Set factor to $region.name for region; region.district for district

electoral2 <- as_tibble(electoral)  #Set as a tibble

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$temp.id), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("ballots.received", "united.russia", "voter.list") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$temp.id)){
  
  sub.reg <- subset(electoral2, electoral2$temp.id == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")                            #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
                                                                     #Drop rows where n of votes for a party exceeds total n of votes
fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$region.name)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/", as.character(fraud.estimates[i,1]),
                  ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()

}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/fraud_estimates_russia_2016.csv")



##Russia 2018
#Current a problem with the region names--showing up as weird strings in R, but not in excel
rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/2018 Russia election data full.csv")
electoral$temp.id <- as.numeric(as.factor(electoral$region.name))  #Set factor to $region.name for region; region.district for district

electoral2 <- as_tibble(electoral)  #Set as a tibble

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$temp.id), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("ballots.received", "putin", "voter.list") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$temp.id)){
  
  sub.reg <- subset(electoral2, electoral2$temp.id == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")                            #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
  #Drop rows where n of votes for a party exceeds total n of votes
  fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$region.name)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/2018/", as.character(fraud.estimates[i,1]),
                    ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()
  
}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/fraud_estimates_russia_2018.csv")



#####Beginning older data####

##Russia 2012
rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/russia 2012 presidential election condensed csv.csv")
key.table <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Region ID lists/russia region ids.csv")  #Loads the regionid values
key.table <- rename(key.table, english.name = ï..english.name)

electoral2 <- as_tibble(electoral)  #Set as a tibble
electoral2 <- key.table  %>% 
  select(-russian.name) %>%
  left_join(electoral2, by = "regionid")

electoral2$ballots.received <- electoral2$valid + electoral2$invalid
electoral2 <- as_tibble(electoral2)

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$regionid), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("ballots.received", "putin", "voters.on.list") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$regionid)){
  
  sub.reg <- subset(electoral2, electoral2$regionid == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")                            #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
  #Drop rows where n of votes for a party exceeds total n of votes
  fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$english.name)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/2012/", as.character(fraud.estimates[i,1]),
                    ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()
  
}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/fraud_estimates_russia_2012.csv")



###2011

##Russia 2012
rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/russia 2011 duma election condensed.csv")
key.table <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Region ID lists/russia region ids.csv")  #Loads the regionid values
key.table <- rename(key.table, english.name = ï..english.name)

electoral2 <- as_tibble(electoral)  #Set as a tibble
electoral2 <- key.table  %>% 
  select(-russian.name) %>%
  left_join(electoral2, by = "regionid")

electoral2$ballots.received <- electoral2$valid + electoral2$invalid
electoral2 <- as_tibble(electoral2)

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$regionid), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("ballots.received", "united.russia", "voterlist") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$regionid)){
  
  sub.reg <- subset(electoral2, electoral2$regionid == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")                            #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
  #Drop rows where n of votes for a party exceeds total n of votes
  fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$english.name)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/2011/", as.character(fraud.estimates[i,1]),
                    ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()
  
}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/fraud_estimates_russia_2011.csv")



