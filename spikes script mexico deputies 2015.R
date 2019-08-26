##Mexico deputies 2015
###PRI votes

rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2015 deputies.csv")
electoral2 <- as_tibble(electoral)  #Set as a tibble

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$ID_ESTADO), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("TOTAL_VOTOS", "PRI", "LISTA_NOMINAL") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$ID_ESTADO)){
  
  sub.reg <- filter(electoral2, electoral2$ID_ESTADO == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")     #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
  #Drop rows where n of votes for a party exceeds total n of votes
  fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$ESTADO)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/Mexico2015deps/", as.character(fraud.estimates[i,1]),
                    ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()
  
}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/Mexico/fraud_estimates_mexico_2015_deps_pri.csv")

######

###PAN votes

rm(list=ls())
library(tidyverse)
library(spikes)

#Load the data

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2015 deputies.csv")
electoral2 <- as_tibble(electoral)  #Set as a tibble

rm(electoral)

#Create container for the results of the spikes program

fraud.estimates <- matrix(NA, nrow=max(electoral2$ID_ESTADO), ncol=5) #5 columns: region.id or name, output from spikes, number of precincts where t > n or v > t, total precicnts, and %suspicious * total + additional suspicious
colnames(fraud.estimates) <- c("region.name", "percent.precincts.suspicious", "additional.precincts.suspicious", "n.precincts", "total.count.suspicious")
fraud.estimates <- as_tibble(fraud.estimates)
class(fraud.estimates[[2]])<-"numeric"
class(fraud.estimates[[3]])<-"numeric"
class(fraud.estimates[[4]])<-"numeric"
class(fraud.estimates[[5]])<-"numeric"

#Keep variables needed for spikes (note UR here)--n of ballots cast, ballots cast for a party, and n of registered voters

keepvars <- c("TOTAL_VOTOS", "PAN", "LISTA_NOMINAL") 

#Set up a loop to calculate spikes for each region
i <- 1
for(i in 1:max(electoral2$ID_ESTADO)){
  
  sub.reg <- filter(electoral2, electoral2$ID_ESTADO == i) #Pick out one region (6=Donetska)
  sub.reg.spikes <- sub.reg[keepvars]
  names(sub.reg.spikes) <- c("t","v", "N")     #Rename variables to fit spikes: N = number of registered voters, t = number who voted, v = number of votes for a party 
  if(nrow(sub.reg.spikes) < 100) next #skips empty / small datasets (regions)
  sub.reg.spikes <- filter(sub.reg.spikes, t <= N & t > v)           #Exclude rows where turnout exceeds 100%
  #Drop rows where n of votes for a party exceeds total n of votes
  fraud.estimates[i,3] <- nrow(sub.reg) - nrow(sub.reg.spikes)         #Save number of prima facie suspicious precincts
  
  sub.reg.spikes <- filter(sub.reg.spikes, t > 0 & v > 0)            #Drop rows with 0 votes cast to make code work
  set.seed(1274)                                                     #Set seed for spikes   
  out <- spikes(sub.reg.spikes, resamples=750)                       #Runs the spikes program                                                                      #Drop rows with 0 votes for party to make code work
  fraud.estimates[i,1] <- as.character(sub.reg$ESTADO)[i]
  fraud.estimates[i,2] <- as.numeric(summary(out)[1])
  fraud.estimates[i,4] <- nrow(sub.reg)
  fraud.estimates[i,5] <- ((fraud.estimates[i,2] / 100) * nrow(sub.reg.spikes)) + fraud.estimates[i,3] 
  filename <- paste("C:/Users/Cole/Documents/Research projects/mexico_russia_fraud/spike_plots/Mexico2015deps/PAN/", as.character(fraud.estimates[i,1]),
                    ".png", sep="")
  png(filename = filename)
  plot.out(out)
  dev.off()
  cat("Region estimate complete for region ", as.character(fraud.estimates[i,1]), ". Estimate: ", as.character(fraud.estimates[i,2]))
  alarm()
  
}  #Paused--set beginning of loop to i = 19 to resume


write.csv(fraud.estimates, "~/Research projects/mexico_russia_fraud/Mexico/fraud_estimates_mexico_2015_deps_pan.csv")


