###Script for doing all chi square tests in one go
##Presidential elections
library(tidyverse)
rm(list=ls())
russia2012 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/russia 2012 presidential election condensed csv.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2012$putin)%%10
kprfdigit<-(russia2012$zyuganov)%%10
ldprdigit<-(russia2012$zhirinovskii)%%10

russia2012<-cbind(russia2012, urdigit, kprfdigit, ldprdigit)
####
ids <- unique(russia2012$regionid)


chivalues<-matrix(NA, nrow=length(unique(russia2012$regionid)), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2012, regionid==ids[i])
  if (nrow(group) > 0){
  table1<-table(group$urdigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)

  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  
  chivalues[i, ]<-rbind(ids[i], chi1$p.value, chi2$p.value, chi3$p.value)  #Might have a type here, chi1 twice
  }
  else{next}
}



##Total fraudscores
fraudscore2012<-matrix(NA, nrow=nrow(chivalues), ncol=5)
fraudscore2012[,1]<-chivalues[,1]
fraudscore2012[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2012[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2012[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:nrow(chivalues)){
  fraudscore2012[i,5]<-sum(fraudscore2012[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=nrow(chivalues), ncol=4)
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2012, regionid==ids[i])
  if (nrow(group) > 0){
    total.votes <- sum(group$valid, na.rm=TRUE) + sum(group$invalid, na.rm=TRUE)
    ur.share <- sum(group$putin, na.rm=TRUE) / total.votes
    kprf.share <- sum(group$zyuganov, na.rm = TRUE) / total.votes
    ldpr.share <- sum(group$zhirinovskii, na.rm=TRUE) / total.votes
    voteshares[i, ]<-rbind(ids[i], ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2012 <- cbind(fraudscore2012, voteshares)

fraudscore2012 <- round(fraudscore2012, 4)
fraudscore2012 <- data.frame(fraudscore2012)

fraudscore2012 <- fraudscore2012 %>% rename(regionid = "X1",
                                            ur.fraud = "X2",
                                            kprf.fraud = "X3",
                                            ldpr.fraud = "X4",
                                            total.fraud = "X5",
                                            regionid.xtra = "X6",
                                            ur.voteshare = "X7",
                                            kprf.voteshare = "X8",
                                            ldpr.voteshare = "X9")

write.csv(fraudscore2012, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2012 presidential fraud scores_update.csv")


###Getting fraud scores and voteshares for the three regions left out of 2012 main data

regions2012 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2012 election data full tatarstan chechnya yaroslavl.csv")

####Set up ones-digits if necessary##
urdigit<-(regions2012$putin)%%10
kprfdigit<-(regions2012$zyuganov)%%10
ldprdigit<-(regions2012$zhirinovskii)%%10

regions2012<-cbind(regions2012, urdigit, kprfdigit, ldprdigit)
####
ids <- unique(regions2012$regionid)


chivalues<-matrix(NA, nrow=length(unique(regions2012$regionid)), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 3:nrow(chivalues)){
  group<-subset(regions2012, regionid==ids[i])
  if (nrow(group) > 0){
    table1<-table(group$urdigit)
    table2<-table(group$kprfdigit)
    table3<-table(group$ldprdigit)
    
    chi1<-chisq.test(x=table1, p=p)
    chi2<-chisq.test(x=table2, p=p)
    chi3<-chisq.test(x=table3, p=p)
    
    chivalues[i, ]<-rbind(ids[i], chi1$p.value, chi2$p.value, chi3$p.value)  #Might have a type here, chi1 twice
  }
  else{next}
}



##Total fraudscores
fraudscore2012<-matrix(NA, nrow=nrow(chivalues), ncol=5)
fraudscore2012[,1]<-chivalues[,1]
fraudscore2012[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2012[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2012[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:nrow(chivalues)){
  fraudscore2012[i,5]<-sum(fraudscore2012[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=nrow(chivalues), ncol=4)
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(regions2012, regionid==ids[i])
  if (nrow(group) > 0){
    total.votes <- sum(group$valid, na.rm=TRUE) + sum(group$invalid, na.rm=TRUE)
    ur.share <- sum(group$putin, na.rm=TRUE) / total.votes
    kprf.share <- sum(group$zyuganov, na.rm = TRUE) / total.votes
    ldpr.share <- sum(group$zhirinovskii, na.rm=TRUE) / total.votes
    voteshares[i, ]<-rbind(ids[i], ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2012 <- cbind(fraudscore2012, voteshares)

fraudscore2012 <- round(fraudscore2012, 4)
fraudscore2012 <- data.frame(fraudscore2012)

fraudscore2012 <- fraudscore2012 %>% rename(regionid = "X1",
                                            ur.fraud = "X2",
                                            kprf.fraud = "X3",
                                            ldpr.fraud = "X4",
                                            total.fraud = "X5",
                                            regionid.xtra = "X6",
                                            ur.voteshare = "X7",
                                            kprf.voteshare = "X8",
                                            ldpr.voteshare = "X9")
fraudscore2012

