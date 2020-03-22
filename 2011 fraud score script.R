###Script for doing all chi square tests in one go
##Presidential elections
library(tidyverse)
rm(list=ls())
russia2011 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/russia 2011 duma election condensed.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2011$united.russia)%%10
kprfdigit<-(russia2011$KPRF)%%10
ldprdigit<-(russia2011$LDPR)%%10

russia2011<-cbind(russia2011, urdigit, kprfdigit, ldprdigit)
####

ids <- unique(russia2011$regionid)

chivalues<-matrix(NA, nrow=max(russia2011$regionid, na.rm=TRUE), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 80:nrow(chivalues)){
  group<-subset(russia2011, regionid== ids[i])
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

##Regionid 17 fails for LDPR --not enough digits--and for KPRF, will add entries manually: result is 0/1/1
##Region 79 fails for KPRF and LDRP--not enough digits

##Total fraudscores
fraudscore2011<-matrix(NA, nrow=nrow(chivalues), ncol=5)
fraudscore2011[,1]<-chivalues[,1]
fraudscore2011[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2011[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2011[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:nrow(chivalues)){
  fraudscore2011[i,5]<-sum(fraudscore2011[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=nrow(chivalues), ncol=4)
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2011, regionid==ids[i])
  if (nrow(group) > 0){
    total.votes <- sum(group$valid, na.rm=TRUE) + sum(group$invalid, na.rm = TRUE)
    ur.share <- sum(group$united.russia, na.rm=TRUE) / total.votes
    kprf.share <- sum(group$KPRF, na.rm=TRUE) / total.votes
    ldpr.share <- sum(group$LDPR, na.rm=TRUE) / total.votes
    voteshares[i, ]<-rbind(ids[i], ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2011 <- cbind(fraudscore2011, voteshares)
fraudscore2011 <- round(fraudscore2011, 4)
fraudscore2011 <- data.frame(fraudscore2011)

fraudscore2011 <- fraudscore2011 %>% rename(regionid = "X1",
                                            ur.fraud = "X2",
                                            kprf.fraud = "X3",
                                            ldpr.fraud = "X4",
                                            total.fraud = "X5",
                                            regionid.xtra = "X6",
                                            ur.voteshare = "X7",
                                            kprf.voteshare = "X8",
                                            ldpr.voteshare = "X9")

write.csv(fraudscore2011, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2011 presidential fraud scores_update.csv")
