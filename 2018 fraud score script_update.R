###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2018 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 Russia election data full ids.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2018$putin)%%10
kprfdigit<-(russia2018$grudinin)%%10
ldprdigit<-(russia2018$zhirinovsky)%%10

russia2018 <- cbind(russia2018, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=max(russia2018$regionid, na.rm=TRUE), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2018, regionid==i)
  if (nrow(group) > 0){
  table1<-table(group$urdigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)

  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  
  chivalues[i, ]<-rbind(i, chi1$p.value, chi2$p.value, chi3$p.value)  #Might have a type here, chi1 twice
  }
  else{next}
}


##Total fraudscores
fraudscore2018<-matrix(NA, nrow=nrow(chivalues), ncol=5)
fraudscore2018[,1]<-chivalues[,1]
fraudscore2018[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2018[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2018[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:nrow(chivalues)){
  fraudscore2018[i,5]<-sum(fraudscore2018[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=nrow(chivalues), ncol=4)
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2018, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$putin) / total.votes
    kprf.share <- sum(group$grudinin) / total.votes
    ldpr.share <- sum(group$zhirinovsky) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2018 <- cbind(fraudscore2018, voteshares)

write.csv(fraudscore2018, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 fraud scores_update.csv")
