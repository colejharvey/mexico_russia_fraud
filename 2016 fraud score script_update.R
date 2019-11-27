###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2016 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2016 Russia election data full ids.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2016$united.russia)%%10
kprfdigit<-(russia2016$kprf)%%10
ldprdigit<-(russia2016$ldpr)%%10

russia2016<-cbind(russia2016, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=92, ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 83:92){
  group<-subset(russia2016, regionid==i)
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

##Region 82 fails for KPRF and LDRP--not enough digits

##Total fraudscores
fraudscore2016<-matrix(NA, nrow=92, ncol=5)
fraudscore2016[,1]<-chivalues[,1]
fraudscore2016[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2016[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2016[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:92){
  fraudscore2016[i,5]<-sum(fraudscore2016[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=92, ncol=4)
i<-1
for (i in 1:92){
  group<-subset(russia2016, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$united.russia) / total.votes
    kprf.share <- sum(group$kprf) / total.votes
    ldpr.share <- sum(group$ldpr) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2016 <- cbind(fraudscore2016, voteshares)

write.csv(fraudscore2016, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2016 fraud scores_update.csv")
