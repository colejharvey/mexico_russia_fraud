###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2003<-read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2003 duma election condensed big csv.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2003$united.russia)%%10
kprfdigit<-(russia2003$kprf)%%10
ldprdigit<-(russia2003$ldpr)%%10

russia2003<-cbind(russia2003, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=88, ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 87:88){
  group<-subset(russia2003, regionid==i)
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

##Regionid 86 fails for KPRF (natural) and LDPR--not enough digits--will add entries manually

##Total fraudscores
fraudscore2003<-matrix(NA, nrow=88, ncol=5)
fraudscore2003[,1]<-chivalues[,1]
fraudscore2003[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2003[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2003[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:88){
  fraudscore2003[i,5]<-sum(fraudscore2003[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=88, ncol=4)
i<-1
for (i in 1:88){
  group<-subset(russia2003, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$united.russia) / total.votes
    kprf.share <- sum(group$kprf) / total.votes
    ldpr.share <- sum(group$ldpr) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2003 <- cbind(fraudscore2003, voteshares)

write.csv(fraudscore2003, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2003 duma fraud scores_update.csv")
