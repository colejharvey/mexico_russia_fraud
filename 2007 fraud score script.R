###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2007<-read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2007 duma election condensed csv.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2007$unitedrussia)%%10
kprfdigit<-(russia2007$kprf)%%10
ldprdigit<-(russia2007$ldpr)%%10

russia2007<-cbind(russia2007, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=max(russia2007$regionid, na.rm = TRUE), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 52:nrow(chivalues)){
  group<-subset(russia2007, regionid==i)
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

##Regionid 51 (Ingush) fails for LDPR and KPRF--not enough digits--will add entries manually

##Total fraudscores
fraudscore2007<-matrix(NA, nrow=nrow(chivalues), ncol=5)
fraudscore2007[,1]<-chivalues[,1]
fraudscore2007[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2007[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2007[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:nrow(chivalues)){
  fraudscore2007[i,5]<-sum(fraudscore2007[i,2:4], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=nrow(chivalues), ncol=4)
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2007, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$unitedrussia) / total.votes
    kprf.share <- sum(group$kprf) / total.votes
    ldpr.share <- sum(group$ldpr) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2007 <- cbind(fraudscore2007, voteshares)

write.csv(fraudscore2007, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2007 presidential fraud scores_update.csv")
