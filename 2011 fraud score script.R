###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2011 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/russia 2011 duma election condensed.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2011$united.russia)%%10
kprfdigit<-(russia2011$KPRF)%%10
ldprdigit<-(russia2011$LDPR)%%10

russia2011<-cbind(russia2011, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=max(russia2011$regionid, na.rm=TRUE), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 83:nrow(chivalues)){
  group<-subset(russia2011, regionid==i)
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

##Regionid 17 fails for LDPR --not enough digits--and for KPRF, will add entries manually
##Region 82 fails for KPRF and LDRP--not enough digits

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
  group<-subset(russia2011, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$united.russia) / total.votes
    kprf.share <- sum(group$KPRF) / total.votes
    ldpr.share <- sum(group$LDPR) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2011 <- cbind(fraudscore2011, voteshares)

write.csv(fraudscore2011, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2011 presidential fraud scores_update.csv")
