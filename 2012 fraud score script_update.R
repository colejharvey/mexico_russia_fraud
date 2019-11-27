###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2012 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/russia 2012 presidential election condensed csv.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2012$putin)%%10
kprfdigit<-(russia2012$zyuganov)%%10
ldprdigit<-(russia2012$zhirinovskii)%%10

russia2012<-cbind(russia2012, urdigit, kprfdigit, ldprdigit)
####

chivalues<-matrix(NA, nrow=max(russia2012$regionid, na.rm=TRUE), ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 1:nrow(chivalues)){
  group<-subset(russia2012, regionid==i)
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
  group<-subset(russia2012, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$putin) / total.votes
    kprf.share <- sum(group$zyuganov) / total.votes
    ldpr.share <- sum(group$zhirinovskii) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share, ldpr.share)  
  }
  else{next}
}

fraudscore2012 <- cbind(fraudscore2012, voteshares)

write.csv(fraudscore2012, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2012 presidential fraud scores_update.csv")
