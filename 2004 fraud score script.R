###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2004<-read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2004 presidential election condensed csv.csv")

####Set up ones-digits if necessary##
urdigit<-(russia2004$Putin)%%10
kprfdigit<-(russia2004$Kharitonov)%%10
#ldprdigit<-(russia2007$ldpr)%%10

russia2004<-cbind(russia2004, urdigit, kprfdigit)
####

chivalues<-matrix(NA, nrow=88, ncol=3)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 87:88){
  group<-subset(russia2004, regionid==i)
  if (nrow(group) > 0){
  table1<-table(group$urdigit)
  table2<-table(group$kprfdigit)
  #table3<-table(group$ldprdigit)

  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  #chi3<-chisq.test(x=table3, p=p)
  
  chivalues[i, ]<-rbind(i, chi1$p.value, chi2$p.value)  #Might have a type here, chi1 twice
  }
  else{next}
}

##Regionid 86 for UR and KPRF--not enough digits--will add entries manually

##Total fraudscores
fraudscore2004<-matrix(NA, nrow=88, ncol=4)
fraudscore2004[,1]<-chivalues[,1]
fraudscore2004[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2004[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
#fraudscore2004[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

i<-1
for(i in 1:88){
  fraudscore2004[i,4]<-sum(fraudscore2004[i,2:3], na.rm=FALSE)
}


###Getting party vote-shares

voteshares <- matrix(NA, nrow=88, ncol=3)
i<-1
for (i in 1:88){
  group<-subset(russia2004, regionid==i)
  if (nrow(group) > 0){
    total.votes <- sum(group$valid) + sum(group$invalid)
    ur.share <- sum(group$Putin) / total.votes
    kprf.share <- sum(group$Kharitonov) / total.votes
    #ldpr.share <- sum(group$ldpr) / total.votes
    voteshares[i, ]<-rbind(i, ur.share, kprf.share)  
  }
  else{next}
}

fraudscore2004 <- cbind(fraudscore2004, voteshares)

write.csv(fraudscore2004, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2004 presidential fraud scores_update.csv")
