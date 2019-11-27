###Script for doing all chi square tests in one go
##Presidential elections
rm(list=ls())
russia2012<-read.csv("C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/russia 2012 presidential election condensed csv.csv")

####Set up ones-digits if necessary##
putindigit<-(russia2012$putin)%%10
kprfdigit<-(russia2012$zyuganov)%%10
ldprdigit<-(russia2012$zhirinovskii)%%10
prokdigit<-(russia2012$prokhorov)%%10

russia2012<-cbind(russia2012, putindigit, kprfdigit, ldprdigit, prokdigit)
####

chivalues<-matrix(NA, nrow=90, ncol=4)
p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
i<-1
for (i in 2:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 19:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 25:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 60:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 71:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 79:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 81:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 84:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 87:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}

for (i in 90:90){
  group<-subset(russia2012, regionid==i)
  table1<-table(group$putindigit)
  table2<-table(group$kprfdigit)
  table3<-table(group$ldprdigit)
  table4<-table(group$prokdigit)
  
  chi1<-chisq.test(x=table1, p=p)
  chi2<-chisq.test(x=table2, p=p)
  chi3<-chisq.test(x=table3, p=p)
  chi4<-chisq.test(x=table4, p=p)
  
  
  chivalues[i, ]<-rbind(chi1$p.value, chi2$p.value, chi3$p.value, chi4$p.value)  #Might have a type here, chi1 twice
}


##Total fraudscores
fraudscore2012<-matrix(NA, nrow=90, ncol=5)
fraudscore2012[,1]<-ifelse(chivalues[,1] < .05, 1, 0)
fraudscore2012[,2]<-ifelse(chivalues[,2] < .05, 1, 0)
fraudscore2012[,3]<-ifelse(chivalues[,3] < .05, 1, 0)
fraudscore2012[,4]<-ifelse(chivalues[,4] < .05, 1, 0)

for(i in 1:83){
  fraudscore2012[i,5]<-sum(fraudscore2012[i,], na.rm=TRUE)
}

write.csv(fraudscore2012, "C:/Users/Cole/Documents/Grad school/Research topics/All Russian election data/2012 presidential fraud scores.csv")
