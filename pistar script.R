##Testing mixture model approach (Juraj M.)

#library(pistar)


electoral <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 Russia election data full ids.csv")

####Set up ones-digits if necessary##
urdigit<-(electoral$putin)%%10
kprfdigit<-(electoral$grudinin)%%10
ldprdigit<-(electoral$zhirinovsky)%%10

electoral<-cbind(electoral, urdigit, kprfdigit, ldprdigit)
###

pstarvalues<-matrix(NA, nrow=max(electoral$regionid, na.rm=TRUE), ncol=4)
i<-1
for (i in 83:nrow(pstarvalues)){
  group<-subset(electoral, regionid==i)
  if (nrow(group) > 0){
    table1<-table(group$urdigit)
    table2<-table(group$kprfdigit)
    table3<-table(group$ldprdigit)
    
    d.table1 <- table1/sum(table1)
    d.table2 <- table2/sum(table2)
    d.table3 <- table3/sum(table3)
    #   Model density, e = expected
    e <- rep(.1, 1e1)
    names(e) <- names(d.table1) <- 0:9
    #   Compute the pi*
    ps.ur <- 1 - 1/max(e/d.table1)
    ps.kp <- 1 - 1/max(e/d.table2)
    ps.ld <- 1 - 1/max(e/d.table3)
    
    pstarvalues[i, ]<-rbind(i, ps.ur, ps.kp, ps.ld) 
  }
  else{next}
}

write.csv(pstarvalues, "russia pstar 2016.csv")

###Delta script
gd <- function(i) sum(abs(i-mean(i)))/(2*sum(i))

deltavalues<-matrix(NA, nrow=max(electoral$regionid, na.rm=TRUE), ncol=4)
i<-1
for (i in 1:nrow(deltavalues)){
  group<-subset(electoral, regionid==i)
  if (nrow(group) > 0){
    table1<-table(group$urdigit)
    table2<-table(group$kprfdigit)
    table3<-table(group$ldprdigit)
    
    
    d.ur <- gd(table1)
    d.kp <- gd(table2)
    d.ld <- gd(table3)
    
   
    
    deltavalues[i, ]<-rbind(i, d.ur, d.kp, d.ld) 
  }
  else{next}
}

colnames(deltavalues) <- c("regionid", "delta.ur", "delta.kprf", "delta.ldpr")

write.csv(deltavalues, "./Russia deltas/russia delta 2018.csv")



