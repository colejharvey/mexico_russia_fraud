##Testing mixture model approach (Juraj M.)

#library(pistar)


election.data <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2018 deputies csv.csv")

####Set up ones-digits if necessary##
pridigit<-(election.data$PRI)%%10
pandigit<-(election.data$PAN)%%10

election.data<-cbind(election.data, pridigit, pandigit)
###

pstarvalues<-matrix(NA, nrow=max(election.data$id_estado, na.rm=TRUE), ncol=4)
i<-1
for (i in 1:nrow(pstarvalues)){
  group<-subset(election.data, id_estado==i)
  if (nrow(group) > 0){
    table1<-table(group$pridigit)
    table2<-table(group$pandigit)

    d.table1 <- table1/sum(table1)
    d.table2 <- table2/sum(table2)
    #   Model density, e = expected
    e <- rep(.1, 1e1)
    names(e) <- names(d.table1) <- 0:9
    #   Compute the pi*
    ps.pri <- 1 - 1/max(e/d.table1)
    ps.pan <- 1 - 1/max(e/d.table2)

    pstarvalues[i, ]<-rbind(i, ps.pri, ps.pan, 1) 
  }
  else{next}
}
colnames(pstarvalues) <- c("id_estado", "pistar.pri", "pistar.pan", "presidential")

write.csv(pstarvalues, "mexico pstar 2018 pres.csv")


###Delta script
gd <- function(i) sum(abs(i-mean(i)))/(2*sum(i))

deltavalues<-matrix(NA, nrow=max(election.data$id_estado, na.rm=TRUE), ncol=4)
i<-1
for (i in 1:nrow(deltavalues)){
  group<-subset(election.data, id_estado==i)
  if (nrow(group) > 0){
    table1<-table(group$pridigit)
    table2<-table(group$pandigit)

    d.pri <- gd(table1)
    d.pan<- gd(table2)

    
    deltavalues[i, ]<-rbind(i, d.pri, d.pan, 0) 
  }
  else{next}
}

colnames(deltavalues) <- c("id_estado", "delta.pri", "delta.pan", "presidential")
deltavalues

write.csv(deltavalues, "./Mexico deltas/mexico delta 2018 deputies.csv")

