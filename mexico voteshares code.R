
library(tidyverse)

###Getting party vote-shares

election.data <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2009 deputies csv.csv")

voteshares <- matrix(NA, nrow=max(election.data$id_estado), ncol=4)
i<-1
for (i in 1:max(election.data$id_estado)){
  group<-subset(election.data, id_estado==i)
  total.votes.group <- sum(group$total.votes, na.rm=TRUE)
  if (nrow(group) > 0){
    pan.share <- sum(group$PAN, na.rm = TRUE) / total.votes.group
    pri.share <- sum(group$PRI, na.rm = TRUE) / total.votes.group
    prd.share <- sum(group$PRD, na.rm = TRUE) / total.votes.group
    voteshares[i, ]<-rbind(i, round(pan.share, 4), round(pri.share, 4), round(prd.share, 4))  
  }
  else{next}
}

write.csv(voteshares, "2009 mexico deputies voteshares.csv")

