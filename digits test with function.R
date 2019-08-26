


###Load the data
electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2015 deputies.csv")

###Digits test function
digits.test <- function(precinct.data, party.variable, id.variable){
  p<-as.vector(rbind(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))
  i <- 1
  key <- select(precinct.data, id.variable)
  chivalues <- matrix(NA, nrow=max(key), ncol=1)
  fraudscore <- matrix(NA, nrow=max(key), ncol=1)
  for (i in 1:max(key)){
  precinct.data.sub <- filter(precinct.data, key == i)  
  party.results <- select(precinct.data.sub, party.variable)
  precinct.data.sub$digits <-  (party.results)%%10
  table1<-table(precinct.data.sub$digits)
  
  chi1<-chisq.test(x=table1, p=p)
  chivalues[i, ]<-chi1$p.value
  fraudscore[i,] <- ifelse(chivalues[i,] < .05, 1, 0)
  }
  
  output <- data.frame(cbind(chivalues, fraudscore, seq(1:max(key))))
  names(output) <- c("p.value", "fraudscore", id.variable)
  return(output)
}

mexico.digits2015.pri <- digits.test(electoral, "PRI", "ID_ESTADO")
mexico.digits2015.pan <- digits.test(electoral, "PAN", "ID_ESTADO")



###Loading 2018 deputies

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/20180708_2130_CW_diputaciones/diputaciones.csv")

mexico.digits2018deps.pri <- digits.test(electoral, "PRI", "ID_ESTADO")
mexico.digits2018deps.pan <- digits.test(electoral, "PAN", "ID_ESTADO")

###Loading 2018 presidential

electoral <- read.csv("C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/mexico 2018 presidential csv.csv")

mexico.digits2018pres.pri <- digits.test(electoral, "PRI", "ID_ESTADO")
mexico.digits2018pres.pan <- digits.test(electoral, "PAN", "ID_ESTADO")

###Saving the output

write.csv(mexico.digits2015.pan, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2015 Mexico deputies PAN fraud scores.csv")
write.csv(mexico.digits2015.pri, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2015 Mexico deputies PRI fraud scores.csv")
write.csv(mexico.digits2018deps.pan, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2018 Mexico deputies PAN fraud scores.csv")
write.csv(mexico.digits2018deps.pri, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2018 Mexico deputies PRI fraud scores.csv")
write.csv(mexico.digits2018pres.pan, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2018 Mexico presidential PAN fraud scores.csv")
write.csv(mexico.digits2018pres.pri, "C:/Users/Cole/Documents/Grad school/Dissertation/Mexico elections data/Mexico fraud scores/2018 Mexico presidential PRI fraud scores.csv")


