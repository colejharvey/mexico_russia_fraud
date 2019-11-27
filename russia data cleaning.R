###Cleaning data for post-2018 update to Russia
###
###
library(tidyverse)
library(readxl)
library(reshape2)
library(interplot)
###Loading data
main.data <- read.csv("russia full ts data_diss.csv")
key.table <- read_excel("key table.xlsx")
ethnicity.data <- as_tibble(read_excel("russian ethnicity extrapolation.xlsx"))
spikes.data <- read_excel("kprf spikes combined.xlsx")

##Merging with spikes info

main.data <- left_join(main.data, spikes.data[,-2], by = c("regionid", "year"))
main.data <- main.data %>% mutate(total.count.suspicious.kprf = round(as.numeric(total.count.suspicious.kprf)))
main.data$n.precincts <- as.numeric(main.data$n.precincts)
main.data$percent.precincts.suspicious.kprf <- as.numeric(main.data$percent.precincts.suspicious.kprf)


###Merging with Russian ethnicity
ethnicity.data <- dplyr::select(ethnicity.data, -`2010`)

molten.ethnic <- melt(ethnicity.data, id.vars = "regionid", variable.name = "year", value.name = "pct.russian.extrap")

main.data <- main.data %>% mutate(year = as.factor(year))

main.data2 <- left_join(main.data, molten.ethnic, by = c("regionid", "year"))

###Creating new comp.scale
main.data2 <- main.data2 %>% mutate(comp.base03_18 = ifelse(is.na(reg.leg.margin) == TRUE, reg.gov.margin, reg.leg.margin))
main.data2 <- main.data2 %>% mutate(comp.base03_18.inv = comp.base03_18 * -1)
main.data2 <- main.data2 %>% mutate(comp.scale03_18 = scale(comp.base03_18.inv))

write.csv(main.data2, "russia full ts data.csv")

###
model.kprf <- (glm(kprf.fraud ~ comp.scale03_18 + urgov.pct.updated + presidential +
              comp.scale03_18*urgov.pct.updated + urgov + log(population) +
              log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
              log(govemploy.by.pop) + urban.pct + unemployment, data=main.data2))
summary(model.kprf)
interplot(model.kprf, var1 = "comp.scale03_18", var2 = "urgov.pct.updated")

model.ur <- (glm(ur.fraud ~ comp.scale03_18 + urgov.pct.updated + presidential +
              comp.scale03_18*urgov.pct.updated + urgov + log(population) +
              pensioners.per.1000 + poverty.pct + he.graduates.by1000pop +
              govemploy.by.pop + urban.pct + unemployment, data=main.data2))
summary(model.ur)
interplot(model.ur, var1 = "comp.scale03_18", var2 = "urgov.pct.updated")


main.data2 <- main.data2 %>% mutate(anyfraud = ifelse(kprf.fraud + ur.fraud > 0, 1, 0))
model.any <- (glm(anyfraud ~ comp.scale03_18 + urgov.pct.updated + presidential +
                   comp.scale03_18*urgov.pct.updated  + log(population) +
                   log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                   govemploy.by.pop + urban.pct + unemployment, data=main.data2))
summary(model.any)
interplot(model.any, var1 = "comp.scale03_18", var2 = "urgov.pct.updated")

###Testing spikes

model.count <- glm(total.count.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                     comp.scale03_18*urgov.pct.updated + urgov + log(population) +
                     log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                     log(govemploy.by.pop) + urban.pct + unemployment + n.precincts, data=main.data2,
                   family = "poisson")
summary(model.count)
interplot(model.count, var1="comp.scale03_18", var2="urgov.pct.updated")

library(censReg) #For censored DV
model.prop <- censReg(percent.precincts.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                   comp.scale03_18*urgov.pct.updated + urgov + log(population) +
                   log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                   log(govemploy.by.pop) + urban.pct + unemployment, data=main.data)
summary(model.prop)

model.prop.lm <- lm(percent.precincts.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                        comp.scale03_18*urgov.pct.updated + urgov + log(population) +
                        log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                        log(govemploy.by.pop) + urban.pct + unemployment, data=main.data2)
summary(model.prop.lm)
interplot(model.prop.lm, var1="comp.scale03_18", var2="urgov.pct.updated")

main.data2$spikes.suspicious.kprf <- (main.data2$percent.precincts.suspicious.kprf / 100) * main.data2$n.precincts

model.count2 <- glm(spikes.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                     comp.scale03_18*urgov.pct.updated + urgov + log(population) +
                     log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                     log(govemploy.by.pop) + urban.pct + unemployment + n.precincts, data=main.data2,
                   family = "poisson")
summary(model.count2)
interplot(model.count2, var1="comp.scale03_18", var2="urgov.pct.updated")


###Interacting with urgov
model.kprf.gov <- (glm(kprf.fraud ~ comp.scale03_18 + urgov.pct.updated + presidential +
                     urgov*urgov.pct.updated + urgov + log(population) +
                     log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                     log(govemploy.by.pop) + urban.pct + unemployment, data=main.data2))
summary(model.kprf.gov)
interplot(model.kprf.gov, var1 = "urgov", var2 = "urgov.pct.updated")


model.count.gov <- glm(total.count.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                     urgov*urgov.pct.updated + urgov + log(population) +
                     log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                     log(govemploy.by.pop) + urban.pct + unemployment + n.precincts, data=main.data2,
                   family = "poisson")
summary(model.count.gov)
interplot(model.count.gov, var1="urgov", var2="urgov.pct.updated")


model.count2.gov <- glm(spikes.suspicious.kprf ~ comp.scale03_18 + urgov.pct.updated + presidential +
                      urgov*urgov.pct.updated + urgov + log(population) +
                      log(pensioners.per.1000) + poverty.pct + he.graduates.by1000pop +
                      log(govemploy.by.pop) + urban.pct + unemployment + n.precincts, data=main.data2,
                    family = "poisson")
summary(model.count2.gov)
interplot(model.count2.gov, var1="urgov", var2="urgov.pct.updated")
