library(tidyverse)

russia2016 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2016 Russia election data full.csv")
region.names <- as.character(unique(russia2016$region.name))

write.csv(region.names, "region_names_2016.csv")


russia2018 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 Russia election data full.csv")
region.names <- as.character(unique(russia2018$region.name))

write.csv(region.names, "region_names_2018.csv")


##Getting regionids for the two years

russia2016 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2016 Russia election data full.csv")
idlist <- read.csv("russia region ids.csv")

idlist <- idlist %>% mutate(region.name = translit.name.16)

russia2016 <- idlist %>% select(regionid, region.name) %>%  right_join(russia2016,  by="region.name")
write.csv(russia2016, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2016 Russia election data full ids.csv")




russia2018 <- read.csv("C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 Russia election data full.csv")
idlist <- read.csv("russia region ids.csv")

idlist <- idlist %>% mutate(region.name = translit.name.18)

russia2018 <- idlist %>% select(regionid, region.name) %>%  right_join(russia2018,  by="region.name")
write.csv(russia2018, "C:/Users/Cole/Documents/Research topics literature/All Russian election data/2018 Russia election data full ids.csv")

