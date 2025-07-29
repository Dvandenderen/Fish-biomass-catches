library(dplyr)
library(tidyr)

setwd("C:/Users/danie/Dropbox/Werk/Archief/2023 GEB Demersal fish and fisheries/Data analysis")

file_names <- c("Catch_all_1980_1984.rds","Catch_all_1985_1989.rds","Catch_all_1990_1994.rds",
                "Catch_all_1995_1999.rds","Catch_all_2000_2004.rds",
                "Catch_all_2005_2009.rds","Catch_all_2010_2014.rds",
                "Catch_all_2015_2015.rds")

load("Cells_in_EcoReg.RData")
AS <- subset(subcells,subcells$ECO_REG == "Gulf of Alaska")

tot <- c()
for(j in 1: length(file_names)){
  dat <- readRDS(file_names[j])
  dat <- subset(dat,dat$Cell %in% AS$Cell)
  dat <- subset(dat,dat$Funcgroup %in% c(1:24))
  dat$Catch <- dat$Reported + dat$IUU + dat$Discards
  dat$Funcgroup <- ifelse(dat$Funcgroup %in% c(1:3,7:9),"pel","dem")
  agg <- aggregate(
    x = list(Reported = dat$Reported, Catch = dat$Catch),
    by = list(Year = dat$IYear, Funcgroup = dat$Funcgroup),
    FUN = sum)
  tot <- rbind(tot,agg)
}

tot <- tot %>%
  pivot_wider(names_from = Funcgroup, 
              values_from = c(Reported,Catch), 
              values_fill = 0) %>% 
  mutate(Total_reported = rowSums(across(c("Reported_dem", "Reported_pel")), na.rm = TRUE),
         Total_catch   = rowSums(across(c("Catch_dem", "Catch_pel")), na.rm = TRUE)) %>%
  as.data.frame()

save(tot,file="C:/Users/danie/Documents/Online for git/Fish-biomass-catches/Data/Watson/GulfofAlaskaME.RData")



