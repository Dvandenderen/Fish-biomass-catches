
# script to estimate the conversion from SSB to TB - which is needed as too many
# US stocks only include SSB, whereas the comparison with survey data needs, as
# much as possible, TB

# load all NOAA stocks -> 
# get the once with good data ->
# match with management assessment area (using RAM legacy and J. Rising polygons) -> 
# match with aquamaps distribution area ->
# spread the biomass and the catch over the area

library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(DBI)
library(RSQLite)
library(marmap)

# load stock assessment biomass and reported landings
stock1 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_1.xlsx",1,colNames = F)
stock2 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_2.xlsx",1,colNames = F)
stock3 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_3.xlsx",1,colNames = F)
stock4 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_4.xlsx",1,colNames = F)

stock <- cbind(stock1,stock2,stock3,stock4)

# get all stocks
stockid <- unique(as.character(stock[1,]))
stockid <- stockid[-c(1:2)]

Fspec <- which(stock[1,] == stockid[1] & stock[3,] == "Catch")
Fmo <- if(length(Fspec) > 0){as.numeric(stock[6:166,Fspec])} else {rep(NA,161)}
Fmo_udes <- if(length(Fspec) > 0){stock[4,Fspec]} else {NA}
Fmo_u    <- if(length(Fspec) > 0){stock[5,Fspec]} else {NA}

Aspec    <- which(stock[1,] == stockid[1] & stock[3,] == "Abundance")
Abu      <- if(length(Aspec) > 0){as.numeric(stock[6:166,Aspec])} else {rep(NA,161)}
Abu_udes <- if(length(Aspec) > 0){stock[4,Aspec]} else {NA}
Abu_u    <- if(length(Aspec) > 0){stock[5,Aspec]} else {NA}

stockdat <- data.frame(Catch = Fmo, Abundance = Abu, stocklong = stockid[1], 
                       unitC_des = Fmo_udes, unitC = Fmo_u,
                       unitA_des = Abu_udes, unitA = Abu_u, Year = 1872:2032)

for (j in 2:length(stockid)){
  Fspec <- which(stock[1,] == stockid[j] & stock[3,] == "Catch")
  Fmo <- if(length(Fspec) > 0){as.numeric(stock[6:166,Fspec])} else {rep(NA,161)}
  Fmo_udes <- if(length(Fspec) > 0){stock[4,Fspec]} else {NA}
  Fmo_u    <- if(length(Fspec) > 0){stock[5,Fspec]} else {NA}
  
  Aspec    <- which(stock[1,] == stockid[j] & stock[3,] == "Abundance")
  Abu      <- if(length(Aspec) > 0){as.numeric(stock[6:166,Aspec])} else {rep(NA,161)}
  Abu_udes <- if(length(Aspec) > 0){stock[4,Aspec]} else {NA}
  Abu_u    <- if(length(Aspec) > 0){stock[5,Aspec]} else {NA}
  
  stock_sub <- data.frame(Catch = Fmo, Abundance = Abu, stocklong = stockid[j], 
                          unitC_des = Fmo_udes, unitC = Fmo_u,
                          unitA_des = Abu_udes, unitA = Abu_u, Year = 1872:2032)
  stockdat <- rbind(stockdat,stock_sub)
}

# correct and combine
tr <- subset(stockdat,!(is.na(stockdat$Catch)))
tr <- subset(tr,!(is.na(tr$Abundance)))

# remove complexes and invertebrates
tr <- tr %>%
  filter(!str_detect(stocklong, regex("complex|crab|shrimp|squid|scallop", ignore_case = TRUE)))

tr <- subset(tr,tr$unitC %in% c("Metric Tons","mt","Metric tons","Thousand Metric Tons",
                                "Kilograms","1000 mt","Thousand Kilograms","Thousand Pounds",
                                "Thousand lbs","Million lbs","lbs","Pounds Whole Weight",
                                "Million Pounds","Pounds","lbs.","Kilograms - Whole Weight"))
tr <- subset(tr,tr$unitA %in% c("Metric Tons","Thousand Metric Tons","lbs","Million lbs.",
                                "Million Pounds","mt","Pounds","Thousand Pounds"))

tr <- subset(tr,!(tr$unitA_des %in% c("Area-Swept Biomass","Area-Swept Total Stock Biomass",
                                      "Spawning Stock Biomass - Gonad Weight (Point Estimate)",
                                      "Survey Biomass - 30+cm fish","Total Stock Biomass - Meat Weight")))

tr$Catch  <- ifelse(tr$unitC == "Thousand Metric Tons",tr$Catch*1000, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Kilograms",tr$Catch/1000, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "1000 mt",tr$Catch*1000, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Thousand Pounds",tr$Catch/2.205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Thousand lbs",tr$Catch/2.205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Million lbs",tr$Catch/2.205*1000, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "lbs",tr$Catch/2205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Pounds Whole Weight",tr$Catch/2205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Million Pounds",tr$Catch/2.205*1000, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Pounds",tr$Catch/2205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "lbs.",tr$Catch/2205, tr$Catch)
tr$Catch  <- ifelse(tr$unitC == "Kilograms - Whole Weight",tr$Catch/1000, tr$Catch)
tr$unitC  <- "Metric Tons"

tr$Abundance  <- ifelse(tr$unitA == "Thousand Metric Tons",tr$Abundance*1000, tr$Abundance)
tr$Abundance  <- ifelse(tr$unitA == "lbs",tr$Abundance/2205, tr$Abundance)
tr$Abundance  <- ifelse(tr$unitA == "Million lbs.",tr$Abundance/2.205*1000, tr$Abundance)
tr$Abundance  <- ifelse(tr$unitA == "Million Pounds",tr$Abundance/2.205*1000, tr$Abundance)
tr$Abundance  <- ifelse(tr$unitA == "Pounds",tr$Abundance/2205, tr$Abundance)
tr$Abundance  <- ifelse(tr$unitA == "Thousand Pounds",tr$Abundance/2.205, tr$Abundance)
tr$unitA  <- "Metric Tons"

tr$Abundance <- ifelse(grepl("female", tr$unitA_des, ignore.case = TRUE), tr$Abundance * 2, tr$Abundance)
tr$unitA_des <- ifelse(grepl("female", tr$unitA_des, ignore.case = TRUE),"Mature biomass", tr$unitA_des)
tr$Abundance <- ifelse(grepl("females", tr$unitA_des, ignore.case = TRUE), tr$Abundance * 2, tr$Abundance)
tr$unitA_des <- ifelse(grepl("females", tr$unitA_des, ignore.case = TRUE),"Mature spawning stock biomass", tr$unitA_des)

# and walleye pollock in Eastern Bering Sea has a unit issue
tr$Catch <- ifelse(tr$stocklong == "Walleye pollock - Eastern Bering Sea",tr$Catch*1000,tr$Catch)

# most cases it is not total biomass but SSB; we use ram v4.44 to update for all stocks where data is available
fishid <- unique(tr$stocklong)

ram <- readRDS("Data/RAM/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds")
ramdata   <- ram$timeseries_values_views
ramunits  <- ram$timeseries_units_views

all_stocks <- c()
for(j in 1:length(fishid)){
  # get data
  spec <- subset(ramdata,ramdata$stocklong == gsub("- *", "", fishid[j]))
  
  if (nrow(spec)>0){
    specunits <- subset(ramunits,ramunits$stocklong == gsub("- *", "", fishid[j]))
    specdat <- data.frame(TB=spec$TBbest,TC=spec$TCbest,TL=spec$TL,SSB=spec$SSB,
                          TBunits = specunits$TB,TCunits = specunits$TC,
                          TLunits = specunits$TL,SSBunits = specunits$SSB,
                          Year=spec$year,
                          stocklong = fishid[j])
    all_stocks <-rbind(all_stocks,specdat)
  }}

all_stocks <- subset(all_stocks,!(is.na(all_stocks$TB)) & !(is.na(all_stocks$TC)))
all_stocks <- subset(all_stocks,all_stocks$Year > 1975)
all_stocks$diff <- all_stocks$SSB/all_stocks$TB
pr <- aggregate(all_stocks$diff,by=list(all_stocks$stocklong),FUN=mean,na.rm=T)

tr <- cbind(tr,pr[match(tr$stocklong,pr$Group.1), c(2)]) 
tr <- tr[!duplicated(tr$stocklong), ]
colnames(tr)[ncol(tr)] <- "conversion_ram"
tr <- tr[,-c(1,2,4,5,8)]
write.csv(tr, file="Data/NOAA_stocks/ssb_tb_conversion_draft.csv",row.names = F)
