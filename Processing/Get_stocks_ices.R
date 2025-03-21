
# script that can be sourced to get all stocks with biomass and/or SSB information
# some missing unit information is corrected using ices advice sheets

# ------------------------------------------------------------------------------
# load ICES data stock Assessment Graphs
icdat <- read.csv("Data/ICES_stocks/StockAssessmentGraphs.csv")
colnames(icdat)[9] <- "ICES_Areas"

# update some stocks - missing unit information (checked with advice sheets)
icdat$CatchesLadingsUnits <- ifelse(icdat$FishStock %in% c("whb.27.1-91214", "her.27.1-24a514a","had.27.5b","cap.27.2a514","pil.27.8c9a","had.27.5a",
                                                           "spr.27.3a4", "caa.27.5a","lin.27.5a","usk.27.5a14","ple.27.5a","her.27.nirs","whg.27.47d",
                                                           "aru.27.5b6a","cod.27.5a","ple.27.21-23","cod.2127.1f14","pok.27.1-2","cod.27.1-2.coastN",
                                                           "cod.21.1","ple.27.420"), "tonnes",icdat$CatchesLadingsUnits)
icdat$StockSizeUnits      <- ifelse(icdat$FishStock %in% c("whb.27.1-91214","had.27.5b", "whg.27.47d"), "tonnes",icdat$StockSizeUnits )

# update some stocks - missing ICES.Areas
icdat$ICES_Areas <- ifelse(icdat$FishStock %in% c("hom.27.2a4a5b6a7a-ce-k8"), 
                           "27.4.a ~ 27.5.b ~ 27.6.a ~ 27.7.a ~ 27.7.b ~ 27.7.c.1 ~ 27.7.c.2 ~ 27.7.e ~ 27.7.f ~ 27.7.g ~ 27.7.h ~ 27.7.j.2 ~ 27.7.j.1 ~ 27.7.k.1 ~ 27.7.k.2",icdat$ICES_Areas)

# create dataframe
tr <- subset(icdat,icdat$CatchesLadingsUnits == "tonnes")
tr <- subset(tr,tr$StockSizeUnits == "tonnes")
tr <- subset(tr,tr$Year %in% c(1980:2021))
tr <- subset(tr, tr$StockSizeDescription %in% c("SSB","Combined-sex SSB","Spawning Stock Biomass","Biomass 1+"))
tr <- subset(tr, tr$StockSize > 0 | tr$TBiomass > 0)
tr <- data.frame(year= tr$Year, FishStock = tr$FishStock, StockDescription = tr$StockDescription,
                 icesA = tr$ICES_Areas,
                 catch = tr$Catches, landings = tr$Landings, Tbio_low = tr$Low_TBiomass,
                 Tbio = tr$TBiomass, Tbio_high = tr$High_TBiomass, 
                 SSB_low = tr$Low_StockSize,SSB = tr$StockSize, SSB_high = tr$High_StockSize)
stocks <- subset(tr, !(tr$icesA == ""))
stocks <- subset(stocks,!(stocks$FishStock == "pra.27.3a4a"))

rm(icdat);rm(tr)
