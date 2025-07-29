
setwd("C:/Users/danie/Dropbox/Werk/Archief/2023 GEB Demersal fish and fisheries/Data analysis")
dat <- readRDS("Catch_all_2010_2014.rds")
load("Cells_in_EcoReg.RData")


BS <- subset(subcells,subcells$ECO_REG == "Baltic Sea")

dat <- subset(dat,dat$Cell %in% BS$Cell)
dat$catch <- dat$Reported + dat$IUU + dat$Discards

all <- aggregate(dat$catch,by=list(dat$IYear),FUN=sum)

BSsub <- subset(BS, BS$LatCentre >= 58 )
datsub <- subset(dat,dat$Cell %in% BSsub$Cell)
sub <- aggregate(datsub$catch,by=list(datsub$IYear),FUN=sum)

mean(sub$x/all$x)
