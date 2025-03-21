
# load ICES data stock Assessment Graphs
icdat <- read.csv("C:/Users/danie/Dropbox/Werk/MC paper 3/Data/ICES data stocks/StockAssessmentGraphs.csv")
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
tr <- subset(tr, !(tr$icesA == ""))

# load ices areas polygon and obtain size for each ices area per LME
icesareas <- read.csv(file = "ICES_area_overlap_LME.csv")

# two areas need to be combined to get a match
# 27.3.a == 27.3.a.21 and 27.3.a.20
comb3a    <- subset(icesareas,icesareas$Area_Full %in% c("27.3.a.21", "27.3.a.20"))
comb3a[3,] <-comb3a[1,]
comb3a$SubDivisio[3] <- NA
comb3a$Area_Full[3] <- "27.3.a"
comb3a$Area_27[3] <- "3.a"
comb3a$Area_km2[3] <- comb3a$Area_km2[1] + comb3a$Area_km2[2]
comb3a$subarea[3] <- comb3a$subarea[1] + comb3a$subarea[2]
icesareas    <- subset(icesareas,!(icesareas$Area_Full %in% c("27.3.a.21", "27.3.a.20")))
icesareas <- rbind(icesareas,comb3a)

# 27.5.b == combination of six
comb5b    <- subset(icesareas,icesareas$Major_FA ==27 & icesareas$SubArea == 5 & icesareas$Division == "b") 
comb5b[7:10,] <- comb5b[c(1,2,4,5),]
comb5b$SubDivisio[7:10] <- NA
comb5b$Area_Full[7:10] <- "27.5.b"
comb5b$Area_27[7:10] <- "5.b"
comb5b$Area_km2[7:10] <- sum(unique(comb5b$Area_km2[1:6]))
comb5b$subarea[8] <- 82300.8393 + 20742.3308
comb5b$frac[7:10] <- comb5b$subarea[7:10] / comb5b$Area_km2[7:10]
icesareas    <- subset(icesareas,!(icesareas$Area_Full %in% comb5b$Area_Full))
icesareas <- rbind(icesareas,comb5b)

# get unique rows (ignoring LMEs)
ic_uni <- icesareas[,1:10]
ic_uni <- ic_uni[!duplicated(ic_uni), ]

# split catches/landings/biomass per ices area in proportion to size of area for each stock by row
tr_new <- c()
nam <- c("catch","landings","Tbio_low","Tbio","Tbio_high","SSB_low","SSB","SSB_high")
for(j in 1:nrow(tr)){
  subarea <- data.frame(icesA = strsplit(tr$icesA[j], split = " ~ ")[[1]])
  subarea <- cbind(subarea,ic_uni[match(subarea$icesA,ic_uni$Area_Full), c("Area_km2")])
  colnames(subarea)[ncol(subarea)] <- "size"
  subarea$size <- subarea$size/ sum(subarea$size)
  specyear <- tr[rep(j, each = nrow(subarea)), ] 
  specyear$icesA <- subarea$icesA
  specyear[,nam] <- specyear[,nam] * subarea$size
  tr_new <- rbind(tr_new,specyear)  
}
colnames(tr_new)[4] <- "Area_Full"

tr_new$Species <- substr(tr_new$FishStock, start = 1, stop = 3)
tr_new$Ftype <- "dem"
tr_new$Ftype <- ifelse(tr_new$Species %in% c("her","pil","cap","san","mac","hom","spr",
                                             "ane", "aru"),"pel",tr_new$Ftype)

# set SSB low and high same as SSB for all without SSB (if Na and SSB is provided)
tr_new$SSB_low <- ifelse(is.na(tr_new$SSB_low) & tr_new$SSB > 0,tr_new$SSB,tr_new$SSB_low)
tr_new$SSB_high <- ifelse(is.na(tr_new$SSB_high) & tr_new$SSB > 0,tr_new$SSB,tr_new$SSB_high)

# set TBio low and high in proportion to SSB low and high (if NA and Tbio is provided)
tr_new$Tbio_low <- ifelse(is.na(tr_new$Tbio_low) & tr_new$Tbio > 0, tr_new$SSB_low /tr_new$SSB * tr_new$Tbio,tr_new$Tbio_low)
tr_new$Tbio_high <- ifelse(is.na(tr_new$Tbio_high) & tr_new$Tbio > 0, tr_new$SSB_high /tr_new$SSB * tr_new$Tbio,tr_new$Tbio_high)

save(tr_new,icesareas,file="ICES_outputs_stocks.Rdata")

