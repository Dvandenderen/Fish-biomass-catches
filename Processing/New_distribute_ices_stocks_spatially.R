# ------------------------------------------------------------------------------
# obtain all ICES fish stocks with total biomass and/or SSB biomass 
# ------------------------------------------------------------------------------
library(sf)
library(dplyr)

# load the stocks
source("Processing/Get_stocks_ices.R")

# load the overlay grid


# and estimate biomass by grid cell by year either combined and/or for each of the species
# to do below

  
  subarea <- cbind(subarea,ic_uni[match(subarea$icesA,ic_uni$Area_Full), c("Area_km2")])
  colnames(subarea)[ncol(subarea)] <- "size"
  subarea$size <- subarea$size/ sum(subarea$size)
  specyear <- tr[rep(j, each = nrow(subarea)), ] 
  specyear$icesA <- subarea$icesA
  specyear[,nam] <- specyear[,nam] * subarea$size
  tr_new <- rbind(tr_new,specyear)  
}
colnames(tr_new)[4] <- "Area_Full"


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

