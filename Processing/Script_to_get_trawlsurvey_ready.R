
# ------------------------------------------------------------------------------
# prepare the script to get an estimate of biomass per haul and species
# where each haul is linked to an LME

# get trawl survey data
trawl <- read.csv("Data/Trawl_survey/230531_vanDenderen_etal_trawl_fish_biomass.csv")

# --------------------------------------------------------------------------------
# get all survey estimates per species and year linked to the grid
# remove hauls that were removed in paper
# --------------------------------------------------------------------------------

# now get all stations and years with data
trawl_sep <- trawl %>% 
  group_by(Haul_id,Survey_Region,Gear,Year,Month,Longitude,Latitude,Bottom_depth) %>%
  summarize_at(.vars=c('kg_km2_corrected'), .funs = function(x) sum(x)) %>% 
  dplyr::select(Haul_id,Survey_Region,Gear,Year,Month,Longitude,Latitude,Bottom_depth,kg_km2_corrected) %>%
  as.data.frame()

# remove NA's
trawl_sep <- subset(trawl_sep,!(is.na(trawl_sep$kg_km2_corrected)))

# remove Aleutian island boundary
trawl_sep <- subset(trawl_sep, trawl_sep$Longitude > -180)

# some data seem too high
# e.g. plot(trawl$biomass[trawl$region == "NS-IBTS"])
# decided to remove all outside 1.5 x the interquantile range (log10(biomass))
# for each 'survey region' and year

# data can be selected based on 1.5 x the interquantile range
trawl_sep$biomass <- log10(trawl_sep$kg_km2_corrected)
IQR           <- trawl_sep %>% 
                    group_by(Survey_Region,Year) %>%  
                    summarise(quantile(biomass,0.25),quantile(biomass,0.75))
IQR           <- as.data.frame(IQR)
IQR$IQR       <- IQR[,4]-IQR[,3]   
IQR$high      <- IQR[,4] + IQR$IQR*1.5
IQR$low       <- IQR[,3] - IQR$IQR*1.5
IQR$uni       <- paste(IQR$Survey_Region,IQR$Year)
trawl_sep$regy    <- paste(trawl_sep$Survey_Region,trawl_sep$Year)
trawl_sep         <- cbind(trawl_sep, IQR[match(trawl_sep$regy,IQR$uni), c("high","low")])
trawl_sep$high    <- trawl_sep$high - trawl_sep$biomass
trawl_sep$low     <- trawl_sep$biomass - trawl_sep$low
trawl_sep         <- subset(trawl_sep,trawl_sep$high >= 0 & trawl_sep$low >= 0)

# select all that are within trawl_sep
trawl <- subset(trawl, trawl$Haul_id %in% trawl_sep$Haul_id)

# ------------------------------------------------------------------------------
# now get overlap between survey and LME66 shapefile
lme66 <- read_sf("Data/LME66_polygons/lme66.shp")
lme66 <- st_make_valid(lme66)

trawl$uniq <- paste(trawl$Longitude, trawl$Latitude, sep = "_")
coords_uni <- unique(trawl$uniq)  
t <- strsplit(coords_uni, "_") # Split the coordinates
coords <- matrix(unlist(t), ncol = 2, byrow = TRUE)

# Convert the coordinates to numeric
coords <- as.data.frame(coords)
coords[, 1] <- as.numeric(as.character(coords[, 1]))
coords[, 2] <- as.numeric(as.character(coords[, 2]))
coords[, 3] <- coords_uni

# Convert the coordinates to sf object
coords_sf <- st_as_sf(coords, coords = c(1, 2), crs = st_crs(lme66))

# Perform a spatial join with the grid (assuming grid_master is an sf object)
dat <- st_intersects(coords_sf, lme66)
dat <- as.data.frame(dat)
dat$uniq <- coords_sf$V3[dat$row.id]
dat$LME <- lme66$LME_NAME[dat$col.id]

# Merge the 'uni_cell' into the original trawl data
trawl <- cbind(trawl, dat[match(trawl$uniq,dat$uniq), c("LME")])
colnames(trawl)[ncol(trawl)] <- "LME"

save(trawl,file="Data/Trawl_survey/Surveys_hauls_species_LMEs.RData")

#--------------------------------------------------------------------------------
# now get overlap between survey and LME66 shapefile
meow <- read_sf("Data/MEOW_shapefiles/meow_ecos.shp")

trawl$uniq <- paste(trawl$Longitude, trawl$Latitude, sep = "_")
coords_uni <- unique(trawl$uniq)  
t <- strsplit(coords_uni, "_") # Split the coordinates
coords <- matrix(unlist(t), ncol = 2, byrow = TRUE)

# Convert the coordinates to numeric
coords <- as.data.frame(coords)
coords[, 1] <- as.numeric(as.character(coords[, 1]))
coords[, 2] <- as.numeric(as.character(coords[, 2]))
coords[, 3] <- coords_uni

# Convert the coordinates to sf object
coords_sf <- st_as_sf(coords, coords = c(1, 2), crs = st_crs(meow))

# Perform a spatial join with the grid (assuming grid_master is an sf object)
dat <- st_intersects(coords_sf, meow)
dat <- as.data.frame(dat)
dat$uniq <- coords_sf$V3[dat$row.id]
dat$ME <- meow$ECOREGION[dat$col.id]

# Merge the 'uni_cell' into the original trawl data
trawl <- cbind(trawl, dat[match(trawl$uniq,dat$uniq), c("ME")])
colnames(trawl)[ncol(trawl)] <- "ME"

save(trawl,file="Data/Trawl_survey/Surveys_hauls_species_LMEs_MEs.RData")

