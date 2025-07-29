
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

# good stocks
stocks <- data.frame(stocklong = unique(tr$stocklong))
stocks <- stocks %>%
  mutate(temp = stocklong) %>%
  separate(temp, into = c("Species", "Area"), sep = " - ")

# --------------------------------------------------------------------------------
### load regions from Ram legacy, match with names from NOAA and get polygons from James Rising
assarea <- read.csv('Data/Assessment_James_Rising_polygons/RLSADBv4.44_assessment_areas.csv',header=T)

# update some before matching
stocks$Area <- ifelse(stocks$Area == "Bering Sea / Aleutian Islands", "Bering Sea and Aleutian Islands",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Southern Atlantic Coast / Gulf of Mexico", "Southern Atlantic coast and Gulf of Mexico",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Southern New England / Mid-Atlantic", "Southern New England /Mid Atlantic",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Western / Central Gulf of Alaska", "Central Western Gulf of Alaska",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Western / Central / West Yakutat Gulf of Alaska", "Central Western Gulf of Alaska",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Oregon", "Oregon Coast",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Pacific", "Pacific Ocean",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Eastern Gulf of Alaska", "Gulf of Alaska",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Florida Keys / East Florida", "Southeast Florida",stocks$Area)
stocks$Area <- ifelse(stocks$Area == "Northern Subpopulation", "Northern Pacific Coast",stocks$Area)
stocks <- cbind(stocks,assarea[match(stocks$Area,assarea$areaname), c("areacode","areaid")]) 
stocks <- subset(stocks,!(is.na(stocks$areaid))) # mainly southern atlantic coast (and survey data is shallow for comparison)

# not all are correct (names of locations have multiple outcomes, fix to US)
stocks$areaid <- ifelse(stocks$areaid == "Canada-DFO-5Z", "USA-NMFS-5Z",stocks$areaid)
stocks$areaid <- ifelse(stocks$areaid == "USA-NMFS-CWGA", "USA-NMFS-GA",stocks$areaid) # CWGA does not have a polygon
stocks$areaid <- ifelse(stocks$areaid == "Japan-FAJ-PAC", "multinational-ISC-PAC",stocks$areaid)

# now load James Rising areas and polygons
shape <- read_sf(dsn = "Data/Assessment_James_Rising_polygons/shapes" ,layer="ram") #warnings are OK
st_crs(shape) <- 4326

### load regions (IDs in shapefile match rownumber in csv file)
regions<-read.csv('Data/Assessment_James_Rising_polygons/latlon.csv',header=T)
regions$SP_ID<-c(1:232)
stocks <- cbind(stocks,regions[match(stocks$areaid,regions$name), c("SP_ID")]) 
colnames(stocks)[ncol(stocks)] <- "SP_ID" # allmost all match after the subsetting
stocks$SP_ID[stocks$areacode == "BOGO"] <- 173


# --------------------------------------------------------------------------------
# load NOAA assessment areas
NOAAareas <- subset(shape, shape$SP_ID %in% stocks$SP_ID)
NOAAareas <- st_make_valid(NOAAareas)
st_crs(NOAAareas) <- 4326

# ------------------------------------------------------------------------------
# now create half a degree grid for the NOAA area
bbox_4326 <- st_bbox(NOAAareas)

# create grid
grid_size <- 0.5   # Define exact grid size in degrees
grid <- st_make_grid(
  bbox_4326, 
  cellsize = c(grid_size, grid_size), 
  what = "polygons"
)
grid <- st_sf(id = 1:length(grid), geometry = grid)

# ------------------------------------------------------------------------------
# for each NOAA stock, get NOAA area polygon and cut out the area without
# species presence, based on aquamaps
#nam <- rfishbase::common_to_sci(stocks$Species)
#nam <- subset(nam,nam$ComName %in% stocks$Species)
#write.csv(nam,file="Data/NOAA_stocks/common_to_scientific_name.csv")
stocks_uni <- read.csv("Data/NOAA_stocks/common_to_scientific_name_cleaned.csv")
colnames(stocks_uni)[1] <- "latin"

# read aqua maps data
conn <- dbConnect(RSQLite::SQLite(), "Data/Aquamaps/am.db") # Get aquamaps .db file
tables <- dbListTables(conn)
# print(tables)

# get probability of occurence data - all species
data <- dbReadTable(conn, "hcaf_species_native")
head(data)  # View first few rows
fish <- dbReadTable(conn, "speciesoccursum_r")

# create match between NOAA stocks and aquamaps 
stocks_uni$Genus   <- word(stocks_uni$latin, 1)
stocks_uni$Species <- word(stocks_uni$latin, 2)
stocks_uni <- stocks_uni %>% left_join(fish %>% select(Genus, Species, SpeciesID), by = c("Genus", "Species"))
aquastocks <- c(stocks_uni$SpeciesID)

# for several species aqua maps has no information 
# and we use the assessment area without correction (see below)

# now get all the grid cells with species probability of occurence
dist <- subset(data,data$SpeciesID %in% aquastocks)
dist <- subset(dist,dist$Probability >0.5) 
dist <- cbind(dist,stocks_uni[match(dist$SpeciesID,stocks_uni$SpeciesID), c("latin","Genus","Species")])
save(dist,file="Data/NOAA_stocks/AquaMaps_occurence_NOAA_stocks.RData")

dbDisconnect(conn)
rm(list = setdiff(ls(), c("grid", "shape","stocks_uni","stocks", "dist","tr")))

# combine everything together
colnames(stocks)[2] <- "common_name"
stocks <- cbind(stocks,stocks_uni[match(stocks$common_name,stocks_uni$ComName), c("latin","Genus","Species")])
stocks <- subset(stocks, !(is.na(stocks$Genus)))
stocks <- subset(stocks,!(is.na(stocks$SP_ID)))

for (j in 1:nrow(stocks)){
  subarea <- subset(shape,shape$SP_ID %in% stocks$SP_ID[j])
  
  if (!(stocks$latin[j] %in% stocks_uni$latin[is.na(stocks_uni$SpeciesID)])) {
    # Now load species from AquaMaps
    am <- subset(dist, dist$latin == stocks$latin[j])
    am <- st_as_sf(am, coords = c("CenterLong", "CenterLat"))  
    st_crs(am) <- st_crs(grid)
    
    # Intersect AquaMaps with grid
    dat <- st_intersects(am, grid) 
    dat <- as.data.frame(dat)
    specgrid <- grid[dat[,2],]  
  } else {
    specgrid <- grid
  }
  
  # and intersect the second grid with JRising polygon area
  dat <- st_intersects(specgrid,subarea)
  dat <- as.data.frame(dat)
  specgrid <- specgrid[dat[,1],] 
  
  if (nrow(specgrid) == 0) {
    specgrid <- grid # some species do not occur in aquamaps in the area with assessment :)
    dat <- st_intersects(specgrid,subarea)
    dat <- as.data.frame(dat)
    specgrid <- specgrid[dat[,1],] 
  }
  
  grid[,stocks$stocklong[j]] <- 0
  grid[specgrid$id,stocks$stocklong[j]] <- 1
}

grid_drop <- st_drop_geometry(grid)
grid$tot <- rowSums(grid_drop[,2:length(names(grid_drop))])
grid <- subset(grid, grid$tot > 0)
grid <- grid[, !names(grid) %in% "tot"]
save(grid,file="Data/NOAA_stocks/AquaMaps_occurence_NOAA_stocks.Rdata")

rm(list = setdiff(ls(), c("grid", "shape","stocks_uni","stocks", "dist","tr")))

# ------------------------------------------------------------------------------
# get area of grid cell on land
land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
land <- st_make_valid(land)
tt2 <- st_intersection(grid, land) 
land_over <- data.frame(uni = tt2$id, land_sqkm = st_area(st_make_valid(tt2))) 
land_over$land_sqkm <- land_over$land_sqkm / 10^6 # m2 to km2
land_over$land_sqkm <- unclass(land_over$land_sqkm)

grid <- cbind(grid,land_over[match(grid$id,land_over$uni), c(2)])
colnames(grid)[ncol(grid)-1] <- "land_sqkm"
grid$land_sqkm <- ifelse(is.na(grid$land_sqkm), 0,grid$land_sqkm)
grid$area_sqkm <- st_area(grid)/10^6
grid$area_sqkm <- unclass(grid$area_sqkm)
grid$ocean_sqkm <- grid$area_sqkm - grid$land_sqkm # get ocean

# Link to LME for each mid-point  
lme66 <- st_read("Data/LME66_polygons/lme66.shp")
grid_mid <- st_centroid(grid)
dat <- st_intersects(grid_mid, st_make_valid(lme66)) 
dat <- as.data.frame(dat)
grid$LME <- NA
grid$LME[dat$row.id] <- lme66$LME_NAME[dat[,2]]  

# Link to ME for each mid-point  
meow <- st_read("Data/MEOW_shapefiles/meow_ecos.shp")
grid_mid <- st_centroid(grid)
dat <- st_intersects(grid_mid, st_make_valid(meow)) 
dat <- as.data.frame(dat)
grid$ME <- NA
grid$ME[dat$row.id] <- meow$ECOREGION[dat[,2]]  

# now get the depth
bbox <- st_bbox(grid)
bathy <- getNOAA.bathy(lon1 = bbox["xmin"], lon2 = bbox["xmax"],
                       lat1 = bbox["ymin"], lat2 = bbox["ymax"])
grid$depth <- NA
for(j in 1:nrow(grid)){
  grid_sf <- st_as_sf(grid$geometry[j])
  bbox <- st_bbox(grid_sf)
  lon <- which(as.numeric(rownames(bathy)) >= bbox["xmin"] & as.numeric(rownames(bathy)) <= bbox["xmax"])
  lat <- which(as.numeric(colnames(bathy)) >= bbox["ymin"] & as.numeric(colnames(bathy)) <= bbox["ymax"])
  dr <- merge(lon,lat)
  grid$depth[j] <- mean(bathy[dr[,1],dr[,2]])
}

save(grid,file="Data/NOAA_stocks/AquaMaps_occurence_NOAA_stocks.Rdata")

# ----------------------------------------------------------------

# stocklong names got a bit messy
clean_name <- function(z) {
  z |>
    tolower() |>
    gsub("[^a-z0-9]+", " ", x = _) |>  # anything not letter/number -> space
    trimws() |>
    gsub("\\s+", " ", x = _)           # collapse multiple spaces
}

colnames_clean <- sapply(colnames(grid), clean_name)

# create database outside script - dataframe for each grid cell,year and stock
totdat <- data.frame(id=NA, Catch = NA, Landings = NA,Tbio_low = NA, Tbio = NA, 
                     Tbio_high = NA, SSB_low = NA, SSB = NA, SSB_high = NA,
                     Stock = NA, Year = NA)

# get more recent years
tr <- subset(tr,tr$Year > 1970)

# some stocks don't match with their assessment area and/or aquamaps 
# (for now not included, can be checked if big stocks are missing)
rm_names <- st_drop_geometry(grid)
rm_names <- colSums(rm_names[,2:(length(names(rm_names))-6)])
rm_names <- subset(rm_names,rm_names == 0)
grid <- grid[, !names(grid) %in% names(rm_names)]
tr$match <- match(clean_name(tr$stocklong), colnames_clean)
tr <- subset(tr,!(is.na(tr$match)))

# get the species that are left
colnames_clean <- sapply(colnames(grid), clean_name)

for (p in 1:nrow(tr)){
  grid_year <- st_drop_geometry(grid)
  nam <- paste(tr$stocklong[p])  
  idx <- match(clean_name(nam), colnames_clean)
  
  if (is.na(idx)) next
  grid_spec <- grid_year[grid_year[,idx] == 1,]
  oc_area <- grid_spec$ocean_sqkm/sum(grid_spec$ocean_sqkm)
  stock_year <- data.frame(
    id=grid_spec$id, 
    Catch = tr$Catch[p] * oc_area,
    Landings = NA,
    Tbio_low = NA,
    Tbio = tr$Abundance[p] * oc_area,
    Tbio_high = NA,
    SSB_low = NA,
    SSB = NA,
    SSB_high = NA,
    Stock = rep(nam,length(oc_area)),
    Year = rep(tr$Year[p],length(oc_area))
    )
  
  totdat <- rbind(totdat,stock_year)
}

save(totdat,file="Data/NOAA_stocks/Assessment_spatially_allocated.Rdata")
  