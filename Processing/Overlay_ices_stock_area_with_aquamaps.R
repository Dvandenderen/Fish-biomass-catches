library(DBI)
library(RSQLite)
library(stringr)
library(sf)
library(dplyr)

# aqua maps data
conn <- dbConnect(RSQLite::SQLite(), "Data/Aquamaps/am.db") # Get aquamaps .db file
tables <- dbListTables(conn)
# print(tables)

# get probability of occurence data - all species
data <- dbReadTable(conn, "hcaf_species_native")
head(data)  # View first few rows

fish <- dbReadTable(conn, "speciesoccursum_r")

source("Processing/Get_stocks_ices.R")
stocks_uni <- stocks[!duplicated(stocks$FishStock),]
# get latin name from stock description
stocks_uni$latin   <- str_extract(stocks_uni$StockDescription, "\\(([^\\)]+)\\)")
stocks_uni$latin   <- str_replace_all(stocks_uni$latin, "[\\(\\)]", "")
stocks_uni$Genus   <- word(stocks_uni$latin, 1)
stocks_uni$Species <- word(stocks_uni$latin, 2)
stocks_uni <- stocks_uni %>% left_join(fish %>% select(Genus, Species, SpeciesID), by = c("Genus", "Species"))
stocks_uni <- stocks_uni[!duplicated(stocks_uni$SpeciesID),]
aquastocks <- c(stocks_uni$SpeciesID,"Fis-29267","Fis-29269") # get both sand-eel species included

# for baltic flounder and greater silver smelt, aqua maps has no information 
# and we use the ICES assessment area without correction

# now get all the grid cells with species probability of occurence
dist <- subset(data,data$SpeciesID %in% aquastocks)
dist <- subset(dist,dist$Probability >0.5) 
dist <- cbind(dist,stocks_uni[match(dist$SpeciesID,stocks_uni$SpeciesID), c("latin","Genus","Species")])
dist$latin <- ifelse(dist$SpeciesID %in% c("Fis-29269","Fis-29267"), "Ammodytes spp.",dist$latin)
dist$Genus <- ifelse(dist$SpeciesID %in% c("Fis-29269","Fis-29267"), "Ammodytes",dist$Genus)
dist$Species <- ifelse(dist$SpeciesID %in% c("Fis-29269","Fis-29267"), "spp.",dist$Species)
save(dist,file="Data/ICES_stocks/AquaMaps_occurence_ICES_stocks.RData")

dbDisconnect(conn)

# ------------------------------------------------------------------------------
# now load data and estimate for each ices stock the area where a
# species is present, based on aquamaps, which is then overlayed with icesareas
# result is for each 0.5 degree grid cell a presence (1) / absence (0) per stock

# load the stocks
source("Processing/Get_stocks_ices.R")

# load the aquamaps stock distribution
load("Data/ICES_stocks/AquaMaps_occurence_ICES_stocks.RData")

# load ices assessment areas
icesareas <- read_sf("Data/ICES_stocks/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")
icesareas <- st_make_valid(st_transform(icesareas, crs = 4326))

# ------------------------------------------------------------------------------
# now create half a degree grid for the ices area
bbox_3857 <- st_bbox(c(st_bbox(icesareas)),crs = st_crs(icesareas))  # get ices bounding box
bbox_4326 <- st_as_sfc(bbox_3857) %>% st_transform(crs = 4326) # Convert WGS 84 (EPSG:4326)

# create grid
grid_size <- 0.5   # Define exact grid size in degrees
grid <- st_make_grid(
  bbox_4326, 
  cellsize = c(grid_size, grid_size), 
  what = "polygons"
)
grid <- st_sf(id = 1:length(grid), geometry = grid)

# ------------------------------------------------------------------------------
# for each ices stock, get ices area polygon and cut out the area without
# species presence, based on aquamaps
stocks_uni <- stocks[!duplicated(stocks$FishStock),2:4]
stocks_uni$latin   <- str_extract(stocks_uni$StockDescription, "\\(([^\\)]+)\\)")
stocks_uni$latin   <- str_replace_all(stocks_uni$latin, "[\\(\\)]", "")

for (j in 1:nrow(stocks_uni)){
  subarea <- c(icesA = strsplit(stocks_uni$icesA[j], split = " ~ ")[[1]])
  subarea <- subset(icesareas,icesareas$Area_Full %in% subarea)
  subarea <- st_union(subarea)   

  if (!(stocks_uni$latin[j] %in% c("Platichthys spp", "Argentina silus"))) {
    # Now load species from AquaMaps
    am <- subset(dist, dist$latin == stocks_uni$latin[j])
    am <- st_as_sf(am, coords = c("CenterLong", "CenterLat"))  
    st_crs(am) <- st_crs(grid)
    
    # Intersect AquaMaps with grid
    dat <- st_intersects(am, grid) 
    dat <- as.data.frame(dat)
    specgrid <- grid[dat[,2],]  
  } else {
    specgrid <- grid
  }
  
  # and intersect the second grid with icesareas
  dat <- st_intersects(specgrid,subarea)
  dat <- as.data.frame(dat)
  specgrid <- specgrid[dat[,1],]  
  grid[,stocks_uni$FishStock[j]] <- 0
  grid[specgrid$id,stocks_uni$FishStock[j]] <- 1
}

