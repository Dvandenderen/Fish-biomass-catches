
# load libraries
library(sf)
library(gridExtra)
library(marmap)
library(ggplot2)
library(cowplot)

# load polygon with countries and LMEs
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
land  <- st_union(st_make_valid(ctrys))
shape_LME <- st_read("Data/LME66_polygons/lme66.shp")
shape_ME <- st_read("Data/MEOW_shapefiles/meow_ecos.shp")

load("Models_sdmtmb_Pred/Prediction_North Sea.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Year %in% c(1995:2010))
reg <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
reg$Region <- "North Sea"

load("Models_sdmtmb_Pred/Prediction_Baltic Sea.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Baltic Sea"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Celtic-Biscay Shelf.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Celtic-Biscay Shelf"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Adriatic Sea.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$yearf %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Adriatic Sea"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_East Bering Sea.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "East Bering Sea"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Gulf of Alaska.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Gulf of Alaska"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Gulf of Maine_Bay of Fundy.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Gulf of Maine_Bay of Fundy"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Northern Norway and Finnmark.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Northern Norway and Finnmark"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Scotian Shelf.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Scotian Shelf"
reg <- rbind(reg,dat)

load("Models_sdmtmb_Pred/Prediction_Virginian.RData")
predictions$data$est <- exp(predictions$data$est)
dat <- subset(predictions$data,predictions$data$Yearf  %in% c(1995:2010))
dat <- aggregate(est~Longitude+ Latitude,data =dat, FUN=mean)
dat$Region <- "Virginian"
reg <- rbind(reg,dat)

# get to tonnes per km2
reg$est <- reg$est/1000

# get coordinates
minlong <- min(reg$Longitude)
maxlong <-  max(reg$Longitude)
minlat  <-  min(reg$Latitude) 
maxlat  <-  max(reg$Latitude)
coordslim <- c(minlong,maxlong,minlat,maxlat)

# create map 
LME <- subset(shape_LME,shape_LME$LME_NAME %in% c("Baltic Sea","North Sea","Celtic-Biscay Shelf","East Bering Sea"))

EBS <- LME[4,]
EBS <- st_coordinates(EBS)
EBS <- subset(EBS,EBS[,1] > -179.5 & EBS[,1] < 20)
# Suppose coords_mat is your matrix with columns: X, Y, L1, L2
coords_df <- as.data.frame(EBS)
names(coords_df) <- c("X", "Y", "L1", "L2","L3")
# Build polygons per L1 (polygon) and L2 (ring)
EBS <- coords_df %>%
  group_by(L1, L2,L3) %>%
  summarise(mat = list(as.matrix(cbind(X, Y))), .groups = "drop") %>%
  group_by(L1) %>%
  summarise(
    geometry = st_sfc(list(st_polygon(mat))), 
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326)  # set CRS as needed
LME <- subset(LME,LME$LME_NAME %in% c("Baltic Sea","North Sea","Celtic-Biscay Shelf"))

ME <- subset(shape_ME,shape_ME$ECOREGION %in% c("Adriatic Sea","Gulf of Alaska","Gulf of Maine/Bay of Fundy","Virginian",
                                      "Northern Norway and Finnmark","Scotian Shelf"))
reg$est[reg$est < 0.5] <- 0.5
reg$est[reg$est > 200] <- 200

reg1 <- subset(reg,reg$Region %in% c("Baltic Sea","North Sea","Celtic-Biscay Shelf","Adriatic Sea","Northern Norway and Finnmark"))
minlong <- min(reg1$Longitude)
maxlong <-  max(reg1$Longitude)
minlat  <-  min(reg1$Latitude) 
maxlat  <-  max(reg1$Latitude)
coordslim <- c(minlong,maxlong,minlat,maxlat)

NEA <- ggplot() +
  #geom_point(data=reg1,aes(x=Longitude,y=Latitude,col=est),cex=0.35,pch=15)+
  geom_sf(data=reg_sf,aes(color = est), size = 1) +
  geom_sf(data=LME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ctrys,col="black",fill= "black") +
  coord_sf(xlim = c(minlong-1,maxlong+1),ylim=c(minlat-1,maxlat+1))+
  scale_color_viridis_c(
    trans = "log10",      # log10 color scale
    limits = c(0.5, 200), # fix min and max of the scale
    option = "viridis") +   theme_classic() +theme(legend.position = "bottom") +
  ggtitle("A)")

NEA_grob <- ggplotGrob(NEA)
legend_index <- which(sapply(NEA_grob$grobs, function(x) x$name) == "guide-box")
legend <- NEA_grob$grobs[[legend_index]]
NEA <-NEA + theme(legend.position = "none")

reg1 <- subset(reg,reg$Region %in% c("Gulf of Alaska","East Bering Sea"))
minlong <- min(reg1$Longitude)
maxlong <-  max(reg1$Longitude)
minlat  <-  min(reg1$Latitude) 
maxlat  <-  max(reg1$Latitude)
coordslim <- c(minlong,maxlong,minlat,maxlat)

# Subset LME to only those that intersect your plotting area
LME <- st_make_valid(LME)

pac <- ggplot() +
  geom_sf(data=reg_sf,aes(color = est), size = 1) +
  geom_sf(data=LME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=EBS,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ctrys,col="black",fill= "black") +
  coord_sf(xlim = c(minlong+1.2,maxlong+1),ylim=c(minlat-1,maxlat+1))+
  theme(legend.position = "none")  +  
  scale_color_viridis_c(
    trans = "log10",      # log10 color scale
    limits = c(0.5, 200), # fix min and max of the scale
    option = "viridis") +   theme_classic() + theme(legend.position = "none")+
 ggtitle("B)")

reg1 <- subset(reg,reg$Region %in% c("Gulf of Maine/Bay of Fundy","Virginian","Scotian Shelf"))
minlong <- min(reg1$Longitude)
maxlong <-  max(reg1$Longitude)
minlat  <-  min(reg1$Latitude) 
maxlat  <-  max(reg1$Latitude)
coordslim <- c(minlong,maxlong,minlat,maxlat)

NEW <- ggplot() +
  geom_sf(data=reg_sf,aes(color = est), size = 1) +
  geom_sf(data=LME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ME,col="black",fill=NA, lwd=0.4)+
  geom_sf(data=ctrys,col="black",fill= "black") +
  coord_sf(xlim = c(minlong-1,maxlong+1),ylim=c(minlat-1,maxlat+1))+
  theme(legend.position = "none")  +  
  scale_color_viridis_c(
    trans = "log10",      # log10 color scale
    limits = c(0.5, 200), # fix min and max of the scale
    option = "viridis") +   theme_classic() + theme(legend.position = "none")+
   ggtitle("C)")

# Arrange plots
grid.arrange(
  NEA, pac, NEW,legend,
  layout_matrix = rbind(c(1, 2),c(1, 3),c(4,4)),
  heights = c(2, 2,0.2),
  widths  = c(2.2,1.8)
  )



