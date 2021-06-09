source("C:/R_Projects/R Speedup.R") ## this is Luke-specific, this is just where I store my R Libraries
library(tmap)
library(tmaptools)
library(sp)
library(raster)



setwd("Z:/ES/WILDLIFE 2016-2020/Trapline management/Data Code/BC Furbearer Harvest Mapping")
wmus.all <- readRDS("ProvincialWMUs.rds")
water <- readRDS("MajorWaters_2Mil.rds")
Regions <- readRDS("RegionsPoly.rds")

setwd("Z:/ES/WILDLIFE 2016-2020/Harvest management/Harvest data/Compulsory Inspection")
data <- read.csv("2016-19 R4 Wolf CI Summary.csv")
greens <- colorRampPalette(c("white", "dark green"))(100)


units <- 401:440
wmus <- wmus.all[wmus.all$MU %in% units,]

wmus$stat <- vector(length = nrow(wmus))
for(i in 1:nrow(wmus)){
  wmus$stat[i] <- data$Hunters[as.character(data$MU) == wmus$MU[i]]/3
}
wmus$stat <- (wmus$stat / (wmus$FEATURE_AR/1e+6)) * 1000

mainlab <- "Annual Wolf Harvest (Wolves/1000Km2)"

mu_map <- tm_shape(wmus) +
  tm_fill(col = "stat", style = "cont", palette = greens, legend.show = T, title = "") +
  tm_borders()

mu_map <- mu_map +
  tm_shape(water) +
  tm_fill(col = "lightskyblue") +
  tm_shape(Regions) +
  tm_borders(lwd = 1.5)

if(nrow(wmus) < nrow(wmus.all)){
  mu_map <- mu_map + tm_shape(wmus.all[!wmus.all$MU %in% units,]) +
    tm_fill(col = "white") +
    tm_borders(col = "white", lwd = 1.5)}
mu_map <- mu_map + 
  tm_shape(wmus) +
  tm_borders(lwd = 1) +
  tm_layout(main.title = mainlab, main.title.size = 1.2, frame = F) +
  tm_legend(scale = 1.8)
mu_map
