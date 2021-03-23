library(raster)

d1 <- raster("data/gridded/ChildMortEstimates_1980s.tif")
d2 <- raster("data/gridded/ChildMortEstimates_1990s.tif")
d3 <- raster("data/gridded/ChildMortEstimates_2000s.tif")

d <- brick(d1, d2, d3)

names(d) <- paste("imr_",c(1980,1990,2000),"s",sep="")

writeRaster(d, filename = "data/ChildMortEstimates_gridded.tif")

rm(list = ls())
d <- brick("data/ChildMortEstimates_gridded.tif")


d <- read_csv("data/rm/ChildMortalityEstimates_Burke_Heft-Neal_Bendavid.csv")
write_rds(d, file = "data/ChildMortEstimates_points.rds", compress = "gz")


load("data/ChildMortality_1m0/Africa_KDE_1yr_decade=1980_Noptim_cellsize01.RData")
d1 <- kde_africa
load("data/ChildMortality_1m0/Africa_KDE_1yr_decade=1990_Noptim_cellsize01.RData")
d2 <- kde_africa
load("data/ChildMortality_1m0/Africa_KDE_1yr_decade=2000_Noptim_cellsize01.RData")
d3 <- kde_africa

ex <- read_rds("data/ChildMortality_5m0/ChildMortEstimates5m0_points.rds")



d1 <- d1 %>% rename(est1m0_1980s = value) %>% dplyr::select(-decade, -N)
d2 <- d2 %>% rename(est1m0_1990s = value) %>% dplyr::select(-decade, -N)
d3 <- d3 %>% rename(est1m0_2000s = value) %>% dplyr::select(-decade, -N)

d <- left_join(d1, d2) %>% left_join(d3)
d <- d %>% dplyr::select(lon = x, lat = y, est1m0_1980s, est1m0_1990s, est1m0_2000s)
