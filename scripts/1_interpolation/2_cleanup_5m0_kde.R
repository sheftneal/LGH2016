source("scripts/packages.R")
source("scripts/functions.R")
source("scripts/0_preprocess.R")
##################################################################################################################################

# write out point data

    #(1) define africa continent grid
    #(2) mask out (i) countries with no data (ii) water (iii) low population areas
    #(3) knn the cell centerpoints for the cells that have not been masked out

##################################################################################################################################

#(1) define grid

  grid = raster(xmn = bbox(africa)[1,1], xmx = bbox(africa)[1,2], ymn = bbox(africa)[2,1], ymx = bbox(africa)[2,2], res = 0.1)			
  grid[] <- 1:ncell(grid)
  all.cells <- coordinates(grid)

#(2) mask out
	
	#(i) subset cells that fall into countries with data
		
		country_overlap <- raster::extract(x = grid, y = africa, weights = T, cellnumbers = T, small = T)
		for(i in 1:length(country_overlap)){
			country_overlap[[i]]<-as.data.frame(country_overlap[[i]])
			country_overlap[[i]][,"country"]<- africa@data[i,"NAME"]
											}
		country_overlap <- data.frame(rbindlist(country_overlap)) 
		country_overlap <- subset(country_overlap, country %in% data.countries$NAME)
		country.cells <- sort(unique(country_overlap$cell))
		ao.ml.cells <- sort(unique(subset(country_overlap, country %in% c("Angola","Mali")))$cell)
		

	#(ii) subset to cells that are not over water
		
			water_overlap <- raster::extract(x = grid, y = water, weights = T, cellnumbers = T, small = T)
					for(i in 1:length(water_overlap)){
			water_overlap[[i]]<-as.data.frame(water_overlap[[i]])
														}
			water_overlap <- data.frame(rbindlist(water_overlap)) 
			water.cells <- sort(unique(water_overlap$cell))
			water.cells <- subset(water.cells, water.cells %in% country.cells)
		
	#(iii) bring in population
		
			grid.pop <- resample(crop(pop.raster, extent(africa)), grid)
			pop.values <- grid.pop[country.cells]
	    grid.locs <- xyFromCell(grid, country.cells)		
	    data <- data.frame(country.cells, grid.locs, pop.values); names(data)<- c("cell","x","y","pop")
	
	#subset  on pop and water
		data <- subset(data, cell %in% country.cells)
		data <- subset(data, cell %in% water.cells == F)
		nopop.cells <- subset(data, (pop <= 0.0002525055 & cell %in% ao.ml.cells) | (pop == 0) )$cell
		data <- subset(data, (pop > 0.0002525055 & cell %in% ao.ml.cells) | (pop > 0) ) #5th percentile of continent wide value for countries with data (taken from fig 1 code)
		
		data.loc <- data[,c("x","y")]
		water.loc <- data.frame(xyFromCell(grid, water.cells))
		nopop.loc <- data.frame(xyFromCell(grid, nopop.cells))
		
		data.loc$value <- "value"
		water.loc $value <- "water"
		nopop.loc $value <- "nopop"

		grid.loc <- rbind(data.loc, water.loc, nopop.loc)
		
		#check that masks look right
		plot(africa)
		points(grid.loc$x[grid.loc$value=='value'], grid.loc$y[grid.loc$value =='value'], pch=16, col = 'red',cex=0.1)
		points(grid.loc$x[grid.loc$value=='water'], grid.loc$y[grid.loc$value =='water'], pch=16, col = 'blue',cex=0.1)
		points(grid.loc$x[grid.loc$value=='nopop'], grid.loc$y[grid.loc$value =='nopop'], pch=16, col = 'green',cex=0.1)


	#now need to knn the points where value== 'value' to data
	#do once then identify nearest points since each decade on same grid
		kde_africa <- read_rds("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5mo_1980.rds")

  	grid.points <- subset(grid.loc, value=="value")[,c("x","y")]
	  data.points <- kde_africa[,c("x","y","value")]
	
	  test <- knn1(train = data.points[,c("x","y")], test = grid.points ,cl = 1:nrow(data.points))
	  grid.points$val1980 <- data.points[test,"value"]
	
	  kde_africa <- read_rds("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5mo_1990.rds")
  	grid.points$val1990 <- kde_africa[test,"value"]
  
  	kde_africa <- read_rds("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5mo_2000.rds")
  	grid.points$val2000 <- kde_africa[test,"value"]

	grid.points <- grid.points[,c("x","y","val1980","val1990","val2000")]
	grid.points$value = "mortality"
	
	water.loc[,c("val1980","val1990","val2000")]=NA
	water.loc<-water.loc[,c("x","y","value","val1980","val1990","val2000")]
	nopop.loc[,c("val1980","val1990","val2000")]=NA
	nopop.loc <-nopop.loc[,c("x","y","value","val1980","val1990","val2000")]
	
	data <- rbind(grid.points, water.loc, nopop.loc)
	names(data)[6] <- "datatype"
	
	
	data <- data %>% rename(est5m0_1980 = val1980)
	data <- data %>% rename(est5m0_1990 = val1990)
	data <- data %>% rename(est5m0_2000 = val2000)
	#write out the point data
	write_rds(data, file = "data/outputs/final_data/ChildMortEstimates5m0_gridded.rds", compress = "gz")
	
	
	##################################################################################################################################
	
	# write out gridded data
	
	#(1) define africa continent grid
	#(2) mask out (i) countries with no data (ii) water (iii) low population areas
	#(3) knn the cell centerpoints for the cells that have not been masked out
	
	##################################################################################################################################
	
	
	

grid <- raster(xmn = min(data$x), xmx = max(data$x), ymn = min(data$y), ymx = max(data$y), crs = crs(africa))
crs(grid)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
pts <- SpatialPoints(data[,c("x","y")])
cells <- cellFromXY(grid, pts)
data$cell <- cells
data <- subset(data, !is.na(cell) & datatype=="mortality")



# 1980
rbrick <- brick(x = raster(grid), nl = 3)
rbrick[[1]][data$cell] <- data$est5m0_1980
rbrick[[2]][data$cell] <- data$est5m0_1990
rbrick[[3]][data$cell] <- data$est5m0_2000

writeRaster(rbrick, filename = "data/outputs/final_data/ChildMortEstimates5m0_gridded.tif")

