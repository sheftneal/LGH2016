source("scripts/packages.R")
source("scripts/functions.R")
source("scripts/0_preprocess.R")		#loads and preps data for analysis

#Interpolation Script
##########################################################################################################################################################			

#No maximum radius			
R = NULL


for (decade in c(1980, 1990, 2000)){		
	
	kde_africa = list();length(kde_africa) = length(data.countries$ISO2); names(kde_africa) = data.countries$ISO2
	
	for (country in data.countries$ISO2){ 
		
	#subset country and map
	country.data = imr[(imr$ISO2==country),]
	country.shape = africa[(africa$ISO2==country),]
	country.name = as.character(data.countries[(data.countries$ISO2==country),"NAME"])

	#subset decade
	data.decade = country.data[country.data$decade==decade,]
			
	#There are a few weird clusters that have incorrect coordinates so we drop clusters where lat lon fall outside of the max coordinates of the shape shape +- 2 deg
		data.decade = data.decade[data.decade$longnum > bbox(country.shape)[1]-2 & data.decade$longnum < bbox(country.shape)[3] + 2,]		
		data.decade = data.decade[data.decade$latnum > bbox(country.shape)[2]-2 & data.decade$latnum < bbox(country.shape)[4] + 2,]		
		data.decade = 	data.decade[duplicated(paste(data.decade$x, data.decade$y, sep="_"))==F,] #shouldn't be any duplictes anyway but just in case
												
												
												
			#build prevR objects according to prevR documentation
					clusters_5yr = data.decade[,c("clusterid","longnum","latnum","n5","died5yr","imr5")]
						names(clusters_5yr) = c("id","x","y","n","pos","prev")
									
					# lat lon for some clusters fall just outside of shape (presumably because of random perturbation added to coordinates for privacy)
					# need coordinates to be inside of shape for purpose of interpolation over shape
					# we will repeatedly move points slightly toward center of shape until they fall inside shape
					clusters_5yr[,"in.shape"]=point.in.SpatialPolygons(clusters_5yr$x, clusters_5yr$y, country.shape)
					shape.center = country.shape@polygons[[1]]@labpt; names(shape.center) = c("x","y")
							
					#repeatedly move the cluster lat lon slightly closer to the center of the shape until inside all cluster lat lon fall inside shape	
					 if(sum(is.na(clusters_5yr$x)+is.na(clusters_5yr$y))>0){stop("Something is wrong. Some coordinates are missing")}#stop if mising coordinates
					 while(sum(clusters_5yr[,"in.shape"]==F)>0){	
					
						 clusters_5yr[(clusters_5yr$in.shape==F),"x"] = clusters_5yr[(clusters_5yr$in.shape==F),"x"] - 
																		 ((clusters_5yr[(clusters_5yr$in.shape==F),"x"] - shape.center["x"])/25)
						 clusters_5yr[(clusters_5yr$in.shape==F),"y"] = clusters_5yr[(clusters_5yr$in.shape==F),"y"] - 
																		 ((clusters_5yr[(clusters_5yr$in.shape==F),"y"] - shape.center["y"])/25)
						 clusters_5yr[,"in.shape"]=point.in.SpatialPolygons(clusters_5yr$x, clusters_5yr$y, country.shape)
																 }
							
					#should have all clusters inside country now but just in case drop clusters that are still outside (there must be something wrong with those clusters)		
					 clusters_5yr = clusters_5yr[point.in.SpatialPolygons(clusters_5yr$x, clusters_5yr$y, country.shape),] 
					 
					  col <- c(id = "id",
				           x = "x",
				           y = "y",
				           n = "n",
				           pos = "pos",
				           prev = "prev"
								  )
					
						crs(country.shape) = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 ")
						proj = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 ")
					
					# create prevR object from items defined above
					
						fdhs_5yr = as.prevR(data = clusters_5yr, col = col, boundary = country.shape)
						N = Noptim(fdhs_5yr) #define optimum N					
						
					# Calculate rings (minimal radius area that meets minimum requirement for number obs (N) in circle for each cell)	
						dhs_5yr = rings(fdhs_5yr, N = N, progression = F)	
						# replace radii that are 0 with some small constant (must be  < cellsize so it doesn't draw on neighboring cell)
						dhs_5yr@rings[[1]]$estimates$r.radius[dhs_5yr@rings[[1]]$estimates$r.radius==0] = 0.05

					# Kernel Denstiy Estimator	
						kde_5yr = kde(dhs_5yr, N = N, R = R, weighted = F, risk.ratio = F, progression=F, keep.details = F, cell.size = 0.1)
						kde_5yr.raster = raster(kde_5yr)
					
					#resample population data grid to match country data grid
						pop.country.resample = resample(crop(pop.raster, extent(country.shape)), kde_5yr.raster)								
						child.pop.country.resample = resample(crop(childPop, extent(country.shape)), kde_5yr.raster)	
						
					#Re-organize data for output
							kde_5yr.values = data.frame(getValues(kde_5yr.raster), country,coordinates(kde_5yr.raster), N,decade, getValues(pop.country.resample), getValues(child.pop.country.resample))
								names(kde_5yr.values) = c("value","country","x","y","N","decade","pop","childpop")
								kde_5yr.values = kde_5yr.values[is.na(kde_5yr.values$value)==F,]
								kde_africa[[country]] = data.frame(kde_5yr.values)
					
																
						
		}#end country loop
	
		#unlist countries into single data frame
		kde_africa = data.frame(rbindlist(kde_africa))
	
		#native units is deaths per 100. Change to deaths per 1000 by mutliplying by 10.
		kde_africa$value = kde_africa$value*10		
		write_rds(kde_africa, file = paste("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5m0_",decade,".rds", sep=""), compress = "gz")
	

		print(paste(country,"-",decade))
	 }#end decade loop	
		

		






