source("scripts/packages.R")
source("scripts/functions.R")
source("scripts/0_preprocess.R")		#loads and preps data for analysis

############################################################################################################################################
######### All border data-prep #######
############################################################################################################################################


for (decade in c(1980,1990,2000)){


		kde_africa <- read_rds(paste("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5mo_",decade,".rds", sep=""))   			
		kde_africa = kde_africa[kde_africa$country%in%data.countries$ISO2,]


africa.data = africa[africa@data$ISO2%in%data.countries$ISO2,]
africa.data@data[,"NAME"] = as.character(africa.data@data[,"NAME"])
n.country = nrow(africa.data)



		#step 1:identify borders

				border.map = data.frame(matrix(nrow = n.country, ncol = n.country))
					rownames(border.map) = names(border.map) = africa.data@data$ISO2
				
				
					for (i in 1:n.country){
						for (j in 1:n.country){		
							border.map[i,j] = gTouches(africa.data[i,], africa.data[j,])
												}
											}
				
				border.map[upper.tri(border.map)] = NA
				diag(border.map)=NA
								
				n.border.pair = sum(border.map==T, na.rm=T)
								
				tmp.list = list()
				
				
				for (i in 1:n.country){
				
				if (length(which(border.map[,i]==T & is.na(border.map[,i])==F))>0){	
					
				tmp.list[[i]] = data.frame(rownames(border.map)[i], rownames(border.map)[which(border.map[,i]==T & is.na(border.map[,i])==F)])
				names(tmp.list[[i]]) = c("COUNTRY1","COUNTRY2")
					
					}
				}
				
				
				border.pairs = data.frame(rbindlist(tmp.list))
				border.pairs[,1] = as.character(border.pairs[,1])
				border.pairs[,2] = as.character(border.pairs[,2])
				
						
			#pull out overlapping lines (ie borders)
				border.list = list()
				for(i in 1:nrow(border.pairs)){
				border.list[[i]] = 
				gIntersection(africa.data[(africa.data$ISO2== border.pairs[i,1]),], africa.data[(africa.data$ISO2== border.pairs[i,2]),])
							}
				names(border.list) = paste(border.pairs[,1], border.pairs[,2], sep="-")
				
				
				
			#now add length of border into border.pairs object
				border.pairs[,"border.length"]=NA
				for (i in 1:nrow(border.pairs)){
				border.pairs[i,"border.length"] = gLength(border.list[[i]])*111.32
					}



			#step 2: pick a sample area around border and pull out data points in sample area
				
for (DIST in c(0.25,0.5,0.75,1)){ #distances in degrees

				buffer.list = list()
				
		for(i in 1:nrow(border.pairs)){
				
				buffer.list[[i]] = gBuffer(border.list[[i]], width = DIST)
				
													}
				names(buffer.list) = names(border.list)



			#pull out water polygons that overlap with border buffer area
			
			water.list = list(); length(water.list) = length(border.list); names(water.list) = names(border.list)

		for (i in 1:nrow(border.pairs)){			
			tmp.sub = sp::over(buffer.list[[i]], water, returnList=T)[[1]]$AF_WTR_ID; tmp.sub = tmp.sub[is.na(tmp.sub)==F]
			if(length(tmp.sub)>0){water.list[[i]] = water[(water@data$AF_WTR_ID %in% tmp.sub),]}			
							}


			#pull out country IMR data that fall into the buffer zone for each border pair
			# but DO NOT include cells that are over water
				
				data.border.list =list()
				
				
				for(i in 1:nrow(border.pairs)){
				
					data.border.list[[i]] = kde_africa[(kde_africa$country%in%c(border.pairs[i,1], border.pairs[i,2])),]
					data.border.list[[i]] = data.border.list[[i]][point.in.SpatialPolygons(data.border.list[[i]]$x,data.border.list[[i]]$y, buffer.list[[i]]),]
					
					#subset water points if there is water over border
					data.border.list[[i]][,"over.water"]=FALSE
					if(!is.null(water.list[[i]])){
					data.border.list[[i]][,"over.water"] = point.in.SpatialPolygons(data.border.list[[i]]$x,data.border.list[[i]]$y, water.list[[i]])
													}#if
													}#i
				names(data.border.list) = names(border.list)



			#step 3: Calculate distance from every point to border (i.e., to other country shape)
			
			for(i in 1:nrow(border.pairs)){
	
				COUNTRY1 =  border.pairs[i,1]
				COUNTRY2 =  border.pairs[i,2]
				c1 = africa.data[africa.data$ISO2==COUNTRY1,]
				c2 = africa.data[africa.data$ISO2==COUNTRY2,]
			
				data.border.list[[i]][,"dist.border"] = NA

					
				N = sum(data.border.list[[i]]$country==COUNTRY1)	
				
				for (j in 1:N){	
					data.border.list[[i]][(data.border.list[[i]]$country==COUNTRY1),"dist.border"][j] = 
						gDistance(SpatialPoints(data.border.list[[i]][(data.border.list[[i]]$country==COUNTRY1),c("x","y")][j,]), c2)								
								}
					
				M = sum(data.border.list[[i]]$country==COUNTRY2)	
				
				for (j in 1:M){	
					data.border.list[[i]][(data.border.list[[i]]$country==COUNTRY2),"dist.border"][j] = 
							gDistance(SpatialPoints(data.border.list[[i]][(data.border.list[[i]]$country==COUNTRY2),c("x","y")][j,]), c1)								
								}
	
			#pick country with higher average mortality rate and multiply it's distances by -1 (arbitrary choice but select this rule for consistency)
			MEAN1 = mean(data.border.list[[i]][data.border.list[[i]]$country==COUNTRY1,"value"], na.rm=T);names(MEAN1)=COUNTRY1
			MEAN2 = mean(data.border.list[[i]][data.border.list[[i]]$country==COUNTRY2,"value"], na.rm=T);names(MEAN2)=COUNTRY2
			MAX = max(MEAN1, MEAN2)			
			HIGHCOUNTRY = paste(names(MEAN1[MEAN1==MAX]), names(MEAN2[MEAN2==MAX]),sep="")

			data.border.list[[i]][data.border.list[[i]]$country==HIGHCOUNTRY,"dist.border"] = data.border.list[[i]][data.border.list[[i]]$country== HIGHCOUNTRY,"dist.border"]*(-1)
			data.border.list[[i]][,"country.pair"] = paste(border.pairs[i,1], border.pairs[i,2], sep="-")
			data.border.list[[i]][,"distance.cutoff"] = DIST
			data.border.list[[i]][,"left.country"] = HIGHCOUNTRY				
			data.border.list[[i]][,"right.country"] = c(COUNTRY1, COUNTRY2)[c(COUNTRY1, COUNTRY2)!=HIGHCOUNTRY]				

							}#i


			# Now store the data in a format convenient for analysis
			
			data.border = data.frame(rbindlist(data.border.list))
	
			save(data.border, buffer.list, border.list, border.pairs, water.list,
					file = paste("data/outputs/RD_intermediate_data/Border_Data_", DIST, "DegreeBuffer_",decade,".RData", sep=""))
			
			}	
	
	
	
	}#end decade loop
	
	
	
	
	
	
	