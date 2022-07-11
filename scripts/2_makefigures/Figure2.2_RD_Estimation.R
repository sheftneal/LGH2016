##########################################################################################################################################################		
source("scripts/packages.R")
source("scripts/functions.R")
source("scripts/0_preprocess.R")		#loads and preps data for analysis
##########################################################################################################################################################			

		# get index that will allow us to index bootstrap sample and identify the relevant border area data
		
    #file is 1.3GB so not on Github. 
    #Can download here first: https://www.dropbox.com/s/42lfhwkz14wlrjy/Africa_CVBootstrap_5yr_Noptim_cellsize01_unweighted.RData?dl=1
    #then put in data/outputs/
    load("data/outputs/ChildMortality_5m0/ChildMortality_KDE_5mo_CVBootstrap_allYears.RData"); B = 500;			
		
    #kde_africa_cv is an array that is [cell x bootstrap x decade]
		#kde_africa_cv_face is a data frame that corresponds to the cells in the array. It is [cell x 3] and can be used to subset the array
	
		#load 1 degree data, decade doesnt matter because we just pull lat lon of points for each border pair. Use 1 degree then can subset.
		load(paste("data/outputs/RD_intermediate_data/Border_Data_1DegreeBuffer_1980.RData", sep="")) #data.border
		data.border[,"db.index"] = 1:nrow(data.border)
	
		kde_africa_cv_face[,"bs.index"] = 1:nrow(kde_africa_cv_face)
		border.index = merge(data.border[,c("country","x","y","pop","dist.border","country.pair","distance.cutoff","db.index")], 
										kde_africa_cv_face, by = c("country","x","y"), all.x = TRUE)
		border.index = border.index[order(border.index$db.index),]

		bs.border.data = kde_africa_cv[border.index$bs.index,,]
		tmp = array(dim = c(dim(bs.border.data)[1], B, 4))
		tmp[,,1:3] = bs.border.data
		tmp[,,4] = bs.border.data[,,3] - bs.border.data[,,1]
		bs.border.data = tmp; rm(tmp)
		dimnames(bs.border.data) = list(1:nrow(bs.border.data), 1:500, c(1980,1990,2000,"deltas"))
	
		border.pairs[,"PAIR"] = paste(border.pairs$COUNTRY1, border.pairs$COUNTRY2,sep="-")

##########################################################################################################################################################			
## Estimation
##########################################################################################################################################################			



RD = list(); length(RD) = 4; names(RD) = c(0.25,0.5,0.75, 1)
rdindex = 0


		for (DIST in c(0.25,0.5,0.75, 1,5)){
		cutoff = DIST
		
		rdindex = rdindex+1
		RD[[rdindex]] = list(); length(RD[[rdindex]]) = 4; names(RD[[rdindex]])=c(1980,1990,2000,"deltas")
				
			for(decade in 1:4){
				
				RD[[rdindex]][[decade]] = list(); length(RD[[rdindex]][[decade]]) = length(border.pairs$PAIR)
				names(RD[[rdindex]][[decade]]) = border.pairs$PAIR
				
					for (j in 1:length(border.pairs$PAIR)){

					
					results1 =results2 = results3 = results4 = results5 = results6= results7 = results8 = data.frame(matrix(nrow = B, ncol = 4)); 
					names(results1) = names(results2) = names(results3) = names(results4) = names(results5) = names(results6) = names(results7) = names(results8) = c("est","se","tval","model")
					

						for (b in 1:B){
				
									COUNTRY1 =  border.pairs[j,1]
									COUNTRY2 =  border.pairs[j,2]
									PAIR = border.pairs[j,"PAIR"]
									name.c1 = as.character(africa@data[africa@data$ISO2==COUNTRY1,"NAME"])			
									name.c2 = as.character(africa@data[africa@data$ISO2==COUNTRY2,"NAME"])	
													
									y = bs.border.data[data.border$country.pair==PAIR,b,decade]
									x = border.index$dist.border[border.index$country.pair==PAIR]
									D = dummy(x>0)[,2]
									w = 1
									if(DIST==5){cutoff = IKbandwidth(x,y,0)} #allow automatic selection of bandwidth for each b data realization
	
									y = y[abs(x)<cutoff]; D = D[abs(x)<cutoff]; w = w[abs(x)<cutoff]; x = x[abs(x)<cutoff]
																
									p = 3; est1a = 	lm(y ~ D*poly(x,p, raw = T)); 											
									p = 4; est1b =  lm(y ~ D*poly(x,p, raw = T));
									p = 5; est1c =  lm(y ~ D*poly(x,p, raw = T));
									
									df = 3; est1d = lm(y ~ D+ns(x,df)); 
									df = 4; est1e = lm(y ~ D+ns(x,df)); 
									df = 5; est1f = lm(y ~ D+ns(x,df)); 
											est1g = lm(y~D*x)

									results1[b,1:3] = as.numeric(summary(est1a)$coefficients["D",1:3]);
									results2[b,1:3] = as.numeric(summary(est1b)$coefficients["D",1:3]);  
									results3[b,1:3] = as.numeric(summary(est1c)$coefficients["D",1:3]);  
									results4[b,1:3] = as.numeric(summary(est1d)$coefficients["D",1:3]);  
									results5[b,1:3] = as.numeric(summary(est1e)$coefficients["D",1:3]);  
									results6[b,1:3] = as.numeric(summary(est1f)$coefficients["D",1:3]); 
									results7[b,1:3] = as.numeric(summary(est1g)$coefficients["D",1:3]); 
									
										
									if(length(unique(round(x,2)))>50){ #fails with insufficient data										
									results8[b,1] = RDestimate(y~x, bw = cutoff)$est[1]
																	}#if
								}#b loop		
								
									 results1[,4]="poly-3" 
									 results2[,4]="poly-4"
									 results3[,4]="poly-5"
									 results4[,4]="spline-3"
									 results5[,4]="spline-4"
									 results6[,4]="spline-5" 
									 results7[,4]="Linear" 
									 results8[,4]="Loc-Lin" 

									results = rbind(results1, results2, results3, results4, results5, results6,results7, results8)
								
								RD[[rdindex]][[decade]][[PAIR]] = results

								}#j loop of PAIRs
						
							print(decade)
				
								}#decade loop
						
							print(DIST)
							}#DIST loop
						


###re-format

		#RD: list[[DIST]][[decade]][[PAIR]]
		#4 x 4 x 30 x (500x4)
		
		for (i in 1:length(RD)){
			for (j in 1:length(RD[[i]])){
				for (k in 1:length(RD[[i]][[j]])){
					RD[[i]][[j]][[k]][,"PAIR"] = border.pairs$PAIR[k]
											}
										}
									}
	
		for (i in 1:length(RD)){
			for (j in 1:length(RD[[i]])){
					RD[[i]][[j]] = data.frame(rbindlist(RD[[i]][[j]]))
										}
									}		

		for (i in 1:length(RD)){
			for (j in 1:length(RD[[i]])){
					RD[[i]][[j]][,"decade"] = c(1980,1990,2000,"delta")[j]
										}
									}		
		
		for (i in 1:length(RD)){
					RD[[i]] = data.frame(rbindlist(RD[[i]]))
								}		

		for (i in 1:length(RD)){
					RD[[i]][,"DIST"] = c(0.25,0.5,0.75, 1,5)[i]
								}		
		
		
		RD = data.frame(rbindlist(RD))
		RDestimates = RD; rm(RD)
		
		
		
	#check whether median cross-border diff is negative. If yes, multiply all estimates by -1 to flip to other side of axis for convenience
	#(since order of country pair doesn't matter).
	
	qmed = aggregate(RDestimates$est, by = list(PAIR = RDestimates$PAIR, model = RDestimates$model, decade= RDestimates$decade, DIST = RDestimates$DIST), FUN = function(x){quantile(x, .5, na.rm=T)})
	RDestimates = merge(RDestimates, qmed, by = c("PAIR", "model","decade","DIST"), all.x = TRUE)
	names(RDestimates)[names(RDestimates)=="x"] = "median"
	RDestimates$median[is.na(RDestimates$median)]=0
	RDestimates[(RDestimates$median<0),"est"] = RDestimates[(RDestimates$median<0),"est"]*-1 #get everything into same signs since ordering of the country pair is arbitrary
	RDestimates[(RDestimates$median<0),"tval"] = RDestimates[(RDestimates$median<0),"tval"]*-1 #get everything into same signs since ordering of the country pair is arbitrary
				
save(RDestimates, file = "data/outputs/RD_intermediate_data/RDestimates.RData")
