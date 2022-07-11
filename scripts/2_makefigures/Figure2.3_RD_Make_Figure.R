##########################################################################################################################################################		
source("scripts/packages.R")
source("scripts/functions.R")
source("scripts/0_preprocess.R")		#loads and preps data for analysis
##########################################################################################################################################################			


load("data/outputs/RD_intermediate_data/RDestimates.RData") 

load(paste("data/outputs/RD_intermediate_data/Border_Data_0.25DegreeBuffer_1980.RData",sep="")); data.border.25 = data.border #only need small bandwidth for testing area over water
load(paste("data/outputs/RD_intermediate_data/Border_Data_1DegreeBuffer_1980.RData",sep="")); 

border.pairs[,"PAIR"] = paste(border.pairs$COUNTRY1,border.pairs$COUNTRY2, sep="-")

#change some name formats for better display in figure
africa@data$NAME[africa@data$NAME=="Dem Rep Congo"] = "DRC"
africa@data$NAME[africa@data$NAME=="Ivory Coast"] = "Iv. Coast"
africa@data$NAME[africa@data$NAME=="Sierra Leone"] = "Sra. Leone"
africa@data$NAME[africa@data$NAME=="Burkina Faso"] = "Brk. Faso"
africa@data$NAME[africa@data$NAME=="Mozambique"] = "Mozam."
africa@data$NAME[africa@data$NAME=="Swaziland"] = "Swaz."
africa@data$NAME[africa@data$NAME=="Tanzania"] = "Tanz."

############################################################################################################################################
######### All border analysis #######
############################################################################################################################################

	#[0]		Drop borders that have more than 20% of area covered in water
			share.water = aggregate(data.border.25$over.water, by = list(PAIR = data.border.25$country.pair), FUN = function(x){sum(x==F)/length(x)})
			drop.water = share.water[(share.water$x<0.8),"PAIR"] #really is share not over water

	#[I] 	Determine significance for each border-pair
			
			median = aggregate(RDestimates$est, by = list(PAIR = RDestimates$PAIR, decade= RDestimates$decade), FUN = function(x){quantile(x, .5, na.rm=T)})
				names(median)[names(median)=="x"] = "median"
			
			pval.all = aggregate(RDestimates$est, by = list(PAIR = RDestimates$PAIR, decade= RDestimates$decade, model =RDestimates$model, DIST = RDestimates$DIST), 
									FUN = function(x){sum(x<0, na.rm=T)/sum(!is.na(x))})
			pval.med = aggregate(pval.all$x, by = list(PAIR = pval.all$PAIR, decade = pval.all$decade), FUN = function(x){median(x, na.rm=T)})
				names(pval.med)[names(pval.med)=="x"] = "pval"
			
			#preferred specification
			pval.poly = pval.all[pval.all$model=="poly-3" & pval.all$DIST==5,]
				names(pval.poly)[names(pval.poly)=="x"] = "pval"
			

			PAIRestimates = data.frame(median, pval.poly$pval)
				names(PAIRestimates)[names(PAIRestimates)=="pval.poly.pval"]="pval"
			
			PAIRestimates$pval = round(PAIRestimates$pval,2)
			PAIRestimates$median = round(PAIRestimates$median, 2)


			
			

	#[II] 	Load and combine data from different decades and with different bandwidths into single list for easy looping

			data.border.list = list(); length(data.border.list) = 4; names(data.border.list) = c(1980,1990,2000)
			
								for(decade in as.character(c(1980,1990,2000))){
									
									data.border.list[[decade]] = list(); length(data.border.list[[decade]])=length(c(1,0.75,0.5, 0.25));
									names(data.border.list[[decade]])=c(1,0.75,0.5, 0.25)
									
									for (buffer in c(1,0.75,0.5, 0.25)){
									load(paste("data/outputs/RD_intermediate_data/Border_Data_",buffer,"DegreeBuffer_",decade,".RData",sep=""))
									if(decade=="deltas"){data.border[,"value"] = data.border[,"change"]} #so variable name of interest is consistent								
									data.border.list[[decade]][[as.character(buffer)]] = data.border									
																	}#buffer loop
																	
																		}#decade loop
								#	names(data.border.list)[4]="delta"#for consistency with RD estimate labels


	# [III] Determine plot order and plot country name order
				
					border.pairs[,"PAIR"] = paste(border.pairs$COUNTRY1, border.pairs$COUNTRY2,sep="-")

				#figure out the order of plotting and country label order. We want plot to start with biggest RD coeff and go the smallest to make visual look better
				# We will use average coefficient estimate across bw-functional form so we have average effect for each pair-decade
	
					border.info = aggregate(RDestimates$est, by = list(decade = RDestimates$decade, PAIR = RDestimates$PAIR), FUN = function(x){median(x, na.rm=T)}) 
					names(border.info)[names(border.info)=="x"] = "median"
					border.info = border.info[(border.info$PAIR%in%border.pairs$PAIR & border.info$PAIR%in%drop.water==F),]

					
					
					
			
#[I] STANDARD RD PLOTS FOR ALL BORDER PAIRS


		#Loop over decades with different figure for each decade:
			
		for(decade in c(1980,1990,2000)){
				
			border.pair.plot.order = border.info[(border.info$decade==decade),]					
			border.pair.plot.order = border.pair.plot.order[order(border.pair.plot.order$median, decreasing = T),]							

			#Namibia and Zimbabwe only meet at a single point so drop that border combo
			border.pair.plot.order <- border.pair.plot.order[border.pair.plot.order$PAIR!="NM-ZW",]
			

	png(paste("figures/raw/BorderRD_Figures_",decade, ".png", sep=""), width = 2400, height = 2800)		

			par(oma = c(7,8,1,6))
			par(mar = c(5.1, 2.1, 4.1, 0.1))
			mat = matrix(nrow = 7, ncol =6, data = c(1:40,41,41), byrow = T)
			layout(mat)
			par(family = "Times")
								
			#loop over country.pairs 
			for (j in 1:length(border.pair.plot.order$PAIR)){

				COUNTRY1 =  border.pair.plot.order[j,"COUNTRY1"]
				COUNTRY2 =  border.pair.plot.order[j,"COUNTRY2"]
				PAIR = border.pair.plot.order[j,"PAIR"]


				#Initialize plot by plotting raw data from largest bandwidth. Everything else will be plotted on top of this (For each pair)
				new.data = data.border.list[[as.character(decade)]][["1"]][data.border.list[[as.character(decade)]][["1"]]$country.pair==PAIR & data.border.list[[as.character(decade)]][["1"]]$over.water==F,]
				
				y = new.data$value
				x = new.data$dist.border

				ave.dist.brd = aggregate(new.data$dist.border, by = list(country = new.data$country), FUN = mean)	

				name.c1 = africa@data[(africa@data$ISO2==ave.dist.brd[(ave.dist.brd$x<0),"country"]),"NAME"]		
				name.c2 = africa@data[(africa@data$ISO2==ave.dist.brd[(ave.dist.brd$x>0),"country"]),"NAME"]		


				xbin = seq(-1,1, 2*1/150)
				yxbin = rep(NA,length(xbin))
				for (i in 1:length(xbin)){yxbin[i] = mean(y[x > xbin[i] & x < xbin[i+1]], na.rm=T)}
				
				
				ylim = c(0,155); if (decade=='delta'){ylim = c(-75,75)};if (decade=="2000"){ylim = c(0,75)}

				
				plot(xbin, yxbin, col=add.alpha('gray70', 0.6),cex=.9, pch=20, axes = F, xlab = "", ylab = "", xlim = c(-1,1), ylim = ylim)
				segments(x0 = 0,x1=0,y0=ylim[1], y1= (ylim[2]-25), lwd=2.5, lty=3, col = 'pink')
				abline(h = seq(0,ylim[2]-30,50), col = 'gray90', lwd=2.5, lty=3)
				if(decade=="2000"){abline(h = seq(0,60,20), col = 'gray90', lwd=2.5, lty=3)}
				if(decade=="2000"){segments(x0 = 0,x1=0,y0=ylim[1], y1= 60, lwd=2.5, lty=3, col = 'pink')}
				if(decade=="delta"){abline(h = seq(-50,50,50), col = 'gray90', lwd=2.5, lty=3)}


				#format pval for plotting (stupid I have to do this. there has to be a better way)
				pval.print = format(PAIRestimates[(PAIRestimates$PAIR == PAIR & PAIRestimates$decade==decade),"pval"],digits=3)
					pval.print[pval.print==0]="0.00"; pval.print[pval.print=="0.1"]="0.10"; pval.print[pval.print=="0.2"]="0.20";
					pval.print[pval.print=="0.3"]="0.30"; pval.print[pval.print=="0.4"]="0.40"; pval.print[pval.print=="0.5"]="0.50";
					pval.print[pval.print=="0.6"]="0.60"; pval.print[pval.print=="0.7"]="0.70"; pval.print[pval.print=="0.8"]="0.80";
					pval.print[pval.print=="0.9"]="0.90"

				med.print = round(abs(PAIRestimates[(PAIRestimates$PAIR == PAIR & PAIRestimates$decade==decade),"median"]), digits=0)


				#add axis and labels
				mtext(side = 3, outer = F, text = paste(name.c1, name.c2, sep="-") ,cex = 4, line = 0.5, col = 'gray50')						
				mtext(side = 3, text = paste("diff=", med.print,"; p-val=", pval.print ,sep="") , col = 'gray75', cex=3.25,line=-4)			
				

								
					for (DIST in c(1,0.75,0.5)){
					cutoff = DIST

									data.border = data.border.list[[as.character(decade)]][[as.character(DIST)]]
													
									y = data.border[data.border$country.pair==PAIR & data.border$over.water==F,"value"]
									x = data.border[data.border$country.pair==PAIR & data.border$over.water==F,"dist.border"]
									D = dummy(x>0)[,2]
									y = y[abs(x)<cutoff]; D = D[abs(x)<cutoff]; x = x[abs(x)<cutoff]
								
																
									p = 3; est1a = lm(y ~ D*poly(x,p)); 
											results = data.frame(x,y,predict(est1a));names(results)[3] = "pred1a"; 
									p = 4; est1b =  lm(y ~ D*poly(x,p)); results[,"pred1b"] = predict(est1b)
									p = 5; est1c =  lm(y ~ D*poly(x,p)); results[,"pred1c"] = predict(est1c)
									df = 3; est1d = lm(y ~ D*ns(x,df)); results[,"pred1d"] = predict(est1d)
									df = 4; est1e = lm(y ~ D*ns(x,df)); results[,"pred1e"] = predict(est1e)
									df = 5; est1f = lm(y ~ D*ns(x,df)); results[,"pred1f"] = predict(est1f)

									results = results[order(results$x),]
								
																
									col_sig = 'gold'
									if(PAIRestimates[(PAIRestimates$PAIR == PAIR & PAIRestimates$decade==decade),"pval"]<0.05){col_sig = 'green'}
									
									segments(x0=0,x1=0, y0 = results[(results$x == min(results$x[results$x>0])),"pred1a"], 
														y1 = results[(results$x == max(results$x[results$x<0])),"pred1a"],
														col=col_sig,lwd=5)
								
									lines(results$x[results$x>0], results$pred1a[results$x>0], col=add.alpha('red',0.25),lwd=3)
									lines(results$x[results$x <0], results$pred1a[results $x<0], col=add.alpha('navyblue',0.25),lwd=3)
				
									lines(results$x[results$x>0], results$pred1c[results$x>0], col=add.alpha('red',0.25),lwd=3)
									lines(results$x[results$x <0], results$pred1c[results $x<0], col=add.alpha('navyblue',0.25),lwd=3)
				
	    						lines(results$x[results$x>0], results$pred1e[results$x>0], col=add.alpha('red',0.25),lwd=3)
									lines(results$x[results$x <0], results$pred1e[results$x<0], col=add.alpha('navyblue',0.25),lwd=3)
									
									 lines(results$x[results$x>0], results$pred1f[results$x>0], col=add.alpha('red',0.25),lwd=3)
									 lines(results$x[results$x<0], results$pred1f[results $x<0], col=add.alpha('navyblue',0.25),lwd=3)

							}#DIST loop
				
				#bottom row			
				if(j%in%37:40){
				axis(1, at = c(-.95,-.5, 0,.5, .95), labels = c(-100,-50, 0,50, 100), tick = F, cex.axis=3.5, col.axis = 'gray75', line =1)								
				mtext(side = 1, outer = F, text = "Distance to Border", cex=3.5, col = 'gray60', line=8)}
			
				
				#side column
				y.lab = "deaths/1000 child-yrs"; if (decade=="delta"){y.lab = "change in child mortality"}
				if(j%in%seq(1,37,6)){
					mtext(side = 2, outer = F, text = y.lab, cex=2.75, col = 'gray60', line=7)
					axis(2, tick = F, cex.axis=4, col.axis = 'gray75',las=2, line = 0)								
					}
										
								
								}#PAIR loop


				
					#legend
					par(mar = c(0,3,0,3))
					plot(1:10,rep(5,10), col = 'white', ylim = c(3,8), axes = F, xlab = "", ylab ="")	
					lines(seq(1,5.5,.5),rep(5, length(seq(1,5.5,.5)))  ,col = 'gold', lwd=8)
					lines(seq(5.5,10,.5),rep(5, length(seq(1,5.5,.5)))  ,col = 'green', lwd=8)
					text(x = 3.25, y = 4.6,labels = "not statistically significant", col = 'gray50',cex=4)	
					text(x = 8, y = 4.6,labels = "statistically significant", col = 'gray50',cex=4)	
					text(x = 5.5, y =5.55,labels = "cross-border difference", col = 'gray50',cex=5.5)	
						
						dev.off()
				
						
													}#decade loop
	
		
		
	
	
	
	
	

		