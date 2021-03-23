
	# Read in shapefile for African countries
	africa<-read_rds("data/inputs/africa_borders.rds") #shape (object in R) is called africa and is african country subset from "TM_WORLD_BORDERS-0.2.shp"	

	#imr and z-scores from DHS by cluster x decade 
      #read in and organize
	    #(note: can't release raw DHS data but to get from there to hear just group by cluster, decade and calculate metrics as described in paper)
	  imr <- read_rds("data/inputs/dhs_cluster_level_averages.rds")		
    	  imr[,"ISO2"] = substr(as.character(imr$svyid),1,2) #ISO2 corresponds to column in africa shapefile
    		imr = imr[names(imr)%in%c("x","y")==F,]
    				imr[,"x"] = imr[,"longnum"]
    				imr[,"y"] = imr[,"latnum"]
    		imr[(imr$decade==1),"decade"] = 1970
    		imr[(imr$decade==2),"decade"] = 1980
    		imr[(imr$decade==3),"decade"] = 1990
    		imr[(imr$decade==4),"decade"] = 2000
    		names(imr)[names(imr)=="unique_id"]="clusterid"
    		imr[,"n1"]= imr[,"under1_alive_midyr"] + imr[,"under1death_in_yr"]
    		imr[,"n5"]= imr[,"under5_alive_midyr"] + imr[,"under5death_in_yr"]
    		names(imr)[names(imr)=="under1death_in_yr"]="died1yr"
    		names(imr)[names(imr)=="under5death_in_yr"]="died5yr"
    		imr <- imr[!is.na(imr$x) & !is.na(imr$y),] #drop any obs missing coordinates

	# Check country match between map and DHS data. Do not have DHS data for every African country.
  		#57 african countries in shapefile
  		#DHS country codes: http://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  		#DHS country codes mostly correspond to ISO2 in shapefile but some codes need to be manually corrected
  		
  		#change codes for Namibia and Madagascar and Niger and Liberia and Burundi to match the map codes
		africa@data$ISO2 = as.character(africa@data$ISO2)
		africa@data$ISO2[africa@data$NAME =="Namibia"] = "NM"
		africa@data$ISO2[africa@data$ISO2=="MG"] = "MD"
		africa@data$ISO2[africa@data$ISO2=="LR"] = "LB"
		africa@data$ISO2[africa@data$ISO2=="NE"] = "NI"
		africa@data$ISO2[africa@data$ISO2=="BI"] = "BU"
		
		africa@data[,"NAME"] = as.character(africa@data[,"NAME"])
		africa@data[,"NAME"][africa@data[,"NAME"]=="United Republic of Tanzania"] = "Tanzania"
		africa@data[,"NAME"][africa@data[,"NAME"]=="Democratic Republic of the Congo"] = "DRC"
		africa@data[,"NAME"][africa@data[,"NAME"]=="Cote d'Ivoire"] = "Ivory Coast"
		africa@data$NAME[africa@data$NAME=="Libyan Arab Jamahiriya"] = "Libya"
		africa@data$NAME[africa@data$NAME=="Central African Republic"] = "CAR"

		africa = africa[africa@data$NAME%in%c("Comoros","Madagascar","Mauritius","Reunion","Cape Verde","Sao Tome and Principe","Mayotte","Seychelles")==F,]

		
		#check no. countries
		sum(africa@data$ISO2%in%imr$ISO2)
		
		#create df of country names and codes for those countries with/without data
		data.countries = africa@data[africa@data$ISO2%in%imr$ISO2,c("NAME","ISO2")]; rownames(data.countries) = 1:nrow(data.countries); 
					data.countries[,2] = as.character(data.countries[,2])
        	#drop NI and CF because insufficient data (no obs 2000)
        	data.countries = data.countries[data.countries$ISO2%in%c("CF","EG","MD","MA","KM","NI")==F,]
			no.data.countries = africa@data[africa@data$ISO2%in%imr$ISO2==F,c("NAME","ISO2")]		
		rownames(data.countries) = 1:nrow(data.countries)
	
 	#load shapefile of water for potential mapping exercises
		water = read_rds("data/inputs/africa_waterbodies.rds")
 
  
	# load population data created in script `Population_Mask_Landscan.R'
		pop.raster <- read_rds("data/inputs/pop_total.rds")
	  childPop <- read_rds("data/inputs/pop_child.rds")
	
	
	#add in 	more info about observed birth weighted year averages by country-decade
		denom_wt = read_rds("data/inputs/rate_denominator_adjusted.rds")
		denom_wt = denom_wt[denom_wt$country%in%data.countries$ISO2,]
		names(denom_wt)[names(denom_wt)=="year"] = "end_yr"
		imr = imr[imr$ISO2%in%data.countries$ISO2,]
		imr = merge(imr, denom_wt, by = c("ISO2"), all.x = TRUE)
