source("scripts/functions.R")
source("scripts/packages.R")

africa <- read_rds("data/inputs/africa_borders.rds")	

dat <- read_rds("data/outputs/final_data/ChildMortEstimates5m0_points.rds")

col.pal = colorRampPalette(c("firebrick3","orangered","orange1","yellow","lightgreen"))(256)[256:1]
data.breaks <- c(-1,seq(0,100,0.5),1000) # no values are negative but boundary cases sometimes left out so better to have neg left end point


pdf(file = paste("figures/raw/Figure1_part1_raw.pdf",sep=""), width = 16, height = 6)

	
	par(mfrow =c(1,3))
	par(oma = c(1,1,1,1))
	par(mar = c(0,0,3,0))
	par(family = "Times")


classint = classIntervals(dat$est5m0_1980, style = "fixed", fixedBreaks = data.breaks)
col = findColours(classint, col.pal)
		
		plot(africa, xlim = c(-10.5,45.5), ylim = c(-32.5,35), col = 'gray90', border = NA)
		points(dat$x, dat$y, col = col ,pch=15, cex=0.2)
		plot(africa, border = 'gray50',col =NA,  add=T, lwd=0.001)


classint = classIntervals(dat$est5m0_1990, style = "fixed", fixedBreaks = data.breaks)
col = findColours(classint, col.pal)
		
		plot(africa, xlim = c(-10.5,45.5), ylim = c(-32.5,35), col = 'gray90', border = NA)
		points(dat$x, dat$y, col = col ,pch=15, cex=0.2)
		plot(africa, border = 'gray50',col =NA,  add=T, lwd=0.001)


classint = classIntervals(dat$est5m0_2000, style = "fixed", fixedBreaks = data.breaks)
col = findColours(classint, col.pal)
		
		plot(africa, xlim = c(-10.5,45.5), ylim = c(-32.5,35), col = 'gray90', border = NA)
		points(dat$x, dat$y, col = col ,pch=15, cex=0.2)
		plot(africa, border = 'gray50',col =NA,  add=T, lwd=0.001)

dev.off()






