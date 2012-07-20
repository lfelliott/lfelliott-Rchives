library(raster)
cover <- raster("30m_guads")
interval <- 60
latdeg <- 31.5
daystart <- 80
dayend <- 80
# convert time to radians
timev <- interval * 0.0043633
daynumber <- daystart
deg2rad <- pi/180
latitude <- latdeg * deg2rad
slopegrid <- terrain(cover, opt="slope", unit="radians")
aspectgrid <- terrain(cover, opt="aspect", unit="degrees")
aspectgrid1 <- aspectgrid
aspectgrid1[aspectgrid <= 180 & aspectgrid != -1] <- 180 - aspectgrid[aspectgrid <= 180 & aspectgrid != -1]
aspectgrid1[aspectgrid > 180] <- 540 - aspectgrid[aspectgrid > 180]
aspectgrid1[aspectgrid == -1] <- 0
aspectgrid2 <- aspectgrid1 * deg2rad
aspectgridr <- terrain(cover, opt="aspect", unit="radians")
outgrid0 <- hillShade(slopegrid, aspectgridr, angle=0, direction=0)
initialgrid <- outgrid0 * 0
rm(outgrid0)
rm(aspectgrid)
rm(aspectgrid1)

for (daynumber in daystart:dayend)
	{
	io <- 1.367 * (1 + 0.034 * cos(360 * deg2rad * daynumber / 365))
	decl <- 23.45 * deg2rad * sin(deg2rad * 360 * (284 + daynumber)/365)
	if ((-1 * tan(latitude) * tan(decl)) < -1) sunrise <- acos(-1)
	else if ((-1 * tan(latitude) * tan(decl)) > 1.0) sunrise <- acos(1)
	else sunrise <- acos(-1 * tan(latitude) * tan(decl))
	sunset <- -1 * sunrise
	hourangle = sunrise - (timev / 2)
	pass = 1
	while (hourangle >= sunset)
		{
		solaralt <- asin(sin(latitude) * sin(decl) + cos(latitude) * cos(decl) * cos(hourangle))
		test <- tan(decl)/tan(latitude)
		if (cos(hourangle) > test) solaraz <- asin(cos(decl) * sin(hourangle)/cos(solaralt))
		else if (cos(hourangle) < test) solaraz <- pi - asin(cos(decl)*sin(hourangle)/cos(solaralt))
		else if (test == cos(hourangle) & hourangle >= 0) solaraz <- pi/2
		else if (test == cos(hourangle) & hourangle < 0) solaraz <- -1 * pi / 2
		if (solaraz >= 0) solarazdeg <- solaraz * 57.29578
		else solaraz <- 360 - abs(solaraz) * 57.29578
		M <- 1229 + (614 * sin(solaralt)^2)^0.5 - 614 * sin(solaralt)
		e <- 2.7182818
		tau <- 0.56 * e ^ (-0.65 * M) + e ^ (-0.095 * M)
		vIs <- io * tau
		solaraltdeg <- solaralt * 57.29578
		if (solarazdeg <= 180) azi <- 180 - solarazdeg
		else azi <- 180 + (360 - solarazdeg)
		sungrid = hillShade(slopegrid, aspectgridr, angle = solaralt, direction = azi, normalize=T)
		cosi <- sin(decl) * (sin(latitude) * cos(slopegrid) - cos(latitude) * sin(slopegrid) * cos(aspectgrid2)) + cos(decl) * cos(hourangle) * (cos(latitude) * cos(slopegrid) + sin(latitude) * sin(slopegrid) * cos(aspectgrid2)) + cos(decl) * sin(slopegrid) * sin(aspectgrid2) * sin(hourangle)
		shaded <- cosi
		shaded[cosi < 0] <- 0
		shaded[cosi >= 0] <- 1
		wattsgrid <- vIs * cosi * sungrid * shaded * 60 * interval
		initialgrid <- wattsgrid + initialgrid
		hourangle <- hourangle - timev
		pass <- pass + 1
		}
	}
writeRaster(initialgrid, "shortwavc.img", format = "HFA")
		