# Function that returns latitude and longitude from an image fileFu
imglatlong <- function(imgname)
	{
	require(rgdal)
	x <- GDALinfo(imgname)
	xattr <- attr(x, "mdata")
	exiflong <- xattr[grep("EXIF_GPSLongitude=", xattr)]
	exiflat <- xattr[grep("EXIF_GPSLatitude=", xattr)]
	long <- as.numeric(gsub("^.*=\\(([0-9\\.]+)\\).*","\\1",exiflong)) + as.numeric(gsub("^.*=\\([0-9\\.]+\\) \\(([0-9\\.]+)\\).*","\\1",exiflong))/60 + as.numeric(gsub("^.*=\\([0-9\\.]+\\) \\([0-9\\.]+\\) \\(([0-9\\.]+)\\).*","\\1",exiflong))/3600
	lat <- as.numeric(gsub("^.*=\\(([0-9\\.]+)\\).*","\\1",exiflat)) + as.numeric(gsub("^.*=\\([0-9\\.]+\\) \\(([0-9\\.]+)\\).*","\\1",exiflat))/60 + as.numeric(gsub("^.*=\\([0-9\\.]+\\) \\([0-9\\.]+\\) \\(([0-9\\.]+)\\).*","\\1",exiflat))/3600
	if (any(grep("EXIF_GPSLongitudeRef=W", xattr))) long <- -(long)
	if (any(grep("EXIF_GPSLatitudeRef=S", xattr))) lat <- -(lat)
	return(c(lat, long))
	}
