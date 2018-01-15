
qgsLayer = setClass("qgsLayer", slots = c(
	project = "qgsProject",
	name = "character",
	source = "character"
))

setMethod("show", "qgsLayer", function(object){
	cat("An object of class qgsLayer (QGIS Layer)\n")
	cat(paste("Layer: ", object@name, "\n", sep = ""))
	cat(paste("File: ", object@source, "\n", sep = ""))
})

vectorDrivers = list(
	shp = "ESRI Shapefile",
	geojson = "GeoJSON",
	osm = "OSM"
)

setMethod("getData", "qgsLayer", function(object){
	source = object@source

	extension = substr(source, nchar(source) - 3 + 1, nchar(source))

	driver = vectorDrivers[[extension]]

	if(is.null(driver)) stop(paste("Could not find driver for ", extension, ".", sep=""))

	readOGR(dsn = dirname(source), layer = tools::file_path_sans_ext(basename(source)))
})

setMethod("plot", "qgsLayer", function(x, y, ...) {
	plot(getData(x), ...)
})

