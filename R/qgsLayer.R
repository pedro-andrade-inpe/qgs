
#' @title A layer of a QGIS project
#' @description This class represents a layer of a QGIS project.
#' Please do not create it manually. Instead, use \code{\link{openLayer,qgsProject-method}}.
#'
#' @slot project A qgsProject.
#' @slot name The name of the layer.
#' @slot provider Name of its provider (WMS, ogr, gdal).
#' @slot source Description of where the data is stored.
qgsLayer = setClass("qgsLayer", slots = c(
	project = "qgsProject",
	name = "character",
	provider = "character", # "WFS", "ogr", "gdal"
	source = "character"
))

#' @title Show the properties of a QGIS layer
#' @description Show the following properties of a QGIS layer: name, provider, and source.
#' @param object A qgsLayer object.
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("show", "qgsLayer", function(object){
	cat("An object of class qgsLayer (QGIS Layer)\n")
	cat(paste("Name: \"", object@name, "\"\n", sep = ""))
	cat(paste("Provider: \"", object@provider, "\"\n", sep = ""))
	cat(paste("Source: \"", object@source, "\"\n", sep = ""))
})

qgs_readGDAL = function(source){
	raster::raster(source)
}

qgs_codeReadGDAL = function(layer, source){
	paste("require(raster)\n",
		layer,
		" = raster(\"", source,
		"\")\n",
		sep = ""
	)
}

qgs_readOGR = function(source){
	extension = substr(source, nchar(source) - 3 + 1, nchar(source))

	readOGR(dsn = dirname(source), layer = tools::file_path_sans_ext(basename(source)), verbose = FALSE)
}

qgs_codeReadOGR = function(layer, source){
	extension = substr(source, nchar(source) - 3 + 1, nchar(source))

	paste("require(rgdal)\n",
		layer,
		" = readOGR(dsn = \"", dirname(source),
		"\", layer = \"", tools::file_path_sans_ext(basename(source)),
		"\", verbose = FALSE)\n",
		sep = ""
	)
}

qgs_readWMS = function(source){
	sourcesplit = strsplit(source, " ")
	url = strsplit(sourcesplit[[1]][5], "'")[[1]][2]
	layer = strsplit(sourcesplit[[1]][4], "'")[[1]][2]

	readOGR(paste("WFS:", url, sep = ""), layer)
}

qgs_codeReadWMS = function(layer, source){
	sourcesplit = strsplit(source, " ")
	url = strsplit(sourcesplit[[1]][5], "'")[[1]][2]
	layer = strsplit(sourcesplit[[1]][4], "'")[[1]][2]

	paste("require(rgdal)\n",
		layer,
		" = readOGR(dsn = \"WFS:", url,
		"\", layer = \"", layer,
		"\")\n",
		sep = ""
	)
}

#' @title Code to read data
#' @description Return a string with R code to the data from a layer using the data source directly.
#' @param object A qgsLayer object.
#' @rdname readDataCode-methods
#' @aliases readDataCode readDataCode,qgsLayer-method
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("readDataCode", "qgsLayer", function(object){
	if(object@provider == "ogr"){
		cat(qgs_codeReadOGR(object@name, object@source))
	}
	else if(object@provider == "WFS"){
		cat(qgs_codeReadWMS(object@name, object@source))
	}
	else if(object@provider == "gdal"){
		cat(qgs_codeReadGDAL(object@name, object@source))
	}
	else
		stop(paste("Could not recognize provider ", object@provider, ".", sep = ""))
})

#' @title Read data from a QGIS layer
#' @description Read data from the data sorce pointed by a QGIS layer into an sp object. It automatically recognizes the type of the data source in order to read it.
#' @param object A qgsLayer object.
#' @rdname readData-methods
#' @aliases readData readData,qgsLayer-method
#' @seealso \code{\link{readData,qgsLayer-method}}
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("readData", "qgsLayer", function(object){
	if(object@provider == "ogr"){
		return(qgs_readOGR(object@source))
	}
	else if(object@provider == "WFS"){
		return(qgs_readWMS(object@source))
	}
	else if(object@provider == "gdal"){
		return(qgs_readGDAL(object@source))
	}
	else
		stop(paste("Could not recognize provider ", object@provider, ".", sep = ""))
})

#' @title Plot a QGIS layer
#' @description Read data from a QGIS layer and plot it.
#' @param x A qgsLayer object.
#' @param y Ignored argument.
#' @param ... Argument passed to sp::plot.
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("plot", "qgsLayer", function(x, y, ...) {
	data = qgs::readData(x)
	sp::plot(data, ...)
})

