require(XML)
require(rgdal)

.onAttach = function(lib, pkg){
	packageStartupMessage(sprintf("QGIS project files package version %s is now loaded.", utils::packageDescription("qgs")$Version))
}

qgs.env <- new.env()

.mkGen = function(func, set)
{
    if(!isGeneric(func)) setGeneric(func, set)
    return(invisible())
}

.mkGen("openLayer", function(object, name) standardGeneric("openLayer"))
.mkGen("openProject", function(file)       standardGeneric("openProject"))
.mkGen("readData", function(object)        standardGeneric("readData"))
.mkGen("readDataCode", function(object)    standardGeneric("readDataCode"))
.mkGen("show", function(object)            standardGeneric("show"))
.mkGen("plot", function(x, y, ...)         standardGeneric("plot"))

#' @importFrom graphics plot
#' @importFrom sp plot
#' @importFrom raster raster
#' @importFrom methods isGeneric new setGeneric show
#' @importFrom stats proj
#' @importFrom rgdal readOGR
#' @importFrom XML xmlParse xmlToList
NULL

