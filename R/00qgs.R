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

.mkGen("openProject", function(file) standardGeneric("openProject"))
.mkGen("openLayer", function(object, name) standardGeneric("openLayer"))
.mkGen("getData", function(object) standardGeneric("getData"))
.mkGen("plot",     function(x, y, ...)          standardGeneric("plot"))

#' @importFrom graphics plot
#' @importFrom methods isGeneric new setGeneric
#' @importFrom stats proj
#' @importFrom rgdal readOGR
#' @importFrom XML xmlParse xmlToList
NULL

