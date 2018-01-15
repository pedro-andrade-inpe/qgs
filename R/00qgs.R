require(XML)
require(rgdal)

.mkGen = function(func, set)
{
    if(!isGeneric(func)) setGeneric(func, set)
    return(invisible())
}

.mkGen("openProject", function(file) standardGeneric("openProject"))
.mkGen("openLayer", function(object, name) standardGeneric("openLayer"))
.mkGen("getData", function(object) standardGeneric("getData"))
.mkGen("plot",     function(x, y, ...)          standardGeneric("plot"))

