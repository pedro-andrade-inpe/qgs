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

qgsProject = setClass("qgsProject", slots = c(file = "character", xml = "list"))

openProject = function(file, replace = FALSE) {
	data = xmlParse(file)
	xml = xmlToList(data)

    proj = new("qgsProject", file = file, xml = xml)

	sapply(proj@xml$projectlayers, function(x){
		value = tryCatch({get(x$layername)}, error = function(cond){
		})

		if(!is.null(value) && !replace)
			stop(paste("Variable", x$layername, "already exists. Set replace = TRUE to overwrite."))

		assign(
			x$layername,
			new("qgsLayer", project = proj, name = x$layername, source = x$datasource),
			envir = .GlobalEnv
		)
	})

	proj
}

setMethod("show", "qgsProject", function(object){
	cat("An object of class qgsProject (QGIS Project)\n")
	cat(paste("File: ", object@file, "\n", sep=""))

	layers = sapply(proj@xml$projectlayers, function(x) x$layername)
	cat("Layers: ", paste(layers, collapse = ", "), "\n", sep = "")
})

setMethod("openLayer", "qgsProject", function(object, name){
	pos = which(sapply(proj@xml$projectlayers, function(x) x$layername) == name)

	if(length(pos) == 0) stop(paste("Could not find layer", name))

	new("qgsLayer", project = object, name = name, source = object@xml$projectlayers[[pos]]$datasource)
})
