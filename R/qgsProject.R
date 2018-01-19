require(XML)
require(rgdal)
require(raster)

#' An S4 class to represent a QGIS project file.
#'
#' @slot file A character with the file.
#' @slot xml The xml content of the file decoded as a list.
qgsProject = setClass("qgsProject", slots = c(file = "character", xml = "list"))

#' Open a given qgs Project
#'
#' Open a given QGIS project stored in a qgs file.
#' It also creates global variables storing the layers of the project.
#' @param file Name of the project file. It can include the complete
#' if the file is not stored in the current directory.
#' @param replace Replace global variables if they already exist?
#' The default value is FALSE.
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
openProject = function(file, replace = FALSE) {
	data = xmlParse(file)
	xml = xmlToList(data)

    proj = new("qgsProject", file = file, xml = xml)
	variables = c()

	sapply(proj@xml$projectlayers, function(x){
		value = tryCatch({get(x$layername)}, error = function(cond){})

		if(is.null(value) || replace)
		{
			name = make.names(x$layername)

			if(name == x$layername)
				variables <<- c(variables, x$layername)
			else
				variables <<- c(variables, paste(name, " (for layer '", x$layername, "')", sep = ""))

			assign(
				x$layername,
				openLayer(proj, x$layername),
				envir = .GlobalEnv
			)
		}
		else
			warning(paste("Variable", x$layername, "already exists and will not be replaced. Set replace = TRUE to overwrite."))
	})

	cat("The following variables were created:", paste(variables, collapse = ", "), "\n")

	proj
}

#' Show the properties of a QGIS project: file and layers.
#' @param object A qgsProject object.
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("show", "qgsProject", function(object){
	cat("An object of class qgsProject (QGIS Project)\n")
	cat(paste("File: \"", object@file, "\"\n", sep=""))

	mylayers = object@xml$projectlayers

	layers = sapply(mylayers, function(x) x$layername)
	cat("Layers: ", paste(paste("\"", layers, "\"", sep = ""), collapse = ", "), "\n", sep = "")
})

#' @title Open a QGIS layer
#' @description Open a given layer within a qgs project.
#' @param object A qgsProject object.
#' @param name Name of the layer to be opened.
#' @rdname openLayer-methods
#' @aliases openLayer openLayer,qgsProject-method
#' @author Pedro R. Andrade, \email{pedro.andrade@inpe.br}
#' @export
setMethod("openLayer", "qgsProject", function(object, name){
	pos = which(sapply(object@xml$projectlayers, function(x) x$layername) == name)

	if(length(pos) == 0) stop(paste("Could not find layer", name))

	source = object@xml$projectlayers[[pos]]$datasource

	if(substr(source, 1, 2) == "./"){
		source = paste(dirname(object@file), substring(source, first = 2), sep = "")
	}

	provider = object@xml$projectlayers[[pos]]$provider
	if(is.list(provider)) provider = provider$text

	new("qgsLayer", project = object, name = name, source = source, provider = provider)
})

