require(qgs)

file = system.file("geo/piemonte.qgs", package = "qgs")

proj = openProject(file, replace = TRUE)
proj

SIC

# the code below might take some time as it
# downloads data from a WFS
plot(AreeProtette, col = "blue") # ... or the layer directly
plot(SIC, col = "red", add = T)
plot(ZPS, col = "green", add = T)

