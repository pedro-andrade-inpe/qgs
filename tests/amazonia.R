require(qgs)

file = system.file("geo/amazonia.qgs", package = "qgs")

proj = openProject(file, replace = TRUE)
proj

limit

prodes

data = readData(prodes)

plot(data) # it is possible to plot the data...
plot(indigenous, border = "gray", add = T) # ... or the layer directly
plot(roads, lwd = 2, add = T)
plot(limit, add = T)
plot(ports, pch=18, col="yellow", add = T)

