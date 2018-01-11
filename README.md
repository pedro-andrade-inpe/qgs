# qgs
R Package to open QGIS project files (.qgs). For example:

```R
import(qgs)

proj = openProject("myproject.qgs")
roads = openLayer(proj, "roads")

plot(roads)
