

install_github("AtlanticR/bio.base")
install_github("AtlanticR/bio.utilities")
install_github("AtlanticR/bio.polygons")
install_github("AtlanticR/bio.coastline")
install_github("AtlanticR/bio.spacetime")
install_github("AtlanticR/bio.substrate")
install_github("AtlanticR/bio.taxonomy")
install_github("AtlanticR/lbm")
install_github("AtlanticR/bio.temperature")
install_github("AtlanticR/bio.indicators")
install_github("AtlanticR/bio.groundfish")
install.packages("spacetime")
install.packages("FNN")
install_github("BradHubley/SpatialHub")

library(bio.base)
library(bio.utilities)
library(bio.polygons)
library(bio.coastline)
library(bio.spacetime)
library(bio.substrate)
library(bio.taxonomy)
library(lbm)
library(bio.indicators)
library(bio.groundfish)

install_github("BradHubley/SpatialHub", force=TRUE)
library(SpatialHub)
bioMap("SS")

also planarMap replaces EAmap




