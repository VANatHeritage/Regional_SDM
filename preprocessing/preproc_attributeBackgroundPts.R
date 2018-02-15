# File: preproc_attributeBackgroundPts.r
# Purpose: attribute environmental data to random background points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

rm(list = ls())
# path where .tif env. var rasters are stored
pathToRas <- "D:/SDM/Tobacco/env_vars/Tobacco"
# path to background points shapefile
pathToPts <- "D:/SDM/Tobacco/inputs/background/tobacco"
# background points shapefile
ranPtsFile <- "tobacco_RanPts"

## create a stack ----
setwd(pathToRas)

## create a stack. Note this is using native R rasters
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

# temporal groups -> take only max year by variable group
tv <- list.dirs(recursive = FALSE, full.names = FALSE)
if (length(tv) > 1) {
  tv_grp <- as.character(do.call(rbind.data.frame, strsplit(tv,"_",fixed = TRUE))[,1])
  for (t in unique(tv_grp)) {
    stv <- tv[grep(t, tv)]
    nouse <- stv[!stv %in% max(stv[grep(t,stv)])]
    raslist <- raslist[-grep(paste(nouse,collapse="|"),raslist)]
  }
}

gridlist <- as.list(paste(pathToRas,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
nm <- unlist(lapply(strsplit(nm, "/", fixed = TRUE), FUN = function(x) {x[length(x)]}))
names(gridlist) <- nm

envStack <- stack(gridlist)

## Get random points file ----
setwd(pathToPts)

ranPtsFileNoExt <- sub(".shp","",ranPtsFile)
# Read these files into a list of SpatialPoints dataframes
shpf <- readOGR(".", layer = ranPtsFileNoExt)[,"stratum"] # we only want one column
# shpf <- crop(shpf, envStack) # removes unnecssary points outside raster extent
  
# Get a list of the codes (this assumes all the input files had '_RanPts.shp' that shall be stripped)
code_name <- strsplit(ranPtsFile, "_RanPts")[[1]][1]

# do it, write it ----
x <- extract(envStack, shpf, method="simple", sp=TRUE)
x <- x[complete.cases(x@data[,-1]),] # subsets to those points which do not occur in nodata regions of any of the rasters
filename <- paste(code_name, "_att_shore", sep="")
writeOGR(x, pathToPts, layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before using another script
rm(list=ls())
