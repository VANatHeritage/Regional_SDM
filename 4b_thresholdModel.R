# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(ROCR)
library(RSQLite)
library(DBI)
removeTmpFiles(48) # clean old (>2days) Raster temporary files

### find and load model data ----
## two lines need your attention. The one directly below (loc_scripts)
## and about line 23 where you choose which Rdata file to use

#loc_scripts <- "K:/Reg5Modeling_Project/scripts/Regional_SDM"

#source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
       
# get a list of what's in the directory
# d <- dir(path = loc_RDataOut, pattern = ".Rdata",full.names=FALSE)
# d
# # which one do we want to run?
# n <- 1
# fileName <- d[[n]]
# load(paste(loc_RDataOut,fileName, sep="/"))
setwd(loc_RDataOut)
load(paste(modelrun_meta_data$model_run_name,".Rdata", sep=""))

## Calculate different thresholds ----
#set an empty list
cutList <- list()

# total number of EOs (subtract absence class)
totEOs <- length(unique(df.full$eo_id_st)) - 1
# total number of polys
totPolys <- length(unique(df.full$stratum)) - 1

#get minimum training presence
allVotes <- data.frame(rf.full$y, rf.full$votes, df.full[,c("eo_id_st", "stratum")])
allVotesPresPts <- allVotes[allVotes$rf.full.y ==1,]

MTP <- min(allVotesPresPts$X1)
capturedEOs <- length(unique(allVotesPresPts$eo_id_st))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

#get 10 percentile training presence
TenPctile <- quantile(allVotesPresPts$X1, prob = c(0.1))
TenPctilePts <- allVotesPresPts[allVotesPresPts$X1 >= TenPctile,]
capturedEOs <- length(unique(TenPctilePts$eo_id_st))
capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

# get min of max values by polygon (MTPP; minimum training polygon presence)
maxInEachPoly <- aggregate(allVotesPresPts$X1, 
                           by=list(allVotesPresPts$stratum, allVotesPresPts$eo_id_st), max)
names(maxInEachPoly) <- c("stratum","eo_id_st","X1")
MTPP <- min(maxInEachPoly$X1)
capturedEOs <- length(unique(maxInEachPoly$eo_id_st))
capturedPolys <- length(unique(maxInEachPoly$stratum))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$X1 >= MTPP,])
cutList$MTPP <- list("value" = MTPP, "code" = "MTPP", 
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

# get min of max values by EO (MTPEO; minimum training EO presence)
maxInEachEO <- aggregate(allVotesPresPts$X1, 
                           by=list(allVotesPresPts$eo_id_st), max)
names(maxInEachEO) <- c("eo_id_st","X1")
MTPEO <- min(maxInEachEO$X1)
capturedEOs <- length(unique(maxInEachEO$eo_id_st))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$X1 >= MTPEO,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$X1 >= MTPEO,])
cutList$MTPEO <- list("value" = MTPEO, "code" = "MTPEO", 
                     "capturedEOs" = capturedEOs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts)


# F-measure cutoff skewed towards capturing more presence points.
# extract the precision-recall F-measure from training data
# set alpha very low to tip in favor of 'presence' data over 'absence' data
# based on quick assessment in Spring 07, set alpha to 0.01
alph <- 0.01
#create the prediction object for ROCR. Get pres col from votes (=named "1")
rf.full.pred <- prediction(rf.full$votes[,"1"],df.full$pres)
#use ROCR performance to get the f measure
rf.full.f <- performance(rf.full.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
rf.full.f.df <- data.frame(cutoff = unlist(rf.full.f@x.values),fmeasure = unlist(rf.full.f@y.values))
rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(rf.full.ctoff) <- c("0","1")
FMeasPt01 <- rf.full.ctoff[2]
z <- allVotesPresPts[allVotesPresPts$X1 >= FMeasPt01,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedEOs" = capturedEOs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
rf.full.sens <- performance(rf.full.pred,"sens")
rf.full.spec <- performance(rf.full.pred,"spec")
rf.full.sss <- data.frame(cutSens = unlist(rf.full.sens@x.values),sens = unlist(rf.full.sens@y.values),
                          cutSpec = unlist(rf.full.spec@x.values), spec = unlist(rf.full.spec@y.values))
rf.full.sss$sss <- with(rf.full.sss, sens + spec)
maxSSS <- rf.full.sss[which.max(rf.full.sss$sss),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= maxSSS,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
  "capturedEOs" = capturedEOs,
  "capturedPolys" = capturedPolys,
  "capturedPts" = capturedPts)

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= eqss,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                       "capturedEOs" = capturedEOs,
                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts)

# upper left corner of ROC plot
### NO, it looks like these calculations are technically not the upper left corner
### and the upper left corner is different from these others.
### So this needs reworking to represent ROC if it is ever used. Need to use
### pythagorean formula, I think. Liu etal 2005 and Cantor et al 1999 don't seem 
### to provide formula. Aha, see package OptimalCutpoints
# rf.full.perf <- performance(rf.full.pred, "tpr","fpr")
# cutpt <- which.max(abs(rf.full.perf@x.values[[1]]-rf.full.perf@y.values[[1]]))
# ROCupperleft <- rf.full.perf@alpha.values[[1]][cutpt]
# z <- allVotesPresPts[allVotesPresPts$X1 >= ROCupperleft,]
# capturedEOs <- length(unique(z$eo_id_st))
# capturedPolys <- length(unique(z$stratum))
# capturedPts <- nrow(z)
# cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
#                           "capturedEOs" = capturedEOs,
#                           "capturedPolys" = capturedPolys,
#                           "capturedPts" = capturedPts)

# collate and write to DB ----

# number of thresholds to write to the db
numThresh <- length(cutList)

allThresh <- data.frame("modelRunName" = rep(modelrun_meta_data$model_run_name, numThresh),
                        "ElemCode" = rep(ElementNames$Code, numThresh),
                "dateTime" = rep(as.character(Sys.time()), numThresh),
                "cutCode" = unlist(lapply(cutList, function(x) x[2])),
                "cutValue" = unlist(lapply(cutList, function(x) x[1])),
                "capturedEOs" = unlist(lapply(cutList, function(x) x[3])),
                "capturedPolys" = unlist(lapply(cutList, function(x) x[4])),
                "capturedPts" = unlist(lapply(cutList, function(x) x[5])),
                stringsAsFactors = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)


op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

for(i in 1:numThresh){
  SQLquery <- paste("INSERT INTO tblCutoffs (", 
                    toString(names(allThresh)),
                    ") VALUES (",
                    toString(sQuote(allThresh[i,])),
                    ");", sep = "")
  dbSendQuery(db, SQLquery)
}

# clean up
options(op)
dbDisconnect(db)


## create all thresholds grid

t2 <- sort(unique(allThresh$cutValue))
t2 <- t2[!is.na(t2)]
# get unique thresholds
t3 <- data.frame(cutCodes = unlist(lapply(t2, FUN = function(x) {paste(allThresh$cutCode[allThresh$cutValue == x], collapse = ";")})),
                 cutValue = t2,
                 order = 1:length(t2))

# get a list of rasters in the directory for this species
r <- dir(path = loc_outRas, 
         pattern = paste(ElementNames$Code, ".*tif$",sep=""),full.names=FALSE)
rasName <- r[gsub(".tif", "", r) == model_run_name]  # uses model_run_name to specify raster

# load the prediction grid
ras <- raster(paste(loc_outRas, rasName, sep = "/"))

# reclassify the raster based on the threshold into binary 0/1
m <- cbind(
  from = c(-Inf, t3$cutValue),
  to = c(t3$cutValue, Inf),
  becomes = c(0, t3$order)
)
# reclassify (multi-core try)
if (all(c("snow","parallel") %in% installed.packages())) {
  try({
    cat("Using multi-core processing...\n")
    beginCluster(type = "SOCK")
    rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
  })
  try(endCluster())
  if (!exists("rasrc")) {
    cat("Cluster processing failed. Falling back to single-core processing...\n")
    rasrc <- reclassify(ras, m)
  }
} else {
  rasrc <- reclassify(ras, m)
}
rasrc <- as.factor(rasrc)
levels(rasrc) <- merge(levels(rasrc), t3, by.x = "ID", by.y = "order", all.x = T)

outfile <- paste(loc_outRas,"/",model_run_name,"_all_thresholds.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE, datatype = "INT2U")

#clean up
rm(m, rasrc)

## continuous grid that drops cells below lowest calculated thresh ----
# reclassify the raster based on the threshold into Na below thresh
m <- cbind(
  from = c(-Inf),
  to = min(t3$cutValue),
  becomes = c(NA)
)

# reclassify (multi-core try)
if (all(c("snow","parallel") %in% installed.packages())) {
  try({
    cat("Using multi-core processing...\n")
    beginCluster(type = "SOCK")
    rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
  })
  try(endCluster())
  if (!exists("rasrc")) {
    cat("Cluster processing failed. Falling back to single-core processing...\n")
    rasrc <- reclassify(ras, m)
  }
} else {
  rasrc <- reclassify(ras, m)
}

#plot(rasrc)
outfile <- paste(loc_outRas,"/",model_run_name,"_min_thresh_continuous.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)
