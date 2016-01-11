## github
#more information about the contest https://www.ideaconnection.com/syngenta-crop-challenge/

##---- 0. Libraries ----

# --- clear all objects
rm(list = ls()); gc();

# ---- DECISIONS
re <-TRUE
keep.soil.k <-FALSE
take.median <- TRUE
consider.dist <- TRUE

# --- load necessary packages
library(openxlsx)
library(ggmap)
library(rpart)
library(rpart.plot)
library(dplyr)
library(reshape2)
library(cluster)
library(fpc)
library(emoa)
set.seed(1)

##---- 1. IMPORT THE DATA ----

#read the data in xlsx file.
indir <- "/Users/xavierign/Documents/Syngenta/Crop_Challenge_Data_Set/"

file1 <- "Training dataset.xlsx"
infile1 <- paste(indir,file1, sep="")

trainDS <- read.xlsx(infile1,sheet=1,
                               colNames=T,
                                startRow = 1)

file2 <- "Evaluation dataset.xlsx"
infile2 <- paste(indir,file2, sep="")

evalDS <- read.xlsx(infile2,sheet=1,
                     colNames=T,
                     startRow = 1)

file3 <- "Training daily weather dataset.xlsx"
infile3 <- paste(indir,file3, sep="")

trainDDS <- read.xlsx(infile3,sheet=1,
                    colNames=T,
                    startRow = 18)

file4 <- "Evaluation daily weather dataset.xlsx"
infile4 <- paste(indir,file4, sep="")

evalDDS <- read.xlsx(infile4,sheet=1,
                      colNames=T,
                      startRow = 24)

file5 <- "Training and Evaluation data key.xlsx"
infile5 <- paste(indir,file5, sep="")

trainevalDK <- read.xlsx(infile5,sheet=1,
                     colNames=T,
                     startRow = 1)

##---- 2. CONVERT AND ANALYZE THE DATA----

#remove duplicated records
trainDS <- trainDS[!duplicated(trainDS),]

#convert SITE to factor
trainDS$SITE <- as.factor(trainDS$SITE)
#118 sites.

#convert VARIETY to factor
trainDS$VARIETY <- as.factor(trainDS$VARIETY)
#182 varieties

#convert RM_BAND BREEDING_G to factor
trainDS$RM_BAND<- as.factor(trainDS$RM_BAND)
trainDS$BREEDING_G<- as.factor(trainDS$BREEDING_G)
trainDS$SOIL_CUBE<- as.factor(trainDS$SOIL_CUBE)
trainDS$SEASON1<- as.factor(trainDS$SEASON1)

#decompose CLIMATE in 3
trainDS$CLIMATE1<- as.numeric(substring(trainDS$CLIMATE,1,1))
trainDS$CLIMATE2<- as.numeric(substring(trainDS$CLIMATE,2,2))
trainDS$CLIMATE3<- as.numeric(substring(trainDS$CLIMATE,3,3))

#CLIMATE 2 AND CLIMATE 3 HAVE MISSING VALES
trainDS$CLIMATE2[is.na(trainDS$CLIMATE2)] <- 0
trainDS$CLIMATE3[is.na(trainDS$CLIMATE3)] <- 0

#decompose SOIL_CUBE in 3
trainDS$SOIL_CUBE1<- as.numeric(substring(trainDS$SOIL_CUBE,1,1))
trainDS$SOIL_CUBE2<- as.numeric(substring(trainDS$SOIL_CUBE,2,2))
trainDS$SOIL_CUBE3<- as.numeric(substring(trainDS$SOIL_CUBE,3,3))

#convert RM_BAND into a number
RM_BAND_N <- rep(1,length(trainDS$RM_BAND))
RM_BAND_N[trainDS$RM_BAND == '1.50-1.99'] <- 1.75
RM_BAND_N[trainDS$RM_BAND == '2.00-2.49'] <- 2.25
RM_BAND_N[trainDS$RM_BAND == '2.50-2.99'] <- 2.75
RM_BAND_N[trainDS$RM_BAND == '3.00-3.49'] <- 3.25

trainDS$RM_BAND_N <- RM_BAND_N

trainDS$CONUS_PH[trainDS$CONUS_PH< -100] <- NA

#same for eval
#convert RM_BAND BREEDING_G to factor
evalDS$RM_BAND<- as.factor(evalDS$RM_BAND)
evalDS$SOIL_CUBE<- as.factor(evalDS$SOIL_CUBE)

#decompose CLIMATE in 3
evalDS$CLIMATE1<- as.numeric(substring(evalDS$CLIMATE,1,1))
evalDS$CLIMATE2<- as.numeric(substring(evalDS$CLIMATE,2,2))
evalDS$CLIMATE3<- as.numeric(substring(evalDS$CLIMATE,3,3))

#CLIMATE 2 AND CLIMATE 3 HAVE MISSING VALES
evalDS$CLIMATE2[is.na(evalDS$CLIMATE2)] <- 0
evalDS$CLIMATE3[is.na(evalDS$CLIMATE3)] <- 0

#decompose SOIL_CUBE in 3
evalDS$SOIL_CUBE1<- as.numeric(substring(evalDS$SOIL_CUBE,1,1))
evalDS$SOIL_CUBE2<- as.numeric(substring(evalDS$SOIL_CUBE,2,2))
evalDS$SOIL_CUBE3<- as.numeric(substring(evalDS$SOIL_CUBE,3,3))

#convert RM_BAND into a number
RM_BAND_N <- rep(1,length(evalDS$RM_BAND))
RM_BAND_N[evalDS$RM_BAND == '1.50-1.99'] <- 1.75
RM_BAND_N[evalDS$RM_BAND == '2.00-2.49'] <- 2.25
RM_BAND_N[evalDS$RM_BAND == '2.50-2.99'] <- 2.75
RM_BAND_N[evalDS$RM_BAND == '3.00-3.49'] <- 3.25

evalDS$RM_BAND_N <- RM_BAND_N

##---- 2. RESAMPLE SCENARIOS REPEATED----

if (take.median == TRUE){
  #reorder
  tab <- aggregate(trainDS$VARIETY_YI,
                   list(trainDS$VARIETY, trainDS$SITE,trainDS$SEASON), median)
  #reorder both matrices
  colnames(tab) <- c( 'VARIETY','SITE','SEASON','VARIETY_YI')
  tab <- tab[order(tab[,'VARIETY'],tab[,'SITE'],
                   tab[,'SEASON'],decreasing=TRUE),]
}
if (re == TRUE){
  #reorder
  trainDS <- trainDS[sample(nrow(trainDS)),]
  r.to.keep <- !duplicated(trainDS[,c('SITE','SEASON','VARIETY')])
  trainDS <- trainDS[r.to.keep,]
}
if (take.median == TRUE){
  #reorder
  trainDS <- trainDS[order(trainDS[,'VARIETY'],trainDS[,'SITE'],
                           trainDS[,'SEASON'],decreasing=TRUE),]
  trainDS$VARIETY_YI <- tab$VARIETY_YI
}

##---- 3. CLUSTER ANALYSIS SOIL----
predictors.soil <- c("LAT","LONG_",
                     "AREA","RM_25","TOT_IRR_DE",
                     "SY_DENS","SY_ACRES","CONUS_PH","CONUS_AWC","CONUS_CLAY","CONUS_SILT",
                     "CONUS_SAND","ISRIC_SAND","ISRIC_SILT","ISRIC_CLAY","ISRIC_PH","ISRIC_CEC",
                     "EXTRACT_CE","SOIL_CUBE1","SOIL_CUBE2","SOIL_CUBE3","RM_BAND_N")

soil.data <- trainDS[!duplicated(trainDS$SITE),predictors.soil]
soil.data.sites <- trainDS$SITE[!duplicated(trainDS$SITE)]
row.names(soil.data) <- soil.data.sites
soil.data <- rbind.data.frame(soil.data, evalDS[,predictors.soil] )

#assign the missing value of conusph
fit.conusph <- lm(CONUS_PH ~TOT_IRR_DE+SY_DENS+SY_ACRES+CONUS_AWC+CONUS_CLAY+
                  CONUS_SILT+CONUS_SAND+ISRIC_SAND+ISRIC_SILT+ISRIC_CLAY+ISRIC_PH+ISRIC_CEC+
                  EXTRACT_CE+RM_BAND_N, data=soil.data) 
ph.value <- predict(fit.conusph, newdata=soil.data[is.na(soil.data$CONUS_PH),])

#assign the value
trainDS$CONUS_PH[is.na(trainDS$CONUS_PH)] <- ph.value
soil.data$CONUS_PH[is.na(soil.data$CONUS_PH)] <- ph.value

#scale the data
soil.data <- scale(soil.data)

fit.clus <- kmeans(soil.data, 3)

#include the cluster in the dataset
cluster.eval <- fit.clus$cluster[length(fit.clus$cluster)]

#append the cluster column into the dataset
site.cluster <- cbind.data.frame(SITE = soil.data.sites, 
                                 CLUSTER = fit.clus$cluster[-length(fit.clus$cluster)])

#append site.cluster to trainDS.
trainDS <- merge(trainDS, site.cluster, by = 'SITE')

#distance between observations

#weigthed distances
w <- c(1,1,1.5,1.5,1.5,1,1,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,1,1,1,1,1.5)
weig.dist <- as.matrix(daisy(soil.data, weights=w, stand=T))
weig.dist <- sort(weig.dist[nrow(weig.dist),])
plot(weig.dist)
threshold <- 5
abline(h=threshold)

#most similar sites #3230, #22NU, #2202, #2270, #2255

##---- 4. VISUALIZE THE DATA----
#visualize the kmeans
plotcluster(soil.data, fit.clus$cluster)
fit.clus$cluster

#geoplot the sites
lat_m <- sum(range(trainDS$LAT))/2
lon_m <- sum(range(trainDS$LONG_))/2

myLocation <- c(lon = lon_m, lat = lat_m)

#myMap <- get_map(location=myLocation,
#                 source="google", maptype="terrain", crop=FALSE, zoom=5)
#ggmap(myMap) + 
#  geom_point(aes(x = LONG_, y = LAT), data = trainDS,
 #            alpha = .4, color=trainDS$CLUSTER, size = 1) +
#  geom_point(aes(x = LONG_, y = LAT), data = evalDS,
#             alpha = .9, color="blue", size = 1)

##---- 5. DECISION KEEP ONLY THE SOIL CLUSTER----
if (keep.soil.k ==T){
  #consider just the same cluster! YES or NO?
  trainDS <- trainDS[trainDS$CLUSTER==cluster.eval,]
}

if (consider.dist ==T){
  sites.tk <- names(weig.dist[weig.dist<threshold])
  print(length(sites.tk)/length (unique(trainDS$SITE)))
  trainDS <- trainDS[trainDS$SITE %in% sites.tk,]

}

##---- 6. AGREGATION OF WEATHER DATA----
#define the temrs 1, 2 and 3

start0 <- 61
start1 <- 119
start2 <- 149
end3 <- 275

start3 <- 209

#define the booleans t1 t2 t3

t0 <- trainDDS$yday>=start0&trainDDS$yday<start1
t1 <- trainDDS$yday>=start1&trainDDS$yday<start2
t2 <- trainDDS$yday>=start2&trainDDS$yday<end3

#these are not used:
t3 <- trainDDS$yday>=start3&trainDDS$yday<=end3
ta <- trainDDS$yday>=1&trainDDS$yday<=365

#column of years
agg.data <- unique(trainDDS[,c('year','Site','Scenario')])
colnames (agg.data) <- c('SEASON','SITE','SCENARIO')

#dayl
dayl.t0.me <- tapply (trainDDS$dayl[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,dayl.t0.me)

dayl.t1.me <- tapply (trainDDS$dayl[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,dayl.t1.me)

dayl.t2.me <- tapply (trainDDS$dayl[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,dayl.t2.me)

dayl.t3.me <- tapply (trainDDS$dayl[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,dayl.t3.me)

dayl.ta.me <- tapply (trainDDS$dayl[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,dayl.ta.me)

#prcp
prcp.t0.me <- tapply (trainDDS$prcp[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,prcp.t0.me)

prcp.t1.me <- tapply (trainDDS$prcp[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,prcp.t1.me)

prcp.t2.me <- tapply (trainDDS$prcp[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,prcp.t2.me)

prcp.t3.me <- tapply (trainDDS$prcp[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,prcp.t3.me)

prcp.ta.me <- tapply (trainDDS$prcp[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,prcp.ta.me)

#dwr
#keep the original index
dwr <- rep (NA,length(trainDDS$prcp))
dwr[trainDDS$yday==1]  <- 0

for (i in 1:length(dwr)){
  if(is.na(dwr[i])){
    if (trainDDS$prcp[i]==0){
      dwr[i]<- dwr[i-1]+1
    } else {
      dwr[i]<- 0
    }
  }
}

#append dwr to the dataframe
trainDDS$dwr <- dwr

#dwr-mean
dwr.t0.me <- tapply (trainDDS$dwr[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,dwr.t0.me)

dwr.t1.me <- tapply (trainDDS$dwr[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,dwr.t1.me)

dwr.t2.me <- tapply (trainDDS$dwr[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,dwr.t2.me)

dwr.t3.me <- tapply (trainDDS$dwr[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,dwr.t3.me)

dwr.ta.me <- tapply (trainDDS$dwr[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,dwr.ta.me)

#dwr-ma
dwr.t0.ma <- tapply (trainDDS$dwr[t0],INDEX=trainDDS$Scenario[t0],FUN=max)
agg.data <- cbind(agg.data,dwr.t0.ma)

dwr.t1.ma <- tapply (trainDDS$dwr[t1],INDEX=trainDDS$Scenario[t1],FUN=max)
agg.data <- cbind(agg.data,dwr.t1.ma)

dwr.t2.ma <- tapply (trainDDS$dwr[t2],INDEX=trainDDS$Scenario[t2],FUN=max)
agg.data <- cbind(agg.data,dwr.t2.ma)

dwr.t3.ma <- tapply (trainDDS$dwr[t3],INDEX=trainDDS$Scenario[t3],FUN=max)
agg.data <- cbind(agg.data,dwr.t3.ma)

dwr.ta.ma <- tapply (trainDDS$dwr[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
agg.data <- cbind(agg.data,dwr.ta.ma)

#srad
srad.t0.me <- tapply (trainDDS$srad[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,srad.t0.me)

srad.t1.me <- tapply (trainDDS$srad[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,srad.t1.me)

srad.t2.me <- tapply (trainDDS$srad[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,srad.t2.me)

srad.t3.me <- tapply (trainDDS$srad[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,srad.t3.me)

srad.ta.me <- tapply (trainDDS$srad[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,srad.ta.me)

#swe
swe.t0.me <- tapply (trainDDS$swe[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,swe.t0.me)

swe.t1.me <- tapply (trainDDS$swe[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,swe.t1.me)

#no snow in t2 and t3
#swe.t2.me <- tapply (trainDDS$swe[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
#agg.data <- cbind(agg.data,swe.t2.me)

#swe.t3.me <- tapply (trainDDS$swe[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
#agg.data <- cbind(agg.data,swe.t3.me)

#swe.ta.me <- tapply (trainDDS$swe[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
#agg.data <- cbind(agg.data,swe.ta.me)

#tmax-ave
tmax.t0.me <- tapply (trainDDS$tmax[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,tmax.t0.me)

tmax.t1.me <- tapply (trainDDS$tmax[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,tmax.t1.me)

tmax.t2.me <- tapply (trainDDS$tmax[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,tmax.t2.me)

tmax.t3.me <- tapply (trainDDS$tmax[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,tmax.t3.me)

tmax.ta.me <- tapply (trainDDS$tmax[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,tmax.ta.me)

#tmax-max
tmax.t0.ma <- tapply (trainDDS$tmax[t0],INDEX=trainDDS$Scenario[t0],FUN=max)
agg.data <- cbind(agg.data,tmax.t0.ma)

tmax.t1.ma <- tapply (trainDDS$tmax[t1],INDEX=trainDDS$Scenario[t1],FUN=max)
agg.data <- cbind(agg.data,tmax.t1.ma)

tmax.t2.ma <- tapply (trainDDS$tmax[t2],INDEX=trainDDS$Scenario[t2],FUN=max)
agg.data <- cbind(agg.data,tmax.t2.ma)

tmax.t3.ma <- tapply (trainDDS$tmax[t3],INDEX=trainDDS$Scenario[t3],FUN=max)
agg.data <- cbind(agg.data,tmax.t3.ma)

tmax.ta.ma <- tapply (trainDDS$tmax[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
agg.data <- cbind(agg.data,tmax.ta.ma)

#tmax-min
tmax.t0.mi <- tapply (trainDDS$tmax[t0],INDEX=trainDDS$Scenario[t0],FUN=min)
agg.data <- cbind(agg.data,tmax.t0.mi)

tmax.t1.mi <- tapply (trainDDS$tmax[t1],INDEX=trainDDS$Scenario[t1],FUN=min)
agg.data <- cbind(agg.data,tmax.t1.mi)

tmax.t2.mi <- tapply (trainDDS$tmax[t2],INDEX=trainDDS$Scenario[t2],FUN=min)
agg.data <- cbind(agg.data,tmax.t2.mi)

tmax.t3.mi <- tapply (trainDDS$tmax[t3],INDEX=trainDDS$Scenario[t3],FUN=min)
agg.data <- cbind(agg.data,tmax.t3.mi)

tmax.ta.mi <- tapply (trainDDS$tmax[ta],INDEX=trainDDS$Scenario[ta],FUN=min)
agg.data <- cbind(agg.data,tmax.ta.mi)

#tmin-ave
tmin.t0.me <- tapply (trainDDS$tmin[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,tmin.t0.me)

tmin.t1.me <- tapply (trainDDS$tmin[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,tmin.t1.me)

tmin.t2.me <- tapply (trainDDS$tmin[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,tmin.t2.me)

tmin.t3.me <- tapply (trainDDS$tmin[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,tmin.t3.me)

tmin.ta.me <- tapply (trainDDS$tmin[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,tmin.ta.me)

#tmin-max
tmin.t0.ma <- tapply (trainDDS$tmin[t0],INDEX=trainDDS$Scenario[t0],FUN=max)
agg.data <- cbind(agg.data,tmin.t0.ma)

tmin.t1.ma <- tapply (trainDDS$tmin[t1],INDEX=trainDDS$Scenario[t1],FUN=max)
agg.data <- cbind(agg.data,tmin.t1.ma)

tmin.t2.ma <- tapply (trainDDS$tmin[t2],INDEX=trainDDS$Scenario[t2],FUN=max)
agg.data <- cbind(agg.data,tmin.t2.ma)

tmin.t3.ma <- tapply (trainDDS$tmin[t3],INDEX=trainDDS$Scenario[t3],FUN=max)
agg.data <- cbind(agg.data,tmin.t3.ma)

tmin.ta.ma <- tapply (trainDDS$tmin[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
agg.data <- cbind(agg.data,tmin.ta.ma)

#tmin-min
tmin.t0.mi <- tapply (trainDDS$tmin[t0],INDEX=trainDDS$Scenario[t0],FUN=min)
agg.data <- cbind(agg.data,tmin.t0.mi)

tmin.t1.mi <- tapply (trainDDS$tmin[t1],INDEX=trainDDS$Scenario[t1],FUN=min)
agg.data <- cbind(agg.data,tmin.t1.mi)

tmin.t2.mi <- tapply (trainDDS$tmin[t2],INDEX=trainDDS$Scenario[t2],FUN=min)
agg.data <- cbind(agg.data,tmin.t2.mi)

tmin.t3.mi <- tapply (trainDDS$tmin[t3],INDEX=trainDDS$Scenario[t3],FUN=min)
agg.data <- cbind(agg.data,tmin.t3.mi)

tmin.ta.mi <- tapply (trainDDS$tmin[ta],INDEX=trainDDS$Scenario[ta],FUN=min)
agg.data <- cbind(agg.data,tmin.ta.mi)

#trange 
#define trange
trainDDS$trange <- trainDDS$tmax -trainDDS$tmin

#trange-ave
trange.t0.me <- tapply (trainDDS$trange[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,trange.t0.me)

trange.t1.me <- tapply (trainDDS$trange[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,trange.t1.me)

trange.t2.me <- tapply (trainDDS$trange[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,trange.t2.me)

trange.t3.me <- tapply (trainDDS$trange[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,trange.t3.me)

trange.ta.me <- tapply (trainDDS$trange[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,trange.ta.me)

#trange-max
trange.t0.ma <- tapply (trainDDS$trange[t0],INDEX=trainDDS$Scenario[t0],FUN=max)
agg.data <- cbind(agg.data,trange.t0.ma)

trange.t1.ma <- tapply (trainDDS$trange[t1],INDEX=trainDDS$Scenario[t1],FUN=max)
agg.data <- cbind(agg.data,trange.t1.ma)

trange.t2.ma <- tapply (trainDDS$trange[t2],INDEX=trainDDS$Scenario[t2],FUN=max)
agg.data <- cbind(agg.data,trange.t2.ma)

trange.t3.ma <- tapply (trainDDS$trange[t3],INDEX=trainDDS$Scenario[t3],FUN=max)
agg.data <- cbind(agg.data,trange.t3.ma)

trange.ta.ma <- tapply (trainDDS$trange[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
agg.data <- cbind(agg.data,trange.ta.ma)

#trange-min
trange.t0.mi <- tapply (trainDDS$trange[t0],INDEX=trainDDS$Scenario[t0],FUN=min)
agg.data <- cbind(agg.data,trange.t0.mi)

trange.t1.mi <- tapply (trainDDS$trange[t1],INDEX=trainDDS$Scenario[t1],FUN=min)
agg.data <- cbind(agg.data,trange.t1.mi)

trange.t2.mi <- tapply (trainDDS$trange[t2],INDEX=trainDDS$Scenario[t2],FUN=min)
agg.data <- cbind(agg.data,trange.t2.mi)

trange.t3.mi <- tapply (trainDDS$trange[t3],INDEX=trainDDS$Scenario[t3],FUN=min)
agg.data <- cbind(agg.data,trange.t3.mi)

trange.ta.mi <- tapply (trainDDS$trange[ta],INDEX=trainDDS$Scenario[ta],FUN=min)
agg.data <- cbind(agg.data,trange.ta.mi)

#vp-ave
vp.t0.me <- tapply (trainDDS$vp[t0],INDEX=trainDDS$Scenario[t0],FUN=mean)
agg.data <- cbind(agg.data,vp.t0.me)

vp.t1.me <- tapply (trainDDS$vp[t1],INDEX=trainDDS$Scenario[t1],FUN=mean)
agg.data <- cbind(agg.data,vp.t1.me)

vp.t2.me <- tapply (trainDDS$vp[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,vp.t2.me)

vp.t3.me <- tapply (trainDDS$vp[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,vp.t3.me)

vp.ta.me <- tapply (trainDDS$vp[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,vp.ta.me)

#vp-max
vp.t0.ma <- tapply (trainDDS$vp[t0],INDEX=trainDDS$Scenario[t0],FUN=max)
agg.data <- cbind(agg.data,vp.t0.ma)

vp.t1.ma <- tapply (trainDDS$vp[t1],INDEX=trainDDS$Scenario[t1],FUN=max)
agg.data <- cbind(agg.data,vp.t1.ma)

vp.t2.ma <- tapply (trainDDS$vp[t2],INDEX=trainDDS$Scenario[t2],FUN=max)
agg.data <- cbind(agg.data,vp.t2.ma)

vp.t3.ma <- tapply (trainDDS$vp[t3],INDEX=trainDDS$Scenario[t3],FUN=max)
agg.data <- cbind(agg.data,vp.t3.ma)

#colinieality
#vp.ta.ma <- tapply (trainDDS$vp[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
#agg.data <- cbind(agg.data,vp.ta.ma)

#vp-min
vp.t0.mi <- tapply (trainDDS$vp[t0],INDEX=trainDDS$Scenario[t0],FUN=min)
agg.data <- cbind(agg.data,vp.t0.mi)

vp.t1.mi <- tapply (trainDDS$vp[t1],INDEX=trainDDS$Scenario[t1],FUN=min)
agg.data <- cbind(agg.data,vp.t1.mi)

vp.t2.mi <- tapply (trainDDS$vp[t2],INDEX=trainDDS$Scenario[t2],FUN=min)
agg.data <- cbind(agg.data,vp.t2.mi)

vp.t3.mi <- tapply (trainDDS$vp[t3],INDEX=trainDDS$Scenario[t3],FUN=min)
agg.data <- cbind(agg.data,vp.t3.mi)

vp.ta.mi <- tapply (trainDDS$vp[ta],INDEX=trainDDS$Scenario[ta],FUN=min)
agg.data <- cbind(agg.data,vp.ta.mi)

#left join.
trainDS <- merge(trainDS, agg.data, by = c('SITE','SEASON'))

# #consider varialbes TEMPXX, PRECXX, RADXX
# trainDS$TEMP_CUR <- rep(NA,length(trainDS$SITE))
# trainDS$PREC_CUR <- rep(NA,length(trainDS$SITE))
# trainDS$RAD_CUR <- rep(NA,length(trainDS$SITE))
# 
# #08
# trainDS$TEMP_CUR[trainDS$SEASON==2008]<- trainDS$TEMP_08[trainDS$SEASON==2008]
# trainDS$PREC_CUR[trainDS$SEASON==2008]<- trainDS$PREC_08[trainDS$SEASON==2008]
# trainDS$RAD_CUR[trainDS$SEASON==2008]<- trainDS$RAD_08[trainDS$SEASON==2008]
# #09
# trainDS$TEMP_CUR[trainDS$SEASON==2009]<- trainDS$TEMP_09[trainDS$SEASON==2009]
# trainDS$PREC_CUR[trainDS$SEASON==2009]<- trainDS$PREC_09[trainDS$SEASON==2009]
# trainDS$RAD_CUR[trainDS$SEASON==2009]<- trainDS$RAD_09[trainDS$SEASON==2009]
# #10
# trainDS$TEMP_CUR[trainDS$SEASON==2010]<- trainDS$TEMP_10[trainDS$SEASON==2010]
# trainDS$PREC_CUR[trainDS$SEASON==2010]<- trainDS$PREC_10[trainDS$SEASON==2010]
# trainDS$RAD_CUR[trainDS$SEASON==2010]<- trainDS$RAD_10[trainDS$SEASON==2010]
# #11
# trainDS$TEMP_CUR[trainDS$SEASON==2011]<- trainDS$TEMP_11[trainDS$SEASON==2011]
# trainDS$PREC_CUR[trainDS$SEASON==2011]<- trainDS$PREC_11[trainDS$SEASON==2011]
# trainDS$RAD_CUR[trainDS$SEASON==2011]<- trainDS$RAD_11[trainDS$SEASON==2011]
# #12
# trainDS$TEMP_CUR[trainDS$SEASON==2012]<- trainDS$TEMP_12[trainDS$SEASON==2012]
# trainDS$PREC_CUR[trainDS$SEASON==2012]<- trainDS$PREC_12[trainDS$SEASON==2012]
# trainDS$RAD_CUR[trainDS$SEASON==2012]<- trainDS$RAD_12[trainDS$SEASON==2012]
# #13
# trainDS$TEMP_CUR[trainDS$SEASON==2013]<- trainDS$TEMP_13[trainDS$SEASON==2013]
# trainDS$PREC_CUR[trainDS$SEASON==2013]<- trainDS$PREC_13[trainDS$SEASON==2013]
# trainDS$RAD_CUR[trainDS$SEASON==2013]<- trainDS$RAD_13[trainDS$SEASON==2013]
# #14
# trainDS$TEMP_CUR[trainDS$SEASON==2014]<- trainDS$TEMP_14[trainDS$SEASON==2014]
# trainDS$PREC_CUR[trainDS$SEASON==2014]<- trainDS$PREC_14[trainDS$SEASON==2014]
# trainDS$RAD_CUR[trainDS$SEASON==2014]<- trainDS$RAD_14[trainDS$SEASON==2014]

##---- 13. AGREGATION OF DAILY DATA EVAL----
#define the booleans t1 t2 t3

t0 <- evalDDS$yday>=start0&evalDDS$yday<start1
t1 <- evalDDS$yday>=start1&evalDDS$yday<start2
t2 <- evalDDS$yday>=start2&evalDDS$yday<end3

#these are not used:
t3 <- evalDDS$yday>=start3&evalDDS$yday<=end3
ta <- evalDDS$yday>=1&evalDDS$yday<=365

#column of years
agg.data <- unique(evalDDS[,c('year')])
agg.data <- data.frame(SEASON=agg.data)


#dayl
dayl.t0.me <- tapply (evalDDS$dayl[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,dayl.t0.me)

dayl.t1.me <- tapply (evalDDS$dayl[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,dayl.t1.me)

dayl.t2.me <- tapply (evalDDS$dayl[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,dayl.t2.me)

dayl.t3.me <- tapply (evalDDS$dayl[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,dayl.t3.me)

dayl.ta.me <- tapply (evalDDS$dayl[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,dayl.ta.me)

#prcp
prcp.t0.me <- tapply (evalDDS$prcp[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,prcp.t0.me)

prcp.t1.me <- tapply (evalDDS$prcp[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,prcp.t1.me)

prcp.t2.me <- tapply (evalDDS$prcp[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,prcp.t2.me)

prcp.t3.me <- tapply (evalDDS$prcp[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,prcp.t3.me)

prcp.ta.me <- tapply (evalDDS$prcp[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,prcp.ta.me)

#dwr
#keep the original index
dwr <- rep (NA,length(evalDDS$prcp))
dwr[evalDDS$yday==1]  <- 0

for (i in 1:length(dwr)){
  if(is.na(dwr[i])){
    if (evalDDS$prcp[i]==0){
      dwr[i]<- dwr[i-1]+1
    } else {
      dwr[i]<- 0
    }
  }
}

#append dwr to the dataframe
evalDDS$dwr <- dwr

#dwr-mean
dwr.t0.me <- tapply (evalDDS$dwr[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,dwr.t0.me)

dwr.t1.me <- tapply (evalDDS$dwr[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,dwr.t1.me)

dwr.t2.me <- tapply (evalDDS$dwr[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,dwr.t2.me)

dwr.t3.me <- tapply (evalDDS$dwr[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,dwr.t3.me)

dwr.ta.me <- tapply (evalDDS$dwr[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,dwr.ta.me)

#dwr-ma
dwr.t0.ma <- tapply (evalDDS$dwr[t0],INDEX=evalDDS$year[t0],FUN=max)
agg.data <- cbind(agg.data,dwr.t0.ma)

dwr.t1.ma <- tapply (evalDDS$dwr[t1],INDEX=evalDDS$year[t1],FUN=max)
agg.data <- cbind(agg.data,dwr.t1.ma)

dwr.t2.ma <- tapply (evalDDS$dwr[t2],INDEX=evalDDS$year[t2],FUN=max)
agg.data <- cbind(agg.data,dwr.t2.ma)

dwr.t3.ma <- tapply (evalDDS$dwr[t3],INDEX=evalDDS$year[t3],FUN=max)
agg.data <- cbind(agg.data,dwr.t3.ma)

dwr.ta.ma <- tapply (evalDDS$dwr[ta],INDEX=evalDDS$year[ta],FUN=max)
agg.data <- cbind(agg.data,dwr.ta.ma)

#srad
srad.t0.me <- tapply (evalDDS$srad[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,srad.t0.me)

srad.t1.me <- tapply (evalDDS$srad[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,srad.t1.me)

srad.t2.me <- tapply (evalDDS$srad[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,srad.t2.me)

srad.t3.me <- tapply (evalDDS$srad[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,srad.t3.me)

srad.ta.me <- tapply (evalDDS$srad[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,srad.ta.me)

#swe
swe.t0.me <- tapply (evalDDS$swe[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,swe.t0.me)

swe.t1.me <- tapply (evalDDS$swe[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,swe.t1.me)

#no snow in t2 and t3
#swe.t2.me <- tapply (evalDDS$swe[t2],INDEX=evalDDS$year[t2],FUN=mean)
#agg.data <- cbind(agg.data,swe.t2.me)

#swe.t3.me <- tapply (evalDDS$swe[t3],INDEX=evalDDS$year[t3],FUN=mean)
#agg.data <- cbind(agg.data,swe.t3.me)

#swe.ta.me <- tapply (evalDDS$swe[ta],INDEX=evalDDS$year[ta],FUN=mean)
#agg.data <- cbind(agg.data,swe.ta.me)

#tmax-ave
tmax.t0.me <- tapply (evalDDS$tmax[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,tmax.t0.me)

tmax.t1.me <- tapply (evalDDS$tmax[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,tmax.t1.me)

tmax.t2.me <- tapply (evalDDS$tmax[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,tmax.t2.me)

tmax.t3.me <- tapply (evalDDS$tmax[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,tmax.t3.me)

tmax.ta.me <- tapply (evalDDS$tmax[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,tmax.ta.me)

#tmax-max
tmax.t0.ma <- tapply (evalDDS$tmax[t0],INDEX=evalDDS$year[t0],FUN=max)
agg.data <- cbind(agg.data,tmax.t0.ma)

tmax.t1.ma <- tapply (evalDDS$tmax[t1],INDEX=evalDDS$year[t1],FUN=max)
agg.data <- cbind(agg.data,tmax.t1.ma)

tmax.t2.ma <- tapply (evalDDS$tmax[t2],INDEX=evalDDS$year[t2],FUN=max)
agg.data <- cbind(agg.data,tmax.t2.ma)

tmax.t3.ma <- tapply (evalDDS$tmax[t3],INDEX=evalDDS$year[t3],FUN=max)
agg.data <- cbind(agg.data,tmax.t3.ma)

tmax.ta.ma <- tapply (evalDDS$tmax[ta],INDEX=evalDDS$year[ta],FUN=max)
agg.data <- cbind(agg.data,tmax.ta.ma)

#tmax-min
tmax.t0.mi <- tapply (evalDDS$tmax[t0],INDEX=evalDDS$year[t0],FUN=min)
agg.data <- cbind(agg.data,tmax.t0.mi)

tmax.t1.mi <- tapply (evalDDS$tmax[t1],INDEX=evalDDS$year[t1],FUN=min)
agg.data <- cbind(agg.data,tmax.t1.mi)

tmax.t2.mi <- tapply (evalDDS$tmax[t2],INDEX=evalDDS$year[t2],FUN=min)
agg.data <- cbind(agg.data,tmax.t2.mi)

tmax.t3.mi <- tapply (evalDDS$tmax[t3],INDEX=evalDDS$year[t3],FUN=min)
agg.data <- cbind(agg.data,tmax.t3.mi)

tmax.ta.mi <- tapply (evalDDS$tmax[ta],INDEX=evalDDS$year[ta],FUN=min)
agg.data <- cbind(agg.data,tmax.ta.mi)

#tmin-ave
tmin.t0.me <- tapply (evalDDS$tmin[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,tmin.t0.me)

tmin.t1.me <- tapply (evalDDS$tmin[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,tmin.t1.me)

tmin.t2.me <- tapply (evalDDS$tmin[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,tmin.t2.me)

tmin.t3.me <- tapply (evalDDS$tmin[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,tmin.t3.me)

tmin.ta.me <- tapply (evalDDS$tmin[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,tmin.ta.me)

#tmin-max
tmin.t0.ma <- tapply (evalDDS$tmin[t0],INDEX=evalDDS$year[t0],FUN=max)
agg.data <- cbind(agg.data,tmin.t0.ma)

tmin.t1.ma <- tapply (evalDDS$tmin[t1],INDEX=evalDDS$year[t1],FUN=max)
agg.data <- cbind(agg.data,tmin.t1.ma)

tmin.t2.ma <- tapply (evalDDS$tmin[t2],INDEX=evalDDS$year[t2],FUN=max)
agg.data <- cbind(agg.data,tmin.t2.ma)

tmin.t3.ma <- tapply (evalDDS$tmin[t3],INDEX=evalDDS$year[t3],FUN=max)
agg.data <- cbind(agg.data,tmin.t3.ma)

tmin.ta.ma <- tapply (evalDDS$tmin[ta],INDEX=evalDDS$year[ta],FUN=max)
agg.data <- cbind(agg.data,tmin.ta.ma)

#tmin-min
tmin.t0.mi <- tapply (evalDDS$tmin[t0],INDEX=evalDDS$year[t0],FUN=min)
agg.data <- cbind(agg.data,tmin.t0.mi)

tmin.t1.mi <- tapply (evalDDS$tmin[t1],INDEX=evalDDS$year[t1],FUN=min)
agg.data <- cbind(agg.data,tmin.t1.mi)

tmin.t2.mi <- tapply (evalDDS$tmin[t2],INDEX=evalDDS$year[t2],FUN=min)
agg.data <- cbind(agg.data,tmin.t2.mi)

tmin.t3.mi <- tapply (evalDDS$tmin[t3],INDEX=evalDDS$year[t3],FUN=min)
agg.data <- cbind(agg.data,tmin.t3.mi)

tmin.ta.mi <- tapply (evalDDS$tmin[ta],INDEX=evalDDS$year[ta],FUN=min)
agg.data <- cbind(agg.data,tmin.ta.mi)

#trange 
#define trange
evalDDS$trange <- evalDDS$tmax -evalDDS$tmin

#trange-ave
trange.t0.me <- tapply (evalDDS$trange[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,trange.t0.me)

trange.t1.me <- tapply (evalDDS$trange[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,trange.t1.me)

trange.t2.me <- tapply (evalDDS$trange[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,trange.t2.me)

trange.t3.me <- tapply (evalDDS$trange[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,trange.t3.me)

trange.ta.me <- tapply (evalDDS$trange[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,trange.ta.me)

#trange-max
trange.t0.ma <- tapply (evalDDS$trange[t0],INDEX=evalDDS$year[t0],FUN=max)
agg.data <- cbind(agg.data,trange.t0.ma)

trange.t1.ma <- tapply (evalDDS$trange[t1],INDEX=evalDDS$year[t1],FUN=max)
agg.data <- cbind(agg.data,trange.t1.ma)

trange.t2.ma <- tapply (evalDDS$trange[t2],INDEX=evalDDS$year[t2],FUN=max)
agg.data <- cbind(agg.data,trange.t2.ma)

trange.t3.ma <- tapply (evalDDS$trange[t3],INDEX=evalDDS$year[t3],FUN=max)
agg.data <- cbind(agg.data,trange.t3.ma)

trange.ta.ma <- tapply (evalDDS$trange[ta],INDEX=evalDDS$year[ta],FUN=max)
agg.data <- cbind(agg.data,trange.ta.ma)

#trange-min
trange.t0.mi <- tapply (evalDDS$trange[t0],INDEX=evalDDS$year[t0],FUN=min)
agg.data <- cbind(agg.data,trange.t0.mi)

trange.t1.mi <- tapply (evalDDS$trange[t1],INDEX=evalDDS$year[t1],FUN=min)
agg.data <- cbind(agg.data,trange.t1.mi)

trange.t2.mi <- tapply (evalDDS$trange[t2],INDEX=evalDDS$year[t2],FUN=min)
agg.data <- cbind(agg.data,trange.t2.mi)

trange.t3.mi <- tapply (evalDDS$trange[t3],INDEX=evalDDS$year[t3],FUN=min)
agg.data <- cbind(agg.data,trange.t3.mi)

trange.ta.mi <- tapply (evalDDS$trange[ta],INDEX=evalDDS$year[ta],FUN=min)
agg.data <- cbind(agg.data,trange.ta.mi)

#vp-ave
vp.t0.me <- tapply (evalDDS$vp[t0],INDEX=evalDDS$year[t0],FUN=mean)
agg.data <- cbind(agg.data,vp.t0.me)

vp.t1.me <- tapply (evalDDS$vp[t1],INDEX=evalDDS$year[t1],FUN=mean)
agg.data <- cbind(agg.data,vp.t1.me)

vp.t2.me <- tapply (evalDDS$vp[t2],INDEX=evalDDS$year[t2],FUN=mean)
agg.data <- cbind(agg.data,vp.t2.me)

vp.t3.me <- tapply (evalDDS$vp[t3],INDEX=evalDDS$year[t3],FUN=mean)
agg.data <- cbind(agg.data,vp.t3.me)

vp.ta.me <- tapply (evalDDS$vp[ta],INDEX=evalDDS$year[ta],FUN=mean)
agg.data <- cbind(agg.data,vp.ta.me)

#vp-max
vp.t0.ma <- tapply (evalDDS$vp[t0],INDEX=evalDDS$year[t0],FUN=max)
agg.data <- cbind(agg.data,vp.t0.ma)

vp.t1.ma <- tapply (evalDDS$vp[t1],INDEX=evalDDS$year[t1],FUN=max)
agg.data <- cbind(agg.data,vp.t1.ma)

vp.t2.ma <- tapply (evalDDS$vp[t2],INDEX=evalDDS$year[t2],FUN=max)
agg.data <- cbind(agg.data,vp.t2.ma)

vp.t3.ma <- tapply (evalDDS$vp[t3],INDEX=evalDDS$year[t3],FUN=max)
agg.data <- cbind(agg.data,vp.t3.ma)

#colinieality
#vp.ta.ma <- tapply (evalDDS$vp[ta],INDEX=evalDDS$year[ta],FUN=max)
#agg.data <- cbind(agg.data,vp.ta.ma)

#vp-min
vp.t0.mi <- tapply (evalDDS$vp[t0],INDEX=evalDDS$year[t0],FUN=min)
agg.data <- cbind(agg.data,vp.t0.mi)

vp.t1.mi <- tapply (evalDDS$vp[t1],INDEX=evalDDS$year[t1],FUN=min)
agg.data <- cbind(agg.data,vp.t1.mi)

vp.t2.mi <- tapply (evalDDS$vp[t2],INDEX=evalDDS$year[t2],FUN=min)
agg.data <- cbind(agg.data,vp.t2.mi)

vp.t3.mi <- tapply (evalDDS$vp[t3],INDEX=evalDDS$year[t3],FUN=min)
agg.data <- cbind(agg.data,vp.t3.mi)

vp.ta.mi <- tapply (evalDDS$vp[ta],INDEX=evalDDS$year[ta],FUN=min)
agg.data <- cbind(agg.data,vp.ta.mi)

agg.data$CLIMATE1 <- evalDS$CLIMATE1
agg.data$CLIMATE2 <- evalDS$CLIMATE2
agg.data$CLIMATE3 <- evalDS$CLIMATE3

agg.data$TEMP_MED <- evalDS$TEMP_MED
agg.data$PREC_MED <- evalDS$PREC_MED
agg.data$RAD_MED <- evalDS$RAD_MED

##---- 7. PCA-----
#create PCA with complete cases
#define predictors
predictors.soil <- c("LAT","LONG_",
"AREA","RM_25","TOT_IRR_DE",
"SY_DENS","SY_ACRES","CONUS_PH","CONUS_AWC","CONUS_CLAY","CONUS_SILT",
"CONUS_SAND","ISRIC_SAND","ISRIC_SILT","ISRIC_CLAY","ISRIC_PH","ISRIC_CEC",
"EXTRACT_CE","SOIL_CUBE1","SOIL_CUBE2","SOIL_CUBE3","RM_BAND_N")

predictors.weather <- c("CLIMATE1","CLIMATE2","CLIMATE3","TEMP_MED","PREC_MED","RAD_MED",
"dayl.t0.me","dayl.t1.me","dayl.t2.me",
"prcp.t0.me","prcp.t1.me","prcp.t2.me",
"dwr.t0.me","dwr.t1.me","dwr.t2.me",
"dwr.t0.ma","dwr.t1.ma","dwr.t2.ma",
"srad.t0.me","srad.t1.me","srad.t2.me",
"swe.t0.me",
#"swe.t1.me",
"tmax.t0.me","tmax.t1.me","tmax.t2.me",
"tmax.t0.ma","tmax.t1.ma","tmax.t2.ma",
"tmax.t0.mi","tmax.t1.mi","tmax.t2.mi",
"tmin.t0.me","tmin.t1.me","tmin.t2.me", 
"tmin.t0.ma","tmin.t1.ma","tmin.t2.ma",
"tmin.t0.mi","tmin.t1.mi","tmin.t2.mi",
"trange.t0.me","trange.t1.me","trange.t2.me",
"trange.t0.ma","trange.t1.ma","trange.t2.ma",
"trange.t0.mi","trange.t1.mi","trange.t2.mi",
"vp.t0.me","vp.t1.me","vp.t2.me",
"vp.t0.ma","vp.t1.ma","vp.t2.ma",
"vp.t0.mi","vp.t1.mi","vp.t2.mi"
#"TEMP_CUR","PREC_CUR","RAD_CUR"
)

predictor.s.f <- paste('~',paste(predictors.soil, collapse = '+'),sep="")
predictor.w.f <- paste('~',paste(predictors.weather, collapse = '+'),sep="")

#define a scaled data frame
PCAs <- prcomp(as.formula(predictor.s.f),
               data=trainDS[,predictors.soil],
              scale.=T)

PCAw <- prcomp(as.formula(predictor.w.f), 
                 data=trainDS[,predictors.weather],
               scale.=T)

#append columns to dataSet
comp.s <- PCAs$x
comp.w <- PCAw$x

colnames(comp.s) <- paste('COMP_S_',1:ncol(comp.s),sep='')
colnames(comp.w) <- paste('COMP_W_',1:ncol(comp.w),sep='')

trainDS <- cbind(trainDS,comp.s)
trainDS <- cbind(trainDS,comp.w)
PCAcalc.s <- PCAs$rotation
PCAcalc.w <- PCAw$rotation

#back up the trainDS
trainDS.bu <- trainDS

#remove the matrices
rm(comp.s,comp.w )

##---- 8. VARIETY COMBINATION - TO DO ----
#which varieties appears more than min.scen times.
min.scenarios <- 10

scen.var.table <- table(trainDS$SCENARIO,trainDS$VARIETY)

var.count <- colSums(scen.var.table > 0)

#varieties tested in more than min.scen 
varieties <- names(var.count[var.count>min.scenarios ])

scen.var.table.bin <- scen.var.table
scen.var.table.bin[scen.var.table>0] <- 1

##---- 8. DECISION KEEP ONLY THE VARIETIES APPEARING IN SCEN.MIN----
#trainDS <- trainDS [trainDS$VARIETY %in% varieties,]

##---- 9. SELECT MANUALY A SUBSET OF 8-10 CANDIDATES.----
#generate average variety_YI - scenario
yield.ave <- with(trainDS, tapply(VARIETY_YI, list(VARIETY, SCENARIO), FUN=median))

#generate data row 1 for each scenario (clear not used data)
scen.data <- trainDS[!duplicated(trainDS$SCENARIO),]

#store the namos of scen.data, they are used afterwards
scen.names <- names(scen.data)

#remove variety and variety_yi from the 
scen.data <- scen.data[,!(scen.names %in% c('VARIETY','VARIETY_YI'))]

#generates options
s <- seq(0,1,by=0.1)
g <- expand.grid(s,s,s,s,s)
g <- g[rowSums(g)==1,]
g <- g[apply(g,1,function(x) sum(x>0))>1,]

candidates.mix <- c('V68','V35','V41','V98','V96','V39','V187','V180') 

#generates a submat matrix to loop. (matrix of comb of varieties)
submat <- apply(combn(length(candidates.mix),5),1, 
                FUN=function(x){candidates.mix[x]})

#outside the loop
scen <- scen.data$SCENARIO
scen.var <- tapply( trainDS$VARIETY, trainDS$SCENARIO,
                    FUN = function(x) unique(x))

for (row.n in 1:nrow(submat)){
  cat(paste(100*row.n/nrow(submat),'%'))
  cand.5 <- submat[row.n,]
  
  #it starts with a list of 5 varieties and their percentage
  #generate names
  g.name <- apply(g,1,function(x) {
                paste(paste0(x[order(cand.5)],sort(cand.5))[x[order(cand.5)]>0],collapse ='_')
                })
  
  #delete the records in g g.name already in trainDS
  to.delete <- g.name %in% trainDS$VARIETY
  g.name <- g.name[!to.delete]
  g1 <- g[!to.delete,]
  
  #generate varlist only name without percentage
  g.var <- apply(g1,1,function(x) {
    cand.5[x>0]
  })

  #in case g.var is a matrix cast to a list
  if (class(g.var)=='matrix') {
    g.var <- as.list(data.frame(g.var))
    g.var[] <- lapply(g.var, as.character)
  }
  
  #generates a blank dataset. 
  comb <- data.frame(SCENARIO = as.character(0), 
                     VARIETY = as.character(0), 
                     VARIETY_YI = as.numeric(0),
                     stringsAsFactors=F)

  #loop among all scenarios. works where the varieties in g.var combinied are in common.
  for (sc.ix in 1:length(scen.var)) {

    sc <- scen.var[[sc.ix]]
    sc.name <- names(scen.var[sc.ix])
  
    #get the average yeilds.
    ave.yields <- yield.ave[,sc.name][cand.5]
  
    #keeps the varieties that intersect with the varieties present in sc[sc.ix]
    #g.var is the generations of different combination of the 5 cand.
    var.to.loop <-  lapply(g.var, function(x) {
      length(intersect(sc, x))==length(x)
    })

    #generates a matrix with the varieties
    row.to.loop <- g1[unlist(var.to.loop),]
    row.to.loop$name <-  g.name[unlist(var.to.loop)]

    #defines the function to add records to the file-- then apply
    add.one.record <- function (x) {
      #calculate yeild
      #print(as.numeric(x[1:5]))
      #print(ave.yields)
      y <- sum(as.numeric(x[1:5])*as.numeric(ave.yields),na.rm = TRUE)
      comb[nrow(comb)+1,] <<- c(sc.name,x[6],y)
    }
    
    #add.columns just if the intersection between scen and var >0
    if (sum(unlist(var.to.loop))>0){
      #apply the function in row to loop df
      apply(row.to.loop,1,add.one.record)
      if(sum(is.na(trainDS$VARIETY)>0)>0){
        cat (paste('... c..:',cand.5))
      }
    }
  } ## end loop through scenarios
  
  #remove firs row
  comb <- comb[-1,]

  #convert as data frame and consider variety yield as numeric
  comb$VARIETY_YI <- as.numeric(comb$VARIETY_YI)

  #merge the dataframe
  comb1 <- merge(comb, scen.data, by = c('SCENARIO'))

  #reorder the columns
  comb1 <- comb1[,scen.names]

  #convert variety in char before merge
  trainDS$VARIETY <- as.character(trainDS$VARIETY)

  #merge with the trainDS
  trainDS <- rbind(trainDS,comb1)

  #convert variety in factor again
  trainDS$VARIETY <- as.factor(trainDS$VARIETY)

  #remove the false
  trainDS <- trainDS[trainDS$VARIETY != FALSE,]
  if(sum(is.na(trainDS$VARIETY)>0)>0){
    cat (paste('... c..:',cand.5))
  }
} #end loop submat

#remove duplicated
trainDS <- trainDS[!duplicated(trainDS[,c('SCENARIO','VARIETY')]),]

##---- 11. CANDIDATE EVALUATION----
#select the variety with the highest 1st quartile
first.q <- tapply(trainDS$VARIETY_YI, trainDS$VARIETY, 
                  FUN = quantile, probs=0.25)
first.q <- data.frame (VARIETY =names(first.q) , VARIETY_YI=first.q)

#count how many records for each variety.
first.q$count <- table(unique(trainDS[,c('SCENARIO','VARIETY')])[,'VARIETY'])
#order
first.q <- first.q[with(first.q, order(-VARIETY_YI)), ]
first.q[first.q$count>10,] [1:20,]

first.m <- tapply(trainDS$VARIETY_YI, trainDS$VARIETY, 
                  FUN = mean)
first.m <- data.frame (VARIETY =names(first.m) , VARIETY_YI=first.m)

#count how many records for each variety.
first.m$count <- table(unique(trainDS[,c('SCENARIO','VARIETY')])[,'VARIETY'])
first.m <- first.m[with(first.m, order(-VARIETY_YI)), ]
#order
first.m [first.m$count>10,][1:20,]

#expected utility arrow pratt
r=2.5
trainDS$Util <- (trainDS$VARIETY_YI^(1-r))/(1-r)
first.e <- tapply(trainDS$Util, trainDS$VARIETY,FUN=mean)
first.e <- data.frame (VARIETY =names(first.e) , VARIETY_YI=first.e)
first.e$count <- table(unique(trainDS[,c('SCENARIO','VARIETY')])[,'VARIETY'])
first.e <- first.e[with(first.e, order(-VARIETY_YI)), ]
#order
first.e[first.e$count>10,] [1:20,]

##---- PLOTING ----
mat <- matrix(c(1,2,3,4),ncol =2)
layout(mat,c(1,1), c(1,3))

##### 9. CANDIDATE SELECTION----
cand <- first.q[first.q$count>10,][1,'VARIETY']
cand <- 'V187'

#define a preliminary testds
trainDS.c <- trainDS[trainDS$VARIETY==cand,]

##---- 12. TREE AND TRADE OFF PLOT----

#generate target categorical value when it is greater than 1st quartil
target <- quantile(trainDS.c$VARIETY_YI,0.25)
yield_t <- rep("GOOD", length(trainDS.c$VARIETY_YI))
yield_t[trainDS.c$VARIETY_YI<target] <- "BAD" 
trainDS.c$YIELD_T <- yield_t

#define the tree formula
#components
formula.s <- paste(paste('COMP_S_',1:dim(PCAcalc.s)[1],sep=""),collapse='+')
formula.w <- paste(paste('COMP_W_',1:dim(PCAcalc.w)[1],sep=""),collapse='+')
comp <- paste(formula.s,formula.w,sep='+')

#predictors
predictors <- paste(paste(predictors.soil,collapse='+'),
                    paste(predictors.weather,collapse='+'),
                    sep='+')

formula.t <- paste('YIELD_T',predictors,sep='~')
formula.tree <- paste(formula.t,comp,sep='+')

#parameters for the decision tree

loss.matrix <- matrix(c(0, 1, 1.2, 0), nrow=2, byrow=TRUE)	
uu8 <- rpart.control(minbucket = 2, maxdepth =30, xval=10)

#fit the tree!
fit.tree <- rpart(formula.tree, data = trainDS.c, control=uu8,parms=list(loss = loss.matrix))

#plot rpart
y <- fit.tree$frame$yval2[,5]
cols <- rgb(1,y,y)

prp(fit.tree, 
    cex.main=0.8,
    main=paste('Scenario Classification \n considering ',cand, sep=''))
#prp(fit.tree, type=0, extra=1, under=TRUE, uniform=TRUE, 
#    branch.col=cols, box.col=cols, branch.type=5, yesno=FALSE, faclen=0 
#)

#calculate the data for trade-off chart
pred.tree <- predict(fit.tree, newdata=trainDS, type="class")

good.axis <- tapply(trainDS$VARIETY_YI[pred.tree == "GOOD"],
               INDEX=trainDS$VARIETY[pred.tree == "GOOD"],
               FUN=quantile, probs=0.25, simplify=TRUE)

bad.axis <- tapply(trainDS$VARIETY_YI[pred.tree == "BAD"],
                   INDEX=trainDS$VARIETY[pred.tree == "BAD"],
                   FUN=quantile, probs=0.25, simplify=TRUE)

#check NAs in good
sort(bad.axis[is.na(good.axis)], decreasing=T)

##---- 12. PLOT THE POINTS----

#determine the paretto frontier
points <- rbind(-bad.axis,-good.axis)
points <- points[,colSums(is.na(points)) == 0]

#points <- points[,is_dominated(points)]
rtk <- table(unique(trainDS[,c('SCENARIO','VARIETY')])[,'VARIETY'])[colnames(points)]>3
points <- points[,rtk]

points <- -points[,!is_dominated(points)]
points <- t(points)
points <- points[order(points[,1]),]
#keep greater than 10

#names to trace V41
if (cand=='V41'){
names.tt <- c('0.1V180_0.9V187', '0.1V180_0.8V187_0.1V39', '0.1V180_0.8V187_0.1V41', 
              '0.1V180_0.7V187_0.2V41', '0.2V180_0.6V187_0.2V41', '0.1V180_0.5V187_0.4V41', 
              '0.1V180_0.4V187_0.5V41', '0.2V180_0.3V187_0.5V41', '0.1V180_0.2V187_0.7V41',  
              '0.1V180_0.1V187_0.8V41',  
              '0.9V41_0.1V98', '0.1V180_0.7V41_0.2V98', 
              '0.2V180_0.5V41_0.3V98', '0.1V180_0.5V41_0.4V98',  '0.1V180_0.4V41_0.5V98', 
              '0.2V180_0.2V41_0.6V98', '0.1V180_0.2V41_0.7V98',  '0.1V180_0.1V41_0.8V98',
              '0.1V41_0.9V98', '0.1V96_0.9V98', '0.2V96_0.8V98', '0.3V96_0.7V98', 
              '0.4V96_0.6V98', 'V96')
} else {
names.tt <- c('0.8V35_0.2V39', '0.7V35_0.2V39_0.1V41', '0.5V35_0.3V39_0.2V41', 
              '0.5V35_0.2V39_0.2V41_0.1V98', '0.5V35_0.1V39_0.2V41_0.2V98', 
              '0.4V35_0.2V39_0.3V41_0.1V98', '0.4V35_0.1V39_0.3V41_0.2V98', 
              '0.2V35_0.3V39_0.4V41_0.1V98', '0.2V35_0.2V39_0.4V41_0.2V98', 
              '0.1V35_0.3V39_0.5V41_0.1V98','0.1V35_0.2V39_0.5V41_0.2V98', 
              '0.1V35_0.1V39_0.5V41_0.3V98', '0.1V35_0.5V41_0.4V98', 
              '0.1V180_0.5V41_0.4V98', '0.1V180_0.4V41_0.5V98', 
              '0.1V180_0.2V41_0.7V98', '0.1V180_0.1V41_0.8V98', 
              '0.3V41_0.7V98', '0.2V41_0.8V98', '0.1V41_0.9V98')
}

#names.tt <- rownames(points)



xlimit=(range(points[,1])+c(-0.5,+1.5))

par(mar=c(5.1,4.6,4.1,2.1))
    
plot(points,
     type="o",
     xlim=xlimit,
     #ylim=c(64,70),
     ylim=(range(points[,2])+c(0,+1.5)),
     xlab="25% percentile of Yield dist. \n BAD-Scenarios",
     ylab="25% percentile of Yield dist. \n GOOD-Scenarios",
     cex=0.5,  cex.axis=0.7, cex.lab=0.7,
     main='Trade-off alternatives', cex.main=0.8)

text (points[names.tt,1]-0.02,points[names.tt,2]+0.05, names.tt,cex=0.5,srt = 45, pos=4)
points(bad.axis[cand],good.axis[cand],type="p", col='red', cex=0.5)
text (bad.axis[cand]-0.15,good.axis[cand]+0.05, 
      names(good.axis[cand]),cex=0.5,srt = 30, pos=4, col='red')

#### 11. COMPUTE ODDS----

comp.w.eval <- predict(PCAw,newdata=agg.data)
colnames(comp.w.eval) <- paste('COMP_W_',1:ncol(comp.w.eval),sep='')
agg.data <- cbind(agg.data,comp.w.eval)

if (cand=='V41') { 
  opt <-'0.9V41_0.1V98'
  prob.bad <- mean(agg.data$COMP_W_1[9:15]< (-0.28))
} else if (cand=='V187') {
  opt <-'0.1V180_0.5V41_0.4V98'
  prob.bad <- mean(agg.data$dwr.t2.me[9:15]>=3.238095)
}

#probability
prob.good <- 1-prob.bad
b <- sum(points[opt,]*c(prob.bad,prob.good))

#trace 
points2 <- data.frame(xs= xlimit, ys= (b-xlimit*prob.bad)/prob.good)
#ADD THIS TO THE GRAPH
points(points2, type='l',col='red',lty=5 )
points(bad.axis[cand],good.axis[cand],type="p", col='red', cex=0.5)
points(points[opt,1],points[opt,2], type="p",pch=19, col='red', cex=0.5)

##-- 12. BARPLOT----
#to do

##---- END. TODO NEXT----
# 1. analize prim algorithm and scenario discovery.
# 2. outliers.
# 5. fin the optimal t0, t1, t2, 
# 8. comparison with traditional methods Optimization.
# 10. weight the distance to select sites
# 11. calculate the expected utility
