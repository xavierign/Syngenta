#more information about the contest https://www.ideaconnection.com/syngenta-crop-challenge/

##---- 0. Libraries ----

# --- clear all objects
rm(list = ls()); gc();

# --- load necessary packages
library(openxlsx)
library(ggmap)
library(rpart)
library(rpart.plot)
library(dplyr)
library(reshape2)

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
                      startRow = 1)

file5 <- "Training and Evaluation data key.xlsx"
infile5 <- paste(indir,file5, sep="")

trainevalDK <- read.xlsx(infile5,sheet=1,
                     colNames=T,
                     startRow = 1)

##---- 2. ANALYZE THE DATA----

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

#convert RM_BAND into a number
RM_BAND_N <- rep(1,length(trainDS$RM_BAND))
RM_BAND_N[trainDS$RM_BAND == '1.50-1.99'] <- 1.75
RM_BAND_N[trainDS$RM_BAND == '2.00-2.49'] <- 2.25
RM_BAND_N[trainDS$RM_BAND == '2.50-2.99'] <- 2.75
RM_BAND_N[trainDS$RM_BAND == '3.00-3.49'] <- 3.25

trainDS$RM_BAND_N <- RM_BAND_N

#create a table site vs. variety
site.vs.variety <- table(trainDS$SITE,trainDS$VARIETY)

#create a table site vs. season
site.vs.season <- table(trainDS$SITE,trainDS$SEASON)

#create a dataset to work with site, seasson and variety.
ssv <- trainDS[,c('SITE','SEASON','VARIETY')]

#remove duplicates same site, season and variety.
ssv <- ssv[!duplicated(ssv),]

#count how many varieties in each site each year.
ssv.pivot = dcast(ssv, SITE ~ SEASON, value.var = "VARIETY")
ssv.pivot

#the data is very sparesed! can not define combinations at this point.

##---- 3. VISUALIZE THE DATA----

lat_m <- sum(range(trainDS$LAT))/2
lon_m <- sum(range(trainDS$LONG_))/2

myLocation <- c(lon = lon_m, lat = lat_m)

myMap <- get_map(location=myLocation,
                 source="google", maptype="terrain", crop=FALSE, zoom=4)

ggmap(myMap) + 
  geom_point(aes(x = LONG_, y = LAT), data = trainDS,
             alpha = .1, color="blue", size = 1) +
  geom_point(aes(x = LONG_, y = LAT), data = evalDS,
             alpha = .6, color="red", size = 1)

   

##---- 4. AGREGATION OF WEATHER DATA----
#define the temrs 1, 2 and 3

start0 <- 91
start1 <- 112
start2 <- 173
start3 <- 235
end3 <- 305

#define the booleans t1 t2 t3

t0 <- trainDDS$yday>=start0&trainDDS$yday<start1
t1 <- trainDDS$yday>=start1&trainDDS$yday<start2
t2 <- trainDDS$yday>=start2&trainDDS$yday<start3
t3 <- trainDDS$yday>=start3&trainDDS$yday<=end3
ta <- trainDDS$yday>=start0&trainDDS$yday<=end3

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

swe.t2.me <- tapply (trainDDS$swe[t2],INDEX=trainDDS$Scenario[t2],FUN=mean)
agg.data <- cbind(agg.data,swe.t2.me)

swe.t3.me <- tapply (trainDDS$swe[t3],INDEX=trainDDS$Scenario[t3],FUN=mean)
agg.data <- cbind(agg.data,swe.t3.me)

swe.ta.me <- tapply (trainDDS$swe[ta],INDEX=trainDDS$Scenario[ta],FUN=mean)
agg.data <- cbind(agg.data,swe.ta.me)

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

vp.ta.ma <- tapply (trainDDS$vp[ta],INDEX=trainDDS$Scenario[ta],FUN=max)
agg.data <- cbind(agg.data,vp.ta.ma)

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

##---- 4. PCA-----
#create PCA with complete cases
formula0 <- '~RM_BAND_N + AREA + CLIMATE1 + CLIMATE2 + CLIMATE3 + RM_25 + TOT_IRR_DE + 
 SY_DENS + SY_ACRES + CONUS_PH + CONUS_AWC + CONUS_CLAY + CONUS_SILT + 
CONUS_SAND +
ISRIC_SAND + ISRIC_SILT + ISRIC_CLAY + ISRIC_PH + ISRIC_CEC + EXTRACT_CE +
dwr.t0.me+  dwr.t1.me+  dwr.t2.me+  dwr.t3.me+  dwr.t0.ma+  dwr.t1.ma+
dwr.t2.ma+  dwr.t3.ma+ 
dayl.t0.me +
dayl.t1.me+dayl.t2.me+dayl.t3.me+prcp.t0.me+prcp.t1.me+prcp.t2.me +
prcp.t3.me+srad.t0.me+srad.t1.me+srad.t2.me+srad.t3.me+
swe.t0.me+ swe.t1.me+ swe.t2.me+ swe.t3.me+ tmax.t0.me+tmax.t1.me +
tmax.t2.me+tmax.t3.me+tmax.t0.ma+tmax.t1.ma+tmax.t2.ma+tmax.t3.ma +
tmax.t0.mi+tmax.t1.mi+tmax.t2.mi+tmax.t3.mi+tmin.t0.me +
tmin.t1.me+tmin.t2.me+tmin.t3.me+tmin.t0.ma+tmin.t1.ma+tmin.t2.ma +
tmin.t3.ma+tmin.t0.mi+tmin.t1.mi+tmin.t2.mi+tmin.t3.mi+
trange.t0.me+
trange.t1.me+trange.t2.me+trange.t3.me+trange.t0.ma+trange.t1.ma+
trange.t2.ma +
trange.t3.ma+trange.t0.mi+trange.t1.mi+trange.t2.mi+trange.t3.mi+

vp.t0.me+  vp.t1.me+  vp.t2.me+  vp.t3.me+    vp.t0.ma+  vp.t1.ma+
vp.t2.ma+  vp.t3.ma+  vp.t0.mi+  vp.t1.mi+  vp.t2.mi+  vp.t3.mi' 

#define a scaled data frame


PCA <- princomp(as.formula(formula0), data=trainDS)
#PCA <- princomp(dataSet[,c(1:6,26:(ncol(dataSet)-5))])

#append columns to dataSet
trainDS <- cbind(trainDS,PCA$scores)

PCAcalc <- loadings(PCA)


##---- 5. CANDIDATE SELECTION----
#select the variety with the highest 1st quartile
first.q <- aggregate(VARIETY_YI ~ VARIETY, data=trainDS, FUN=quantile, probs=0.25)

#count how many records for each variety.
first.q$count <- table(trainDS$VARIETY)

#order
first.q <- first.q[with(first.q, order(-VARIETY_YI)), ]

#V39 seems to be the first candidate. V182 the second

cand <- 'V39'
#histogram of performances
hist(trainDS$VARIETY_YI[trainDS$VARIETY==cand])

#define a preliminary testds
trainDS.c <- trainDS[trainDS$VARIETY==cand,]

##---- 10. preliminary test----

#generate target categorical value when it is greater than 1st quartil
target <- quantile(trainDS.c$VARIETY_YI,0.25)
yield_t <- rep("GOOD", length(trainDS.c$VARIETY_YI))
yield_t[trainDS.c$VARIETY_YI<target] <- "BAD" 
trainDS.c$YIELD_T <- yield_t

formula <- 'YIELD_T~ 
    RM_BAND_N + AREA + CLIMATE1 + CLIMATE2 + CLIMATE3 + RM_25 + TOT_IRR_DE + SEASON1 +
    SY_DENS + SY_ACRES + CONUS_PH + CONUS_AWC + CONUS_CLAY + CONUS_SILT + 
    CONUS_SAND +
    ISRIC_SAND + ISRIC_SILT + ISRIC_CLAY + ISRIC_PH + ISRIC_CEC + EXTRACT_CE +
    dwr.t0.me+  dwr.t1.me+  dwr.t2.me+  dwr.t3.me+  dwr.ta.me+  dwr.t0.ma+  dwr.t1.ma+
    dwr.t2.ma+  dwr.t3.ma+  dwr.ta.ma+ 
    dayl.t0.me +
    dayl.t1.me+dayl.t2.me+dayl.t3.me+dayl.ta.me+prcp.t0.me+prcp.t1.me+prcp.t2.me +
    prcp.t3.me+prcp.ta.me+srad.t0.me+srad.t1.me+srad.t2.me+srad.t3.me+srad.ta.me +
    swe.t0.me+ swe.t1.me+ swe.t2.me+ swe.t3.me+ swe.ta.me+ tmax.t0.me+tmax.t1.me +
    tmax.t2.me+tmax.t3.me+tmax.ta.me+tmax.t0.ma+tmax.t1.ma+tmax.t2.ma+tmax.t3.ma +
    tmax.ta.ma+tmax.t0.mi+tmax.t1.mi+tmax.t2.mi+tmax.t3.mi+tmax.ta.mi+tmin.t0.me +
    tmin.t1.me+tmin.t2.me+tmin.t3.me+tmin.ta.me+tmin.t0.ma+tmin.t1.ma+tmin.t2.ma +
    tmin.t3.ma+tmin.ta.ma+tmin.t0.mi+tmin.t1.mi+tmin.t2.mi+tmin.t3.mi+tmin.ta.mi +
    trange.t0.me+
    trange.t1.me+trange.t2.me+trange.t3.me+trange.ta.me+trange.t0.ma+trange.t1.ma+
    trange.t2.ma +
    trange.t3.ma+trange.ta.ma+trange.t0.mi+trange.t1.mi+trange.t2.mi+trange.t3.mi+
    trange.ta.mi +
    vp.t0.me+  vp.t1.me+  vp.t2.me+  vp.t3.me+  vp.ta.me+  vp.t0.ma+  vp.t1.ma+
    vp.t2.ma+  vp.t3.ma+  vp.ta.ma+  vp.t0.mi+  vp.t1.mi+  vp.t2.mi+  vp.t3.mi+
    vp.ta.mi +
    Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.9 + Comp.10+
    Comp.11 + Comp.12 + Comp.13 + Comp.14 + Comp.15 + Comp.16 + Comp.17 + Comp.18 + Comp.19 + Comp.20+
    Comp.21 + Comp.22 + Comp.23 + Comp.24 + Comp.25 + Comp.26 + Comp.27 + Comp.28 + Comp.29 + Comp.30+
    Comp.31 + Comp.32 + Comp.33 + Comp.34 + Comp.35 + Comp.36 + Comp.37 + Comp.38 + Comp.39 + Comp.40+
    Comp.41 + Comp.42 + Comp.43 + Comp.44 + Comp.45 + Comp.46 + Comp.47 + Comp.48 + Comp.49 + Comp.50+
    Comp.51 + Comp.52 + Comp.53 + Comp.54 + Comp.55 + Comp.56 + Comp.57 + Comp.58 + Comp.59 + Comp.60+
    Comp.61 + Comp.62 + Comp.63 + Comp.64 + Comp.65 + Comp.66 + Comp.67 + Comp.68 + Comp.69 + Comp.70+
    Comp.71 + Comp.72 + Comp.73 + Comp.74 + Comp.75 + Comp.76 + Comp.77 + Comp.78 + Comp.79 + Comp.80+
    Comp.81 + Comp.82 + Comp.83 + Comp.84 + Comp.85 + Comp.86 + Comp.87 + Comp.88 + Comp.89 + Comp.90+
    Comp.91 + Comp.92' 
  
loss.matrix <- matrix(c(0, 1, 1, 0), nrow=2, byrow=TRUE)	

# --- Fit the classification tree.
uu8 <- rpart.control(minbucket = 2, maxdepth =29, xval=100)

fit.tree <- rpart(formula, data = trainDS.c, control=uu8,parms=list(loss = loss.matrix))
#fit.tree <- rpart(formula, data = trainDS.c)

#plot rpart
y <- fit.tree$frame$yval2[,5]
cols <- rgb(1,y,y)
prp(fit.tree, type=0, extra=1, under=TRUE, uniform=TRUE, 
    branch.col=cols, box.col=cols, branch.type=5, yesno=FALSE, faclen=0 
)

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

plot(bad.axis,good.axis,
     xlim=c(30,80),
     ylim=c(45,80),
     type="n")
text (bad.axis,good.axis, names(good.axis),cex=0.7)
text (bad.axis[cand],good.axis[cand], names(good.axis[cand]),cex=0.7, col='red')

#review the fronteir candidates
first.q[first.q$VARIETY %in% c('V59','V24','V15','V25', 'V87','V27','V24','V79','V200'),]

##---- TODO NEXT----
# 1. analize prim algorithm and scenario discovery.
# 2. principal components are not selected-- try scale the matrix and outliers.
# 3. clean the code and the formulas.
