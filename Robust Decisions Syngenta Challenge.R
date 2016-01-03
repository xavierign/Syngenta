## github
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

##---- 2. VISUALIZE THE DATA----

lat_m <- sum(range(trainDS$LAT))/2
lon_m <- sum(range(trainDS$LONG_))/2

myLocation <- c(lon = lon_m, lat = lat_m)

myMap <- get_map(location=myLocation,
                 source="google", maptype="terrain", crop=FALSE, zoom=5)

ggmap(myMap) + 
  geom_point(aes(x = LONG_, y = LAT), data = trainDS[trainDS$VARIETY=='V39',],
             alpha = .3, color="blue", size = 1) +
  geom_point(aes(x = LONG_, y = LAT), data = evalDS,
             alpha = .6, color="red", size = 1)

##---- 3. CONVERT AND ANALYZE THE DATA----

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

##---- 5. AGREGATION OF WEATHER DATA----
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

#consider varialbes TEMPXX, PRECXX, RADXX
trainDS$TEMP_CUR <- rep(NA,length(trainDS$SITE))
trainDS$PREC_CUR <- rep(NA,length(trainDS$SITE))
trainDS$RAD_CUR <- rep(NA,length(trainDS$SITE))

#08
trainDS$TEMP_CUR[trainDS$SEASON==2008]<- trainDS$TEMP_08[trainDS$SEASON==2008]
trainDS$PREC_CUR[trainDS$SEASON==2008]<- trainDS$PREC_08[trainDS$SEASON==2008]
trainDS$RAD_CUR[trainDS$SEASON==2008]<- trainDS$RAD_08[trainDS$SEASON==2008]
#09
trainDS$TEMP_CUR[trainDS$SEASON==2009]<- trainDS$TEMP_09[trainDS$SEASON==2009]
trainDS$PREC_CUR[trainDS$SEASON==2009]<- trainDS$PREC_09[trainDS$SEASON==2009]
trainDS$RAD_CUR[trainDS$SEASON==2009]<- trainDS$RAD_09[trainDS$SEASON==2009]
#10
trainDS$TEMP_CUR[trainDS$SEASON==2010]<- trainDS$TEMP_10[trainDS$SEASON==2010]
trainDS$PREC_CUR[trainDS$SEASON==2010]<- trainDS$PREC_10[trainDS$SEASON==2010]
trainDS$RAD_CUR[trainDS$SEASON==2010]<- trainDS$RAD_10[trainDS$SEASON==2010]
#11
trainDS$TEMP_CUR[trainDS$SEASON==2011]<- trainDS$TEMP_11[trainDS$SEASON==2011]
trainDS$PREC_CUR[trainDS$SEASON==2011]<- trainDS$PREC_11[trainDS$SEASON==2011]
trainDS$RAD_CUR[trainDS$SEASON==2011]<- trainDS$RAD_11[trainDS$SEASON==2011]
#12
trainDS$TEMP_CUR[trainDS$SEASON==2012]<- trainDS$TEMP_12[trainDS$SEASON==2012]
trainDS$PREC_CUR[trainDS$SEASON==2012]<- trainDS$PREC_12[trainDS$SEASON==2012]
trainDS$RAD_CUR[trainDS$SEASON==2012]<- trainDS$RAD_12[trainDS$SEASON==2012]
#13
trainDS$TEMP_CUR[trainDS$SEASON==2013]<- trainDS$TEMP_13[trainDS$SEASON==2013]
trainDS$PREC_CUR[trainDS$SEASON==2013]<- trainDS$PREC_13[trainDS$SEASON==2013]
trainDS$RAD_CUR[trainDS$SEASON==2013]<- trainDS$RAD_13[trainDS$SEASON==2013]
#14
trainDS$TEMP_CUR[trainDS$SEASON==2014]<- trainDS$TEMP_14[trainDS$SEASON==2014]
trainDS$PREC_CUR[trainDS$SEASON==2014]<- trainDS$PREC_14[trainDS$SEASON==2014]
trainDS$RAD_CUR[trainDS$SEASON==2014]<- trainDS$RAD_14[trainDS$SEASON==2014]

##---- 6. PCA-----
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
"swe.t0.me","swe.t1.me",
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
"vp.t0.mi","vp.t1.mi","vp.t2.mi",
"TEMP_CUR","PREC_CUR","RAD_CUR")

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

##---- 4. EXPLORE VARIETIES INTO GENERATE MIX.----
#parameters
min.scenarios <- 40

#create a table of varieties in each scenario.
var.mat <- tapply(trainDS$VARIETY, trainDS$SCENARIO, 
                  FUN = function(x) unique(x))
#count which are the pairs of varieties repeated in elements of a matrix.
varieties <- names(table(trainDS$VARIETY)[table(trainDS$VARIETY)>min.scenarios])
var.grid  <- NULL
#generate combinations
for (i in 1:(length(varieties)-1)){
  for (j in (1+i):length(varieties)){
    Var1 <- varieties[i]
    Var2 <- varieties[j]
    var.grid <- rbind(var.grid,data.frame(Var1,Var2))
  }
}

#convert columns to string
var.grid[, c(1,2)] <- sapply(var.grid[, c(1,2)], as.character)

scen.list <- vector("list", nrow(var.grid))
#loop 
for (sc in names(var.mat)){
    for (row in 1:nrow(var.grid)){
      if (sum(c(var.grid[row,1],var.grid[row,2] )%in% unlist(var.mat[sc]))>1){
        scen.list[[row]]<- c(scen.list[[row]],sc)
      }
    }
}

#keep only those records with more than 20 scen.
var.grid <- var.grid[(lapply(scen.list, length)>min.scenarios),]
scen.list <- scen.list[(lapply(scen.list, length)>min.scenarios)]

##---- . GENERATE MIX----
#generate average variety_YI - scenario
yield.ave <- with(trainDS, tapply(VARIETY_YI, list(VARIETY, SCENARIO), mean))

#generate data row 1 for each scenario (clear not used data)
scen.data <- trainDS[!duplicated(trainDS$SCENARIO),]
#remove unused data
names.to.del <- c("SEASON","BREEDING_G","EXTNO","VARIETY","VARIETY_YI","CHECK_YIEL",
                  "YIELD_DIFF")
scen.data[,names.to.del] <- NA
row.names(scen.data) <- scen.data$SCENARIO

#for pair in var.grid

comb <- data.frame(SCENARIO = as.character(0), 
                   VARIETY = as.character(0), 
                   VARIETY_YI = as.numeric(0),
                   stringsAsFactors=F)

for (row in 1:nrow(var.grid)) {
    #generate name text 25 50 75 <- 3 new variety-options
    opt1 <- paste(c('25',var.grid[row,1],'_75',var.grid[row,2]), collapse="")
    opt2 <- paste(c('50',var.grid[row,1],'_50',var.grid[row,2]), collapse="")
    opt3 <- paste(c('75',var.grid[row,1],'_25',var.grid[row,2]), collapse="")
    
    #for scen in list 
    for (scen in scen.list[[row]]){
        #copy.row <- scen.data[scen,]
        #copy 3 lines with the scen data
        SCENA <- scen
        
        #1
        VARI <- opt1
        VARI_YI <- yield.ave[var.grid[row,1],scen]*0.25 +
                               yield.ave[var.grid[row,2],scen]*0.75
        comb <- rbind(comb, c(SCENA,VARI,VARI_YI))
        
        #2
        VARI <- opt2
        VARI_YI <- yield.ave[var.grid[row,1],scen]*0.5 +
                               yield.ave[var.grid[row,2],scen]*0.5
        comb <- rbind(comb, c(SCENA,VARI,VARI_YI))
        
        #3
        VARI <- opt3
        VARI_YI <- yield.ave[var.grid[row,1],scen]*0.75 +
                               yield.ave[var.grid[row,2],scen]*0.25
        comb <- rbind(comb, c(SCENA,VARI,VARI_YI))
     
        #overwrigth the variety name and average variety yield (25 50 75)
        #add the 3 rows to the trainDS 
        #cat(paste("-", scen))
    }
    cat(paste("....", row))
}
#remove firs row
comb <- comb[-1,]

#convert as data frame and consider variety yield as numeric
comb$VARIETY_YI <- as.numeric(comb$VARIETY_YI)

#store the namos of scen.data
scen.names <- names(scen.data)

#remove variety and variety_yi from the 
scen.data <- scen.data[,!(scen.names %in% c('VARIETY','VARIETY_YI'))]

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
 

##---- 5. CANDIDATE SELECTION----
#select the variety with the highest 1st quartile
first.q <- aggregate(VARIETY_YI ~ VARIETY, data=trainDS, FUN=quantile, probs=0.25)

#count how many records for each variety.
first.q$count <- table(trainDS$VARIETY)

#order
first.q <- first.q[with(first.q, order(-VARIETY_YI)), ]

#V39 seems to be the first candidate. V182 the second
cand <- '75V38_25V98'

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
loss.matrix <- matrix(c(0, 1, 1, 0), nrow=2, byrow=TRUE)	
uu8 <- rpart.control(minbucket = 4, maxdepth =30, xval=100)

#fit the tree!
fit.tree <- rpart(formula.tree, data = trainDS.c, control=uu8,parms=list(loss = loss.matrix))

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
     xlim=c(30,90),
     ylim=c(45,80),
     type="n")
text (bad.axis,good.axis, names(good.axis),cex=0.7)
text (bad.axis[cand],good.axis[cand], names(good.axis[cand]),cex=0.7, col='red')

#review the fronteir candidates
first.q[first.q$VARIETY %in% c('V24','V22','V68', 'V81','V25','V200','V171'),]

##---- TODO NEXT----
# 1. analize prim algorithm and scenario discovery.
# 2. outliers.
# 4. generate combinations of variety
# 5. fin the optimal t0, t1, t2, 
# 6. verify same variety repeated in the same scenario.
