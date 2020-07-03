# setting work directory and libraries #
rm(list = ls())

# # Packages used
# require(R6)
require(data.table)
# require(Rcpp)
# require(splines)
library(devtools)
library(roxygen2)

pack <- as.package("../../onlineforecast")
load_all(pack)


# Importing data # First unzip to get the .csv system('unzip
# ../data/DataSoenderborg.zip')
data_or <- fread("data_soenderborg.csv", sep = ",", header = TRUE)
data_or[, `:=`(t, asct(data_or$t))]
setDF(data_or)
names(data_or)[names(data_or) == "Ig.obs"] <- "I.obs"

# Make a data.table for each variable
tmp <- unlist(getse(strsplit(names(data_or)[-1], "\\."), 2))
colnm <- unlist(getse(strsplit(names(data_or)[-1], "\\."), 1))
nms <- unique(colnm[grep("^k\\d*$", tmp)])

kmax <- 48
data <- list()
data$t <- data_or$t
for (ii in 1:length(nms)) {
    # Find the columns
    i <- grep(pst("^", nms[ii], "$"), unlist(getse(strsplit(names(data_or)[-1],"\\."), 1))) + 1
    # Take only with kxx
    i <- i[grep("k[[:digit:]]+$", names(data_or)[i])]
    # 
    #
    data[[nms[ii]]] <- lag(data_or[ ,i], -1:-length(i))
    names(data[[nms[ii]]]) <- pst("k", 1:length(i))
    row.names(data[[nms[ii]]]) <- NULL
    data[[nms[ii]]] <- as.data.frame(data[[nms[ii]]])
    # Check if observed climate variable is there
    nm <- pst(nms[[ii]], ".obs")
    if (nm %in% names(data_or)) {
        data[[nm]] <- data_or[, nm]
    }
}
# More
cols <- pst("Heatload.house", 1:16)
data[["Heatload"]] <- data_or[, cols]
names(data[["Heatload"]]) <- pst("house", 1:16)
# 
data[["cosAoi"]] <- data_or[, "cosAoi.obs"]
data[["sunElevation"]] <- data_or[, "sunElevation.obs"]


# # The time of day
# ncol <- ncol(data$Ta)
# tmp <- aslt(data$t)$hour
# tmp <- matrix(tmp, nrow = length(tmp), ncol = ncol)
# tmp <- data.frame(t(t(tmp) + (0:(ncol - 1))))
# names(tmp) <- pst("k", 0:(ncol - 1))
# data$tday <- tmp%%24

#
class(data) <- c("data.list","list")

# Save for other scripts to read it
#saveRDS(data, "data_soenderborg.RDS")


data$heatloadtotal <- sapply(1:nrow(data$Heatload), function(i){
    mean(unlist(data$Heatload[i, ]), na.rm=TRUE)
})
data$totaln <- sapply(1:nrow(data$Heatload), function(i){
    sum(is.na(unlist(data$Heatload[i, ])))
})
plot(data$t, data$totaln)

# Write for building heat load forecasting
#Dbuildingheatload <- subset(data, c("2010-12-15","2011-03-01"), nms=c("t","Heatload","Ta","I","Ws","Wd","Ta.obs","I.obs","Wd.obs","Ws.obs","cosAoi","sunElevation","tday"))
data$heatload <- data$Heatload$house9
Dbuildingheatload <- subset(data, c("2010-12-15","2011-03-01"), nms=c("t","heatload","heatloadtotal","Ta.obs","I.obs","Ta","I"))
rownames(Dbuildingheatload$Ta) <- NULL
Dbuildingheatload$Ta <- Dbuildingheatload$Ta[ ,1:36]
rownames(Dbuildingheatload$I) <- NULL
Dbuildingheatload$I <- Dbuildingheatload$I[ ,1:36]
#
usethis::use_data(Dbuildingheatload, overwrite=TRUE) 




#----------------------------------------------------------------
# The electricity was not in the above data, so add it
# Open the info
info <- read.table("data_soenderborg_selectedInfo.csv",header=TRUE,sep=",")

# Open the hourly data, and make one data.frame with the power for each house
load("data_soenderborg_orig.rda")
#load("../data/heat_sp-1.rda")
D <- D[D$sn %in% info$sn, ]

# Make the acc. el energy (kWh) into into power (kW)
L <- split(D,D$sn)
x <- L[[3]]
Lre <- lapply(L, function(x){
  x$el <- c(NA,diff(x$P2) / as.numeric(diff(x$t),unit="hours"))
  # Remove outliers
  x$el[x$el>100] <- NA
  x$el[x$el<0] <- NA
# plot(x$t,x$el,ylim=c(0,20))
#   plot(x$t,cumsumWithNA(x$el))
# plot(x$t,x$P2)  
  return(x)
})
D <- do.call("rbind",Lre)

# Make into a data.frame with each the power for each seperate house as columns
L <- split(D[ ,c("t","el")],D$sn)
X <- L[[1]]
houseid <- info$houseid[ which(info$sn==as.numeric(names(L)[1])) ]
names(X)[2] <- pst("el",houseid)
# Rename the columns
for(i in 2:length(L))
  {
    tmp <- L[[i]]
    houseid <- info$houseid[ which(info$sn==as.numeric(names(L)[i])) ]
    names(tmp)[2] <- pst("el",houseid)
    X <- merge(X,tmp,by="t",all=TRUE)
  }

# To hourly
X <- resample(X, ts=3600, tstart=asct("2008-12-01"), tend=asct("2011-05-01"))

#---------
# SOME OF THE FOLLOWING MIGHT BE ERRORFUL (some leftover stuff from a merge was making problems)
# Read the splitted heat
load(pst("data_soenderborg_heatsplit/heatSplit.sp_1.sn_",info$sn[1],".rda"))
tmp <- DH1
names(tmp)[-1] <- pst("house",1,"_",names(tmp)[-1])
Xor <- tmp
#
for(i in 2:nrow(info)){
    load(pst("data_soenderborg_heatsplit/heatSplit.sp_1.sn_",info$sn[i],".rda"))
    #
    tmp <- DH1
    names(tmp)[-1] <- pst("house",i,"_",names(tmp)[-1])
    Xor <- merge(Xor,tmp,by="t")
}


# Put together data and X and Xor
Dbuilding <- subset(data, nms=c("t","Heatload","Ta","I","Ws","Wd","Ta.obs","I.obs","Wd.obs","Ws.obs","cosAoi","sunElevation","tday"))
range(Dbuilding$t)
range(X$t)

tmp <- X#[in_range("2010-12-15 01:00:00 GMT", X$t, "2011-02-01 00:00:00 GMT"), ]
tmp <- tmp[ ,-1]
names(tmp) <- pst("house", 1:16)
Dbuilding$Electricityload <- tmp


# Xor
Dbuilding <- subset(Dbuilding, c("2009-01-01 00:00:00 GMT", "2011-05-01 00:00:00 GMT"))
range(Dbuilding$t)
range(Xor$t)
tmp <- Xor[period("2008-12-01 01:00:00 GMT",Xor$t,"2011-05-01 00:00:00 GMT"), ]
tmp <- tmp[ ,-1]
Dbuilding$All <- tmp


#
str(Dbuilding)

#
plot(Dbuilding$Ta.obs, Dbuilding$Ta$k1)
plot(Dbuilding$Ta.obs, lag(Dbuilding$Ta$k1, 0))
plot(Dbuilding$Ta.obs, lag(Dbuilding$Ta$k1, 1))
plot(Dbuilding$Ta.obs, lag(Dbuilding$Ta$k1, 2))

plot(Dbuilding$All$house1_effekt)
lines(Dbuilding$All$house1_Pheat)
lines(Dbuilding$All$house1_Pwater)
plot(Dbuilding$Electricityload$house1, Dbuilding$All$house1_P1)


# Don't save it in this folder
# saveRDS(Dbuilding, file="XX/Dbuilding.Rda")



#----------------------------------------------------------------
# Make for solar power forecasting example
source("functions/aoi.R")
data_all <- data
# Take the observed global radiation
names(data_all)
data_all$y <- data_all$I.obs

# Make a transformation into solar power, simply a projection on an inclined surface
names(data_all)

X <- data.frame(t=data_all$t, G=data_all$I.obs, sunElevation=data_all$sunElevation)
X$IbNwp <- lag_vector(data_all$Ib$k1, 1)
X$IdNwp <- lag_vector(data_all$Id$k1, 1)
#
# Some small positive morning values
plot_ts(X[period("2011-04-01",X$t,"2011-04-10"), ], c("^G","sunElevation"))
X$G[X$sunElevation < 0] <- 0
plot_ts(X[period("2011-04-01",X$t,"2011-04-10"), ], c("^G","sunElevation"))
#
#----------------------------------------------------------------
# First split into direct and diffuse
zenith <- (pi/2) - X$sunElevation
# Clearness index with fitted clear sky radiation
G0 <- 1367 * (1 + 0.033*cos( 2*pi*as.POSIXlt(X$t)$yday/365)) * cos(zenith)
kt <- X$G / (G0/1000)
# air mass
m <- 1/cos(zenith)     
m[zenith>(0.95*pi/2)] <- 20
# Only solar elevation above xx deg
iNA <- 89<aoiToDeg(zenith) | aoiToDeg(zenith)<0
iNA[is.na(iNA)] <- FALSE
# i <- 10000:12800
# plotTSBeg(2)
# plot(X$t[i],kt[i],ylim=c(0,1.5))
# plot(X$t[i],X$Ig[i],ylim=c(0,1000))
# lines(X$t[i],cl$Surf$I[i],col=2)
# plotTSXAxis(X$t[i])
kt[iNA] <- NA
kt[kt>1] <- 1
# Split into Diffuse
# Id <- Ig * (0.944-1.538*exp(-exp(2.808-4.759*kt+2.276*kt^2)))# + 0.125*m + 0.013*m^2)))
# Id <- Ig * (0.952-1.041*exp(-exp(2.3-4.702*kt)))
# oldktFunction <- function(kt,a=0.952,b=1.041,c=2.3,d=4.702){ 0.3*(a-b*exp(-exp(c-d*kt)))+0.1 }
ktSigmoid <- function(kt,minOut,maxOut,offset,slope){ minOut+(maxOut-minOut)*(1-1/(1+exp(-(slope*(kt-offset))))) }
# See how the sigmoid looks
# t1 <- seq(0,1,by=0.01)
# plot(t1,ktSigmoid(t1,minOut=0.12,maxOut=0.85,offset=0.45,slope=10),ylim=c(0,1))
# Split it
X$Gdiffuse <- X$G * ktSigmoid(kt,minOut=0.12,maxOut=0.85,offset=0.45,slope=10)
X$Gdirect <- X$G - X$Gdiffuse
# For solar evelation below xx deg, set to all diffuse
X$Gdiffuse[iNA] <- X$G[iNA]
X$Gdirect[iNA] <- 0
#
#plotmulti(X[period("2009-06-01",X$t,"2009-06-10"), ], c("^G","^Ib|^Id"))
#
#
X$sinsunelev <- sin(X$sunElevation)
# Clip it
X$sinsunelev[X$sinsunelev<0.01] <- 0.01
# Project the solar beam plane 
X$Ib_solarplane <- X$Gdirect / X$sinsunelev

#
panel_power <- function(pAzimuth){
    latitude <- 54.909275
    longitude <- 9.807340
    slope <- 45
    X$cosAOI <- aoiCos1(X$t, latitude, longitude, slope, pAzimuth)
    X$sinAOI <- sqrt(1 - X$cosAOI^2)
    # Since the sun is behind the plane when cosAOI < 0
    X$sinAOI[X$cosAOI < 0] <- 0
    X$Ib_PV_plane <- X$Ib_solarplane * X$sinAOI
    #
    #plotmulti(X[period("2009-06-01",X$t,"2009-06-10"), ], c("^I|^G","cos|sin"))
    #
    # Apply an angle of incidence modifier on the direct
    fKtab <- function(b0,cosTheta)
    {
        # Calculate the AOI modifier
        Ktab <- 1 - b0*(1/cosTheta - 1)
        # set the AOI modifier to 0 for cosTheta<0
                                        #    Ktab[cosTheta<=0] <- 0
                                        #    Ktab[Ktab<=0] <- 0
        cosTheta0 <- 1/(1/b0+1)
        x <- 1/(1+exp(-1000*(cosTheta-cosTheta0)))
        Ktab <- x*Ktab
        return(Ktab)
    }
    X$fKtab <- fKtab(0.1, X$cosAOI)
    # The power is then a mix of direct and diffuse
    X$P_PV_plane <- 0.6 * (X$fKtab * X$Ib_PV_plane + X$Gdiffuse) + 0.4 * X$G
    #plotmulti(X[period("2009-07-01",X$t,"2009-07-10"), ], c("^I|^G|^P","sin","fKtab","^G$|^P_PV_plane$"))
    #
    #
    X$y <- 6 * X$P_PV_plane
    X$y[X$y > 5000] <- 2000
    #
    #
    #    tmp <- as.datalist(data_all, period("2011-04-01", data_all$t, "2011-04-08"))
    tmp <- X[period("2011-03-07", X$t, "2011-03-14"), ]
    plot(tmp$t, tmp$G/max(tmp$G, na.rm=TRUE), type="l", ylab="Normalized")
    title(paste("Panel surface towards",pAzimuthSeq_text[i]), line=-1.2)
    lines(tmp$t, tmp$y/max(tmp$y, na.rm=TRUE), type="l", col=2)
    X$P_PV_plane[X$P_PV_plane>1000] <- 998
    return(X$P_PV_plane)
}

# Fit for these azimuth angles
pAzimuthSeq <- c(-90, -20, 0, 90)
pAzimuthSeq_text <- c("East", "South", "20 deg. East", "West")
#

setpar("ts", mfrow=c(length(pAzimuthSeq),1))
for(i in 1:length(pAzimuthSeq)){
    # The sine of the AOI on an inclined surface
    panel_power(pAzimuthSeq[i])
}
legend("topright", c("Global radiation    ","Solar power"), lty=1, col=c(1,2))
axis.POSIXct(1, X$t, xaxt="s")


pAzimuthSeq <- c(-40, -20, 0, 20, 40)
setpar("ts", mfrow=c(length(pAzimuthSeq),1))
x <- lapply_cbind_df(pAzimuthSeq, panel_power)
names(x) <- pst("azimuth.",gsub("-","m",pAzimuthSeq))

data_all$PVpower <- x * 5
plot_ts.data.list(data_all, c("^I$","PVpower"), kseq=1)


# Write for solar power forecasting
Dsolarpower <- subset(data_all, c("2010-01-01","2011-01-01"), nms=c("t","PVpower","I","Ta","I.obs","Ta.obs","cosAoi","sunElevation","tday"))
#
usethis::use_data(Dsolarpower, overwrite=TRUE) 
