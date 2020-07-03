#### setting work directory and libraries ####
rm(list = ls())

## ## Packages used
## require(R6)
require(data.table)
## require(Rcpp)
## require(splines)
library(devtools)
library(roxygen2)

pack <- as.package("../../onlineforecast")
load_all(pack)


##### Importing data #### First unzip to get the .csv system('unzip
##### ../data/DataSoenderborg.zip')
data_or <- fread("data_soenderborg.csv", sep = ",", header = TRUE)
data_or[, `:=`(t, asct(data_or$t))]
setDF(data_or)


##----------------------------------------------------------------
## The electricity was not in the above data, so add it
## Open the info
info <- read.table("data_soenderborg_selectedInfo.csv",header=TRUE,sep=",")

## Open the hourly data, and make one data.frame with the power for each house
load("data_soenderborg_orig.rda")
#load("../data/heat_sp-1.rda")
D <- D[D$sn %in% info$sn, ]

## Make the acc. el energy (kWh) into into power (kW)
L <- split(D,D$sn)
x <- L[[3]]
Lre <- lapply(L, function(x){
  x$el <- c(NA,diff(x$P2) / as.numeric(diff(x$t),unit="hours"))
  ## Remove outliers
  x$el[x$el>100] <- NA
  x$el[x$el<0] <- NA
## plot(x$t,x$el,ylim=c(0,20))
##   plot(x$t,cumsumWithNA(x$el))
## plot(x$t,x$P2)  
  return(x)
})
D <- do.call("rbind",Lre)

## Make into a data.frame with each the power for each seperate house as columns
L <- split(D[ ,c("t","el")],D$sn)
X <- L[[1]]
houseid <- info$houseid[ which(info$sn==as.numeric(names(L)[1])) ]
names(X)[2] <- pst("el",houseid)
## Rename the columns
for(i in 2:length(L))
  {
    tmp <- L[[i]]
    houseid <- info$houseid[ which(info$sn==as.numeric(names(L)[i])) ]
    names(tmp)[2] <- pst("el",houseid)
    X <- merge(X,tmp,by="t",all=TRUE)
  }

## To hourly
X <- resample(X, ts=3600, tstart=asct("2008-12-01"), tend=asct("2011-05-01"))

##---------
## Read the splitted heat
load(pst("data_soenderborg_heatsplit/heatSplit.sp_1.sn_",info$sn[1],".rda"))
Xheat <- DH1[,c("t","Pheat")]
names(Xheat) <- c("t",pst("heat",1))
##
for(i in 2:nrow(info))
  {
    load(pst("data_soenderborg_heatsplit/heatSplit.sp_1.sn_",info$sn[i],".rda"))
    tmp <- DH1[,c("t","effekt")]
    names(tmp) <- c("t",pst("heat",i))
    Xheat <- merge(Xheat,tmp,by="t")
  }

## Put them together
X <- merge(Xheat,X,by="t")

## Check that they are the same houses:
## tmp <- merge(X,data_or, by="t")
## tmp$heat8 == tmp$Heatload.house8
## plot(tmp$heat2 - tmp$Heatload.house2)
## ##
## i <- 1:1000
## plot(tmp$heat8[i])
## lines(tmp$Heatload.house8[i])

range(data_or$t)
names(data_or)
range(X$t)
names(X)

## The electricity to data_or
tmp <- X[ ,c(1,grep("^el",names(X)))]
names(tmp) <- gsub("el", "Elecload.house", names(tmp))
data <- merge(data_or,tmp, by="t")

unique(diff(data$t))
range(data$t)

tmp <- as.data.list(data)



## Write it
tmp <- data[in_range("2010-01-01", data$t, "2011-01-01"), ]
write.table(tmp, "~/tmp/data_soenderborg_2010.csv", sep=",", row.names=FALSE)
