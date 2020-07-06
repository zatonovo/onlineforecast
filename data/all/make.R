# setting work directory and libraries #
rm(list = ls())

# # Packages used
# require(R6)
require(data.table)
# require(Rcpp)
# require(splines)
library(devtools)
library(roxygen2)

pack <- as.package("../../../onlineforecast")
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
#Dbuilding <- subset(data, c("2010-12-15","2011-03-01"), nms=c("t","Heatload","Ta","I","Ws","Wd","Ta.obs","I.obs","Wd.obs","Ws.obs","cosAoi","sunElevation","tday"))
data$heatload <- data$Heatload$house9
Dbuilding <- subset(data, c("2010-12-15","2011-03-01"), nms=c("t","heatload","heatloadtotal","Ta.obs","I.obs","Ta","I"))
rownames(Dbuilding$Ta) <- NULL
Dbuilding$Ta <- Dbuilding$Ta[ ,1:36]
rownames(Dbuilding$I) <- NULL
Dbuilding$I <- Dbuilding$I[ ,1:36]
#
usethis::use_data(Dbuilding, overwrite=TRUE)
