library(knitr)
library(rmarkdown)

## Put the files in this dir (ignored in the git)
dirnam <- "tmp-output/"
dir.create(dirnam)

makeit <- function(nam, openit=FALSE, clean=TRUE){
    namrmd <- paste0(nam,".Rmd")
    render(namrmd, output_file=paste0(dirnam,nam), clean=clean)
    purl(namrmd)
    system(paste0("mv ",nam,".R ",dirnam,nam,".R"))
    if(openit){ system(paste0("chromium-browser ",dirnam,nam,".html &")) }
}

file.remove(dir("tmp-output/tmp-setup-data/", full.names=TRUE))
makeit("setup-data", openit=FALSE)

#
file.remove(dir("cache", full.names=TRUE))
file.remove("cache")
file.remove(dir("tmp-output/tmp-setup-and-use-model/", full.names=TRUE))

makeit("setup-and-use-model", openit=FALSE, clean=FALSE)
oi

#
file.remove(dir("tmp-output/tmp-forecast-evaluation/", full.names=TRUE))
makeit("forecast-evaluation", openit=FALSE)

#
file.remove(dir("tmp-output/tmp-online-updating/", full.names=TRUE))
makeit("online-updating", openit=FALSE)
