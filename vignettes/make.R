
library(knitr)
library(rmarkdown)

# Put the files in this dir (ignored in the git)
dirnam <- "../tmp/vignettes/"
dir.create("../tmp")
dir.create(dirnam)


makeit <- function(nam, openit=FALSE, clean=TRUE){
    namrmd <- paste0(nam,".Rmd")
    render(namrmd, output_file=paste0(dirnam,nam), clean=clean)
    purl(namrmd)
    system(paste0("mv ",nam,".R ",dirnam,nam,".R"))
    if(openit){ system(paste0("chromium-freeworld ",dirnam,nam,".html &")) }
}

#
unlink(paste0(dirnam,"tmp-setup-data/"), recursive=TRUE)
makeit("setup-data", openit=FALSE)

#
unlink(paste0(dirnam,"tmp-setup-and-use-model/"), recursive=TRUE)
makeit("setup-and-use-model", openit=FALSE, clean=TRUE)

#
unlink(paste0(dirnam,"tmp-forecast-evaluation/"), recursive=TRUE)
makeit("forecast-evaluation", openit=FALSE)

#
unlink(paste0(dirnam,"tmp-model-selection/"), recursive=TRUE)
makeit("model-selection", openit=FALSE)

# 
unlink(paste0(dirnam,"tmp-output/tmp-online-updating/"), recursive=TRUE)
makeit("online-updating", openit=FALSE)
