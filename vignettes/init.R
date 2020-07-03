# ---- init

# Width will scale all
figwidth <- 12
# Scale the wide figures (100% out.width)
figheight <- 4
# Heights for stacked time series plots
figheight1 <- 5
figheight2 <- 6.5
figheight3 <- 8
figheight4 <- 9.5
figheight5 <- 11
# Set the size of squared figures (same height as full: figheight/figwidth)
owsval <- 0.35
ows <- paste0(owsval*100,"%")
ows2 <- paste0(2*owsval*100,"%")
# 
fhs <- figwidth * owsval

# Set for square fig: fig.width=fhs, fig.height=fhs, out.width=ows}
# If two squared the:  fig.width=2*fhs, fig.height=fhs, out.width=ows2

# Check this: https://bookdown.org/yihui/rmarkdown-cookbook/chunk-styling.html
# Set the knitr options
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##    ",
  prompt = FALSE,
  cache = TRUE,
  cache.path = paste0("tmp-output/tmp-",vignettename,"/"),
  fig.align="center",
  fig.path = paste0("tmp-output/tmp-",vignettename,"/"),
  fig.height = figheight,
  fig.width = figwidth,
  out.width = "100%"
)
options(digits=3)

hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "## ...output cropped"
  if (length(lines)==1) {        # first n lines
    if (length(x) > lines) {
      # truncate the output, but add ....
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(more, x[lines], more)
  }
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})
