

# Load packages
library("atable")
atable_options(format_to = "console", add_margins = TRUE)
library("tidyverse")
library("here")
library("renv")
library("Hmisc")
library(broom)



# custom functions ----
# function to retain only named objects (+ 'paths', pp and functions)
mykeep <- function(...){
  lss <- ls(.GlobalEnv)
  lss_fun <- sapply(lss, function(x) is.function(eval(parse(text = x))))
   gdata::keep(paths, pp, mykeep, ..., list = unlist(lss)[unlist(lss_fun)],
      sure = TRUE)
}
