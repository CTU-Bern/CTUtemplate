
# install missing packages (leave commented out) ----
# install.packages("presize")
# install.packages("presize", repos = "https://ctu-bern.r-universe.dev") # dev version
# install.packages("accrualPlot", repos = "https://ctu-bern.r-universe.dev")
# install.packages("kpitools", repos = "https://ctu-bern.r-universe.dev")
# install.packages("btabler", repos = "https://ctu-bern.r-universe.dev")
# install.packages("SwissASR", repos = "https://ctu-bern.r-universe.dev")
# install.packages("unibeCols", repos = "https://ctu-bern.r-universe.dev")
# install.packages("redcaptools", repos = "https://ctu-bern.r-universe.dev")
# install.packages("secuTrialR")

# install.packages("groundhog")
# library.groundhog(" #UNCOMMENT TO USE GROUNDHOG

## Load packages ----
# library(atable)
# atable_options(format_to = "console", add_margins = TRUE)
# library(renv)
# library(tidyverse)
# library(Hmisc)
# library(broom)
# library(quarto)

## CTU Bern/SCTO packages ----
# library(secuTrialR)
# library(redcaptools)
# library(accrualPlot)
# library(SwissASR)
# library(presize)
# library(unibeCols)
# library(svn)
# library(kpitools)
# library(btabler)
# library(sse)

# ", date = "2024-02-01") #UNCOMMENT TO USE GROUNDHOG

# custom functions ----
# function to retain only named objects (+ 'paths', pp and functions)
mykeep <- function(...){
  lss <- ls(.GlobalEnv)
  lss_fun <- sapply(lss, function(x) is.function(eval(parse(text = x))))
   gdata::keep(paths, mykeep, ..., list = unlist(lss)[unlist(lss_fun)],
      sure = TRUE)
}
