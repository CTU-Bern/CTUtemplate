

source(paths$rs("01_packages_functions.R"))
source(paths$rs("02_dataprep.R"))
source(paths$rs("03_baseline.R"))
source(paths$rs("04_analysis.R"))


# package control through renv ----
# initialize
# renv::init() # just needed once
# save a snapshot of packages and versions (update lockfile)
# renv::snapshot() # occasionally (e.g. when finalizing a report)
# restore versions in current lockfile
# renv::restore() # e.g. if something broke after updating package(s)

