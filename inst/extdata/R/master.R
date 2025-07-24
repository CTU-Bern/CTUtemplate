

source(paths$rs("01_packages_functions.R"))
source(paths$rs("02_dataprep.R"))
source(paths$rs("03_baseline.R"))
source(paths$rs("04_analysis.R"))

library(quarto)
quarto_render(paths$rd("QMD-FILENAME.qmd"),
              # # additional parameters:
              # execute_params = list(dat = all_data),
              # metadata = list(
              #   # e.g. subtitle (remove subtitle from the qmd header)
              #   subtitle = paste("Recruitment report,", Sys.Date()),
              #   # e.g. quarto (not R) variable that can be used to hide content
              #   foo = list(closed = TRUE))
)
file.copy(paths$rd("QMD-FILENAME.pdf"),
          paths$rd(paste0("OUTPUT-FILENAME",
                          Sys.Date(),
                          ".pdf")), overwrite = TRUE)


# package control through renv ----
# initialize
# renv::init() # just needed once
# save a snapshot of packages and versions (update lockfile)
# renv::snapshot() # occasionally (e.g. when finalizing a report)
# restore versions in current lockfile
# renv::restore() # e.g. if something broke after updating package(s)

