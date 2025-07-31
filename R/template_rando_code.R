#' Template randomisation list code
#'
#' Open a file with template code to generate a randomisation list (and report).
#'
#' This must be used within RStudio.
#'
#' @returns an R script
#' @export
#'
#' @examples
#'
#' # template_rando_code()
template_rando_code <- function(){

  rstudioapi::documentNew(
    paste(glue::glue(
                           '
# /* ************************************************** */
# /* Study: #### PROJECTNAME                            */
# /* Purpose: Create randomisation list                 */
# /* Author: AUTHOR                                     */
# /* Date created: {Sys.Date()}                           */
# /* Last update:                                       */
# /* Underlying SAP version: NA                         */
# /* Underlying Study Protocol version: ###             */
# /* ****************************************************/
'
), '


# TO BE CHANGED BY DATA MANAGERS
# relevant folder (i.e. 01_TestRandomization or 02_ProductiveRandomization)
FOLDER <- "01_TestRandomization" # "02_ProductiveRandomization"
# seed for reproducibility
seed <- 1234




# DMs: DO NOT TOUCH
projnum <- ####
projname <- "PROJECTNAME"
setwd(glue::glue("R://Clinical studies//{projnum}_{projname}//10_Randomisation_Decoding_{projnum}//{FOLDER}_{projnum}"))

# packages
# remotes::install_github("CTU-Bern/randotools")
library(randotools)


n_per_stratum <- N # number of randos per strata

# arms and their equivalent values/variable name in the database
rando_enc <- data.frame(
  arm = c("TREATLABEL1", "TREATLABEL2"),
  RANDORES = 1:2
  )

strata_enc <- list(
  STRATAVAR = data.frame(
    STRATAVAR = c("STRATALABEL1", "STRATALABEL2"),
    code = 1:2
  )
  # add additional STRATAVAR dataframes as necessary
)

strata_labs <- lapply(strata_enc, `[[`, 1)

r <- randolist(n_per_stratum, blocksizes = 1:3,
               arms = rando_enc$arm,
               strata = strata_labs)

rlist <- randolist_to_db(r, target_db = "REDCap",
                         rando_enc = rando_enc,
                         strata_enc = strata_enc)

# save the list
write.csv(r,
          file.path(glue::glue("02_list_{projnum}"), "complete_randolist.csv"),
          row.names = FALSE)
write.csv(rlist,
          file.path(glue::glue("02_list_{projnum}"), "randolist_for_redcap.csv"),
          row.names = FALSE)

# report
capture.output(file = file.path(glue::glue("03_report_{projnum}"),
                                paste0("rando_report", Sys.Date(), ".txt")),
  cat("Report on Randomization List for Clinical Trial\\n"),
  cat(glue::glue("{projnum} {projname}\\n\\n")),
  cat("\\n\\n"),
  summary(r),
  cat("\\n\\n\\n\\n"),
  cat("------ Session Info ------\\n"),
  sessionInfo()
)



'))


}
