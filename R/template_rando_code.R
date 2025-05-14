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
armlabels <- c("TREATLABEL1", "TREATLABEL2")
stratalabels1 <- c("STRATALABEL1", "STRATALABEL2")
# add extra stratalabels objects as necessary

r <- randolist(n_per_stratum, blocksizes = 1:3,
               arms = armlabels,
               strata = list(STRATAVAR = stratalabels1))

rlist <- randolist_to_db(r, target_db = "REDCap",
                rando_enc = data.frame(arm = unique(armlabels),
                                       RANDORESVAR = 1:2),
                strata_enc = list(
                  STRATAVAR = dataframe(STRATAVAR = stratalabels1,
                                        code = 1:2)
                                  )
                         )

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
