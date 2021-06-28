#' Open a new report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return
#' @export
#' @importFrom usethis use_template
#'
#' @examples
#' # use_report_template("foo.Rmd")
use_report_template <- function(save_as, open = TRUE, ...){

  use_template(template = "report.Rmd",
               save_as = save_as,
               package = "CTUtemplate",
               ...)

}

#' Open a new sample size report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return
#' @export
#' @examples
#' # use_ssreport_template("ss.Rmd")
use_ssreport_template <- function(save_as, open = TRUE, ...){

  use_template(template = "ssreport.Rmd",
               save_as = save_as,
               package = "CTUtemplate",
               ...)

}



#' Generate the CLO file for use with the UNIBE tex template
#'
#' The CLO file contains the information identifying the author, project and
#' institution (although we do not modify that). It is used by the UNIBE CLS
#' file to create the parts of the template.
#'
#' @param dir Directory in which to create the clo file (should be the same as
#' the Rmd to be knit)
#' @param projnum Project number to appear in report
#' @param projname Project name to appear in report
#' @param reporttype The main title of the report
#' @param version Version number or similar
#' @param sign Your name as it should appear
#' @param email your email address
#' @param job your job title
#' @param open open the file or not
#' @param ... other options passed to \code{use_template}
#'
#' @return
#' @export
#'
#' @examples
#' # use_ubreportclo(dir = "temp", # THE SAME DIR AS THE Rmd FILE
#' #                 # Personal info
#' #                 sign = "Alan",
#' #                 email = "alan.haynes@ctu.unibe.ch",
#' #                 job = "Senior Statistician",
#' #                 # Project info
#' #                 projnum = "1234",
#' #                 projname = "Project X",
#' #                 # Report info
#' #                 version = Sys.Date(),
#' #                 reporttype = "Recruitment report"
#' #                 )
use_ubreportclo <- function(dir,
                            projnum = "xxx",
                            projname = "Project YYY",
                            reporttype = "Type of report",
                            version = "Version",
                            sign = "Author name",
                            email = "author.name@ctu.unibe.ch",
                            job = "Senior Statistician",
                            open = FALSE,
                            ...){

  file <- file.path(dir, "ubreport.clo")

  use_template(template = "ubreport.clo",
               save_as = file,
               data = list(projnum = paste0("{", projnum, "}"),
                           projname = paste0("{", projname, "}"),
                           reporttype = paste0("{", reporttype, "}"),
                           version = paste0("{", version, "}"),
                           sign = paste0("{", sign, "}"),
                           email = paste0("{", email, "}"),
                           job = paste0("{", job, "}")),
               package = "CTUtemplate",
               open = open,
               ...
               )

  # use_template(template = "ubreport.cls",
  #              save_as = file.path(dir, "ubreport.cls"),
  #              data = list(col_logo = paste0("{", find_resource("report", "ub_16pt-cmyk.pdf"), "}}"),
  #                          bw_logo = paste0("{", find_resource("report", "ub_16pt-bl.pdf"), "}}")),
  #              package = "CTUtemplate",
  #              open = open,
  #              ...)

}
