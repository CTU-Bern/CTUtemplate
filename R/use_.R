#' Open a new report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return Creates an Rmd file in the \code{save_as} location
#' @export
#' @importFrom usethis use_template
#'
#' @examples
#' # use_report_template("foo.Rmd")
use_report_template <- function(save_as, open = TRUE, ...){

  use_template(template = "report.Rmd",
               save_as = save_as,
               package = "CTUtemplate",
               data = list(dir = dirname(save_as)),
               open = open,
               ...)

}

#' Open a new sample size report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return Creates an Rmd file in the \code{save_as} location
#' @export
#' @examples
#' # use_ssreport_template("ss.Rmd")
use_ssreport_template <- function(save_as, open = TRUE, ...){

  use_template(template = "ssreport.Rmd",
               save_as = save_as,
               package = "CTUtemplate",
               data = list(dir = dirname(save_as)),
               open = open,
               ...)

}


#' Open a new recruitment report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return Creates an Rmd file in the \code{save_as} location
#' @export
#' @examples
#' # use_recreport_template("rr.Rmd")
use_recreport_template <- function(save_as, open = TRUE, ...){

  use_template(template = "recruitmentreport.Rmd",
               save_as = save_as,
               package = "CTUtemplate",
               data = list(dir = dirname(save_as)),
               open = open,
               ...)

}


#' Open a new tex template allowing customization
#'
#' @param save_as filename to save the tex file as (should be the same as in the Rmd header)
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return Creates an tex file in the \code{save_as} location
#' @export
#' @examples
#' # use_ub_tex_template("report.tex")
use_ub_tex_template <- function(save_as, open = TRUE, ...){

  use_template(template = "report.tex",
               save_as = save_as,
               package = "CTUtemplate",
               data = list(dir = dirname(save_as)),
               open = open,
               ...)

}


#' Generate the CLO file for use with the UNIBE tex template
#'
#' The CLO file contains the information identifying the author, project and
#' institution (although we do not modify that). It is used by the UNIBE CLS
#' file to create the parts of the template.
#'
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
#' @return writes the UNIBE template clo file
#' @export
#' @importFrom whisker whisker.render
#' @importFrom fs path_package
#'
#' @examples
#' # use_ubreportclo(# Personal info
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
use_ubreportclo <- function(
                            projnum = "xxx",
                            projname = "Project YYY",
                            reporttype = "Type of report",
                            version = "Version",
                            sign = "Author name",
                            email = "author.name@ctu.unibe.ch",
                            job = "Senior Statistician",
                            open = FALSE,
                            ...){

  file <- file.path("ubreport.clo")

  template <- path_package(package = "CTUtemplate", "templates", "ubreport.clo")

  template <- readLines(template)

  data <- list(projnum = paste0("{", projnum, "}"),
               projname = paste0("{", projname, "}"),
               reporttype = paste0("{", reporttype, "}"),
               version = paste0("{", version, "}"),
               sign = paste0("{", sign, "}"),
               email = paste0("{", email, "}"),
               job = paste0("{", job, "}"))

  writeLines(whisker.render(template, data), file)

}


#' Template files for a parameterized report
#'
#' Parameterized reports allow to create reports in a loop. Examples might be
#' site specific quality reports or recruitment reports.
#'
#' This function creates 2 files as a barebones template for such parameterized
#' reports. An Rmd file is created (based on the UNIBE template) and an R file
#' containing a small loop. Currently, the you have to supervise this loop, as
#' \code{use_ubreportclo} (which relies on \code{usethis::use_template}) will
#' not overwrite files without being given explicit permission.
#'
#' @param save_as filename to save the files as (do not include extension as multiple files are created)
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return saves two files, an Rmd and an R file
#' @export
#'
#' @examples
#' # use_param_report_template("param")
use_param_report_template <- function(save_as, open = TRUE, ...){

  if(grepl(".", save_as, fixed = TRUE))
    warning("file extensions not required for this function",
            "multiple files with different extentions are created")


  use_template(template = "paramreport.Rmd",
               save_as = paste0(save_as, ".Rmd"),
               package = "CTUtemplate",
               data = list(dir = dirname(save_as)),
               open = open,
               ...)
  use_template(template = "paramcode.R",
               save_as = paste0(save_as, ".R"),
               package = "CTUtemplate",
               data = list(dir = dirname(save_as),
                           Rmd = paste0(save_as, ".Rmd")),
               open = open,
               ...)


}



