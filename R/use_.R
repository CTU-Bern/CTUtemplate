#' Open a new report
#'
#' @param save_as filename to save the Rmd as
#' @param open logical indicating whether to open the file
#' @param ... other arguments passed to \code{use_template}
#'
#' @return Creates an Rmd file in the \code{save_as} location
#' @export
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
#' @param dir directory in which to create the file
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
                            dir = ".",
                            ...){

  file <- file.path(dir, "ubreport.clo")

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


#' Use the CTU quarto html template
#' This function will download and optionally open the quarto markdown (qmd) file
#' and the other necessary files from github.
#' @param save_as filename to save the main qmd as (other filenames are fixed to be compatible with the qmd)
#' @param open logical indicating whether to open the file
#' @export
#' @examples
#' # dir <- tempdir()
#' # file <- file.path(dir, "filename.qmd")
#' # use_qmd_html(file, FALSE)
#' # in practice at CTU:
#' # use_qmd_html("08_Reports_xx/ReportName.qmd", TRUE)
use_qmd_html <- function(save_as = "file.qmd", open = TRUE){

  if(!canPingSite("raw.githubusercontent.com")) stop("check internet connection")
  if(!grepl("qmd$", save_as)){
    warning("file extension missing... adding '.qmd'")
    save_as <- paste0(save_as, ".qmd")
  }

  d <- dirname(save_as)
  if(d == "."){

  } else {
    if(!dir.exists(d)) dir.create(d)
  }

  url <- "https://raw.githubusercontent.com/CTU-Bern/quarto/html"

  # template itself
  download.file(file.path(url, "CTU_html_template.qmd"), save_as)
  # references
  download.file(file.path(url, "references.bib"), file.path(d, "references.bib"))
  # qmd extension
  dir.create(file.path(d, "_extensions"))
  dir.create(file.path(d, "_extensions", "CTU_Bern"))
  dir.create(file.path(d, "_extensions", "CTU_Bern", "qmd-ctuhtml"))
  lapply(c("_extension.yml", "plos-one.csl", "styles.css",
           "ub_Logo_english_2019_RGB_wb.png","unibe.scss"),
         function(x){
           download.file(file.path(url, "_extensions", "qmd-ctuhtml", x),
                         file.path(d, "_extensions", "CTU_Bern", "qmd-ctuhtml", x))
         })

  if (open) {
    if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      rstudioapi::navigateToFile(save_as)
    } else {
      utils::file.edit(save_as)
    }
  }

}

#' Use the CTU quarto revealjs presentation template
#' This function will download and optionally open the quarto markdown (qmd) file
#' and the other necessary files from github.
#' @param save_as filename to save the main qmd as (other filenames are fixed to be compatible with the qmd)
#' @param open logical indicating whether to open the file
#' @export
#' @importFrom utils download.file
#' @examples
#' # dir <- tempdir()
#' # file <- file.path(dir, "filename.qmd")
#' # use_qmd_pres(file, FALSE)
#' # in practice at CTU:
#' # use_qmd_pres("08_Reports_xx/ReportName.qmd", TRUE)
use_qmd_pres <- function(save_as = "file.qmd", open = TRUE){

  if(!canPingSite("raw.githubusercontent.com")) stop("check internet connection")
  if(!grepl("qmd$", save_as)){
    warning("file extension missing... adding '.qmd'")
    save_as <- paste0(save_as, ".qmd")
  }

  d <- dirname(save_as)
  if(d == "."){

  } else {
    if(!dir.exists(d)) dir.create(d)
  }

  url <- "https://raw.githubusercontent.com/CTU-Bern/quarto/pres"
  fmt <- "ctupres"

  # template itself
  download.file(file.path(url, "template.qmd"), save_as)
  # qmd extension
  dir.create(file.path(d, "_extensions"))
  dir.create(file.path(d, "_extensions", "CTU_Bern"))
  dir.create(file.path(d, "_extensions", "CTU_Bern", fmt))
  lapply(c("_extension.yaml", "ub_Logo_english_2019_RGB_wb.png","unibe.scss"),
         function(x){
           download.file(file.path(url, "_extensions", fmt, x),
                         file.path(d, "_extensions", "CTU_Bern", fmt, x))
         })

  if (open) {
    if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      rstudioapi::navigateToFile(save_as)
    } else {
      utils::file.edit(save_as)
    }
  }

}

