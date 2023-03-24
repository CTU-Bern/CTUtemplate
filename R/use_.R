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
#' @rdname use_qmd
#' @param save_as filename to save the main qmd as (other filenames are fixed to be compatible with the qmd)
#' @param open logical indicating whether to open the file
#' @export
#' @examples
#' # dir <- tempdir()
#' # file <- file.path(dir, "filename.qmd")
#' # use_qmd_html(file, FALSE)
#' # in practice at CTU:
#' # use_qmd_html("08_Reports_xx/ReportName.qmd", TRUE)
use_qmd_html <- function(save_in = ".", open = TRUE){

  if_no_ping_stop()

  use_quarto("html", save_in, open = open)

}

#' Use the CTU quarto revealjs presentation template
#' This function will download and optionally open the quarto markdown (qmd) file
#' and the other necessary files from github.
#' @describeIn use_qmd revealjs Presentation Template
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
use_qmd_pres <- function(save_in = ".", open = TRUE){

  if_no_ping_stop()

  use_quarto("pres", save_in, open = open)

}


#' Use the CTU quarto html recruitment report template
#' This function will download and optionally open the quarto markdown (qmd) file
#' and the other necessary files from github.
#' @describeIn use_qmd Recruitment Report Template
#' @param save_as filename to save the main qmd as (other filenames are fixed to be compatible with the qmd)
#' @param open logical indicating whether to open the file
#' @export
#' @examples
#' # dir <- tempdir()
#' # file <- file.path(dir, "filename.qmd")
#' # use_qmd_html(file, FALSE)
#' # in practice at CTU:
#' # use_qmd_htmlrecruitment("08_Reports_xx/ReportName.qmd", TRUE)
use_qmd_htmlrecruitment <- function(save_in = ".", open = TRUE){

  if_no_ping_stop()

  use_quarto("html-ss", save_in, open = open)

}

#' Use the CTU quarto html sample size report template
#' This function will download and optionally open the quarto markdown (qmd) file
#' and the other necessary files from github.
#' @describeIn use_qmd Sample Size Report Template
#' @param save_in directory to save the main qmd in
#' @param open logical indicating whether to open the file
#' @export
#' @examples
#' # dir <- tempdir()
#' # file <- file.path(dir, "filename.qmd")
#' # use_qmd_html(file, FALSE)
#' # in practice at CTU:
#' # use_qmd_htmlrecruitment("08_Reports_xx/ReportName.qmd", TRUE)
use_qmd_htmlsampsi <- function(save_in = ".", open = TRUE){

  if_no_ping_stop()

  use_quarto("html-ss", save_in, open = open)

}



#' Get a quarto template from the CTU-Bern quarto repository
#'
#' @param x branch name of the template in question
#' @param dir folder in which to save the template
#' @param open logical, whether to open the file
#'
#' @return a qmd file (and extension file)
#' @export
#'
#' @examples
#' #use_quarto("html", ".")
use_quarto <- function(x, dir, open = TRUE){

  if(!x %in% c("html", "html-ss", "html-rec", "pres")){
    stop("unknown branch of CTU-Bern/quarto")
  }

  wd <- getwd()
  on.exit(setwd(wd))

  setwd(dir)

  system(glue("quarto use template CTU-Bern/quarto@{x} --no-prompt"))

  message("You probably want to rename the .qmd file in dir")

  if(open){

    qmd <- list.files(pattern = "\\.qmd$")

    if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      rstudioapi::navigateToFile(qmd)
    } else {
      utils::file.edit(qmd)
    }
  }
}

if_no_ping_stop <- function(){
  if(!canPingSite("raw.githubusercontent.com")) stop("check internet connection")
}
