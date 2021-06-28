#' Generate PDF report
#'
#' This is not intended to be run manually... rather via knitting a document
#'
#' @param ... options passed to bookdown
#' @importFrom bookdown pdf_document2
#' @export
report_pdf <- function(...) {
  template_name <- "report.tex"

  # copy cls and clo to new dir
  # file.copy(find_resource("report", "ubreport.clo"),
  #           file.path(getwd(), "ubreport.clo"))
  file.copy(find_resource("report", "ubreport.cls"),
            file.path(getwd(), "ubreport.cls"))
  file.copy(find_resource("report", "ub_16pt-cmyk.pdf"),
            file.path(getwd(), "ub_16pt-cmyk.pdf"))


  report_template <- find_resource("report", template_name)
  base <- bookdown::pdf_document2(template = report_template,
                                  latex_engine = "pdflatex",
                                  citation_package = "biblatex",
                                  keep_tex = TRUE, ...)

  # nolint start
  base$knitr$opts_chunk$comment <- "#>"
  base$knitr$opts_chunk$message <- FALSE
  base$knitr$opts_chunk$warning <- FALSE
  base$knitr$opts_chunk$error <- FALSE
  base$knitr$opts_chunk$echo <- FALSE
  base$knitr$opts_chunk$cache <- FALSE
  base$knitr$opts_chunk$fig.width <- 8
  base$knitr$opts_chunk$fig.asp <- 0.618
  base$knitr$opts_chunk$fig.ext <- "pdf"
  base$knitr$opts_chunk$fig.align <- "center"
  base$knitr$opts_chunk$fig.retina <- 3
  base$knitr$opts_chunk$fig.path <- "figures/"
  base$knitr$opts_chunk$fig.pos <- "H"
  base$knitr$opts_chunk$out.extra <- ""
  base$knitr$opts_chunk$out.width <- "100%"
  base$knitr$opts_chunk$fig.show <- "hold"
  # nolint end

  # if (tolower(apa6) %in% c("true", "yes")) {
  #   base$knitr$knit_hooks$plot <- knitr::hook_plot_tex
  # } else {
  #   base$knitr$knit_hooks$plot <- hook_tex_plot_rat
  # }

  base
}

# adapted from atlas-aai/ratlas
