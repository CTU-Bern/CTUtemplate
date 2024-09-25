CTUtemplate 0.6.0
---------------------
update templates to use DCR-like branding

CTUtemplate 0.5.0
---------------------
addition of template R code for saving data as CSV
addition of example code for using groundhog

CTUtemplate 0.4.7
---------------------
addition of `coloured_text` for using in quarto/RMarkdown documents to more easily colour particular pieces of text

CTUtemplate 0.4.6
---------------------
`use_qmd_*` functions now call quarto under the hood, rather than copying files from github. this fixes a bug with the previous version where the UNIBE logo was not shown.

CTUtemplate 0.4.4
---------------------
fix bug with html-rec quarto template

CTUtemplate 0.4.4
---------------------
fix bug with other quarto templates (figure not loading correctly due to incorrect path)

CTUtemplate 0.4.3
---------------------
fix bug with html presentation (figure not loading correctly due to incorrect path)

CTUtemplate 0.4.2
---------------------
addition of `use_qmd_htmlrecruitment` and `use_qmd_htmlsampsi`

CTUtemplate 0.4.1
---------------------
fix typo in `use_qmd_pres`

CTUtemplate 0.4.0
---------------------
`asr` no issues an error pointing the user to SwissASR
streamline dependencies

CTUtemplate 0.3.2
---------------------
use `here` instead of `setwd` and `file.path`

addition of `use_qmd_html` and `use_qmd_pres` functions to download and save CTUs html quarto templates

CTUtemplate 0.3.1
---------------------
addition of add_ctu_header and RStudio addin

CTUtemplate 0.3.0
---------------------
Reference to `unibeCols` package for UNIBE corporate colours

Addition of administrative info, change log and reproducibility section to report template.

CTUtemplate 0.2.2
---------------------
Markdown templates write output to reports folder by default.

CTUtemplate 0.2.1
---------------------
Addition of vignette on baseline tables.

CTUtemplate 0.2.0
---------------------
BREAKING CHANGE - `asr` no longer exported. Use the SwissASR package instead!

Other changes:
modification to tex template to make the second page have a header when there is no TOC.

CTUtemplate 0.1.6
---------------------
The tex template is now based on pandoc template (more features). UNIBE document class still used to retain UNIBE formatting.

CTUtemplate 0.1.5
---------------------
Removal of restricted figure height/width

CTUtemplate 0.1.4
---------------------
Addition of use_ub_tex_template for allowing modifications to the tex file
Addition of use_recreport_template for recruitment reports (template in dev.)

CTUtemplate 0.1.3
---------------------
Easier handling of CLO files - no longer any need to create it prior to each run

CTUtemplate 0.1.2
---------------------
More details added to the template report use_report_template
dir param in templates is now autofilled
example of parameterized reports

CTUtemplate 0.1.0
---------------------
Addition of LaTeX template that can be used with Rmd
* use_report_template for examples
* use_ssreport_template for sample size reports (based on the word template)

CTUtemplate 0.0.4
---------------------
Addition of OneStageSS for sample size for an exact binomial test
Addition of redcap code for stata

