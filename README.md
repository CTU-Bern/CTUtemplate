<!-- README.md is generated from README.Rmd. Please edit that file -->

`CTUtemplate` <img src='man/figures/sticker.png' align="right" height="200">
============================================================================

`CTUtemplate` is a package to create a template directory structure (and
files) and also includes the CTUs annual safety report function.

Installation
------------

    remotes::install_github("CTU-Bern/CTUtemplate")

This may require `Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")`
if packages were built under a different R version to the one you are
using.

Usage
-----

    library(CTUtemplate)

### Template director structure

Once installed, in RStudio, open a “new project in a new directory” and
select “CTU project template”. Options in the following window are used
to create the folders and headers of R (and eventually STATA) files.

### Annual safety report

THIS FUNCTION WILL BE REMOVED FROM THIS PACKAGE IN THE NEAR FUTURE

The `asr` function is used to fill out the Swiss Ethics Annual Safety
Report template. It is intended that by passing a dataframe with the
relevant information, plus a few other values, filling the template can
be automated.

Assuming that all data variables are consistent with the defaults, the
following might be used to fill out the report.

    asr(dataframe,
        trial_title = "Trial Name",
        protocol_number = "1",
        basec_number = "2153153",
        snctp_number = "3516135468",
        swissmedic_number = "kuis16153613",
        ec_name = "KEK Bern",
        product_name = "Drug name",
        sponsor_contact = "Mr Foo, University of Foo, Bar", 
        inst_name_address = "Mr Bar, University of Foo, Bar",
        n_centers_t = length(unique(alldata$site)),      # total number of sites
        n_centers_p = 15,                                # number of planned sites
        n_centers_c = length(unique(closedsites$site)),  # number of closed sites
        n_centers_o = length(unique(alldata$site)),      # number of open sites
        n_pat_t = 1000,                                  # total number of participants
        n_pat_e = length(unique(alldata$record_id)),     # enrolled participants
        n_pat_c = sum(all_data$eos_complete),            # complete participants
        n_pat_p = sum(all_data$eos_terminated),          # prematurely terminated participants
        period_from = min(dataframe$ic_date),            # assuming that the variable exists!
        period_to = Sys.Date()
        )

### UNIBE Red

There is also a function for the UNIBEs shade of red:

    par(mai = c(.5, .1, .1, .1), tck = -.01, mgp = c(1,.2,0))
    plot(
      # main point:
      col = unibeRed(alpha = seq(.2, 1, .2)), 
      # other stuff for the figure
      x = seq(.2, 1, .2), y = rep(1,5), 
      pch = 15, cex = 5, xlab = "alpha", ylab = "", 
      yaxt = "n")

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

### Report templates

`CTUtemplate` has an Rmd template for sample size reports and a more
generic template with some examples of how to do things.

<table>
<colgroup>
<col style="width: 40%" />
<col style="width: 59%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Function</th>
<th style="text-align: left;">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>use_report_template</code></td>
<td style="text-align: left;">Opens a generic file with various examples</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>use_ssreport_template</code></td>
<td style="text-align: left;">Opens a template for a sample size report</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>use_recreport_template</code></td>
<td style="text-align: left;">Opens a template for a recruitment report</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>use_param_report_template</code></td>
<td style="text-align: left;">Opens template files for using parameterized reports</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>use_ub_tex_template</code></td>
<td style="text-align: left;">Opens the UNIBE tex template allowing modifications for additional features of latex</td>
</tr>
</tbody>
</table>

The functions are used to open a new template in the location designated
(the recommended location would probably be `08_Reports_projnum`).

    # for a sample size report
    use_ssreport_template("folder/ssreport.Rmd")
    # for the examples
    use_report_template("folder/report.Rmd")
    # for a template recruitment report
    use_recreport_template("folder/recreport.Rmd")
    # for an example of a parameterized report (note the lack of file extension here)
    use_param_report_template("folder/param_report")

If modifications to the latex template are desired, it can be copied to
the folder via

    use_ub_tex_template("folder/report.tex")

The YAML header in the Rmd file then needs to be changed to include

    output: 
      CTUtemplate::report_pdf:
        report_template: "report.tex"

Using this approach, other latex packages can be used to extend the
capabilities of latex.
