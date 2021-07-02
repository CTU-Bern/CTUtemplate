# This template serves are a bare bones example of using the report templates
#   for producing parameterized reports - that is, reports depending on
#   different settings. E.g. different patient sets or centers or ...
#
#


for(i in c("foo", "bar")){

  # create the CLO file
  use_ubreportclo("{{dir}}",
                  # Personal info
                  sign = "My name",
                  email = "me@ctu.unibe.ch",
                  job = "Statistician",
                  # Project info
                  projnum = "9999",
                  projname = "Example project",
                  # Report info
                  version = sys.Date(),
                  reporttype = paste("Strata:", i)
                  )
  # you will be asked to approve the overwriting of the CLO on each iteration
  #   of the loop... so don't go too far and check back occasionally...

  # render the reports
  rmarkdown::render("{{Rmd}}", # Rmd file to be compiled
                    # parameters to pass to the Rmd:
                    params = list(example_param = i),
                    # name of the file to output
                    output_file = here::here("{{dir}}", paste0("rep_", i, ".pdf"))
                    )

}





