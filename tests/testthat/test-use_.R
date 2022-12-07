

dir <- tempdir()

test_that("use_report_template", {
  use_report_template(file.path(dir, "rep.Rmd"), open = FALSE)
  expect_true(file.exists(file.path(dir, "rep.Rmd")))
})

test_that("use_ssreport_template", {
  use_ssreport_template(file.path(dir, "ss.Rmd"), open = FALSE)
  expect_true(file.exists(file.path(dir, "ss.Rmd")))
})

test_that("use_recreport_template", {
  use_recreport_template(file.path(dir, "rec.Rmd"), open = FALSE)
  expect_true(file.exists(file.path(dir, "rec.Rmd")))
})

test_that("use_ub_tex_template", {
  use_ub_tex_template(file.path(dir, "tex.tex"), open = FALSE)
  expect_true(file.exists(file.path(dir, "tex.tex")))
})

test_that("use_ubreportclo", {
  use_ubreportclo(dir = dir, open = FALSE)
  expect_true(file.exists(file.path(dir, "ubreport.clo")))
})

test_that("use_param_report_template", {
  use_param_report_template(file.path(dir, "params"), open = FALSE)
  expect_true(file.exists(file.path(dir, "params.Rmd")))
  expect_true(file.exists(file.path(dir, "params.R")))
})


