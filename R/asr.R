



# wdfile <- system.file("extdata/clino_annual_safety_report_bm.docx", package = "CTUtemplate")
#
#
# def <- "defaut"
#
# trial_title <- def
# protocol_number <- def
# basec_number <- def
# snctp_number <- def
# swissmedic_number <- def
# ec_name <- def
# product_name <- def
# sponsor_contact <- "default name, default number, default email"
# inst_name_address <- "default name, default address"
# n_centers_t <- def # total
# n_centers_p <- def #planned
# n_centers_c <- def #closed
# n_centers_o <- def #open
# n_pat_t <- def #target
# n_pat_e <- def #enrolled
# n_pat_c <- def #complete
# n_pat_p <- def #prematurely terminated
#
#
# last_report <- as.Date("2020-11-01")
#
# var_class <- "class" #sae/sadr/susar
# var_sae_n <- "sae_n" #sae ID
# var_part_id <- "record_id" #participant ID
# var_age <- "age"
# var_sex <-"sex"
# var_country <- "country"
# var_site <- "site"
# var_sae <- "sae"
# var_date_onset <- "sae_date"
# var_date_trt_start <- "sae_trtstart"
# var_date_trt_start <- "sae_trtstop"
# var_outcome <- "outcome"
# var_comment <- "comment"
#
#
# report_date <- format(Sys.Date(), format = "%d/%m/%Y")
# period_from <- "start_date"
# period_to <- "to_date"





# prepare table specific variables


# asr(sae_data)
# asr(sae_data, var_class = "sae_type")
#
# with(sae_data, asr(class = sae_type, ))


check_fac_char <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor", "character") %in% class(x))) stop("'", var, "' should be factor or character")
}
check_fac_char_log_int <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor", "character", "logical", "integer") %in% class(x))) stop("'", var, "' should be factor, character, logical or integer")
}
check_date <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("Date", "POSIXt") %in% class(x))) stop("'", var, "' should be Date or POSIX")
}
check_fac <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor") %in% class(x))) stop("'", var, "' should be factor")
}

#' Title
#'
#' @param data dataframe containing safety data
#' @param target filename to save the report to
#' @param trial_title name of the trial
#' @param protocol_number trial code/protocol number
#' @param basec_number BASEC number
#' @param snctp_number SNCTP number
#' @param swissmedic_number Swissmedic number
#' @param ec_name EC name (Lead EC and/concerned EC)
#' @param product_name product name or intervention
#' @param sponsor_contact contact details of sponsor(-investigator)
#' @param inst_name_address name and address of institute
#' @param n_centers_t total number of participating centres
#' @param n_centers_p planned number of participating centres
#' @param n_centers_c number of closed centres
#' @param n_centers_o number of open centres
#' @param n_pat_t target number of participants
#' @param n_pat_e number of enrolled participants
#' @param n_pat_c number of completed participants
#' @param n_pat_p number of prematurely terminated participants
#' @param report_date report date
#' @param period_from start of reporting period
#' @param period_to end of reporting period
#' @param template path to template file
#' @param international internation or national trial (logical)
#' @param trial_type one of \code{imp}, \code{medical device}, \code{other}. Abbreviations OK.
#' @param var_class variable containing SAE class. Options allowed are "SAE", "SADR", "SUSAR"
#' @param var_sae_n variable containing SAE ID
#' @param var_part_id variable containing participant ID
#' @param var_age variable containing participant age
#' @param var_sex variable containing participant sex
#' @param var_country variable containing participants country
#' @param var_site variable containing participants site
#' @param var_sae variable containing SAE type (description)
#' @param var_date_onset variable containing the date the SAE occured
#' @param var_trt variable containing the treatment used to alleviate the SAE
#' @param var_date_trt_start variable containing the date that the treatment started
#' @param var_date_trt_stop variable containing the date that the treatment ended
#' @param var_outcome variable containing the outcome of the SAE
#' @param var_comment variable containing any comment
#' @param var_relation variable containing the relationship to randomized intervention
#' @param var_expected variable saying whether the SAE was expected
#' @param var_devdef variable containing whether the SAE is a device deficiency
#' @param var_devattr variable containing whether the SAE is attributable to the device
#'
#' @return
#' @export
#' @import officer
#' @import flextable
#' @importFrom magrittr %>%
#' @examples
#'
#'
#' # demo data
#' last_report <- as.Date("2020-11-01")
#' n <- 20
#'
#' msample <- function(x, ...) sample(x, n, TRUE, ...)
#' sae_data <- data.frame(sae_date = last_report + msample(-20:20),
#'                        record_id = msample(1:300),
#'                        age = round(runif(n, 45, 75)),
#'                        sex = factor(msample(c(c("M", "F")))),
#'                        country = factor(msample("CH")),
#'                        site = factor(msample(letters[1:15])),
#'                        sae = factor(msample(c("headache", "diarhea", "spontaneous unconsciousness", "death", "poor sleep"))),
#'                        intervention = factor(msample(c("grp1", "grp2"))),
#'                        outcome = factor(msample(c("resolved", "fatal", "improved", "sequel", "unknown"))),
#'                        comment = msample(c("", "fribble", "foo", "bar", "foobar")),
#'                        trt = msample(c("", "trt1", "trt2", "trt3", "trt4")),
#'                        class = msample(c("SAE", "SUSAR", "SADR")),
#'                        expected = msample(c(TRUE, FALSE)),
#'                        devdef = msample(c(TRUE, FALSE)),
#'                        devattr = msample(c(TRUE, FALSE)),
#'                        devdef = msample(c(TRUE, FALSE)),
#'                        devint = msample(c(TRUE, FALSE))
#' )
#' sae_data <- sae_data[order(sae_data$sae_date), ]
#' sae_data$sae_n <- 1:nrow(sae_data)
#' sae_data$sae_trtstop <- as.Date(ifelse(sae_data$outcome %in% c("resolved", "fatal") & sae_data$trt != "", sae_data$sae_date + runif(2:10, 1), NA), origin = "1970-01-01")
#' sae_data$sae_trtstart <- as.Date(ifelse(sae_data$trt != "", sae_data$sae_date + 1, NA), origin = "1970-01-01")
#' sae_data$related <- rbinom(n, 1, .25)
#'
#' sae_data$sae_date
#' # data <- sae_data
#'
#' asr(sae_data)
#' asr(sae_data, trial_type = "med")
#'
#'
#'
#'
#'
#'
#'

asr <- function(data,
                target = "tmp.docx",
                trial_title = "TRIAL NAME"
                , protocol_number = "default"
                , basec_number = "default"
                , snctp_number = "default"
                , swissmedic_number = "default"
                , ec_name = "default"
                , product_name = "default"
                , sponsor_contact = "default name, default number, default email"
                , inst_name_address = "default name, default address"
                , n_centers_t = "default" # total
                , n_centers_p = "default" #planned
                , n_centers_c = "default" #closed
                , n_centers_o = "default" #open
                , n_pat_t = "default" #target
                , n_pat_e = "default (300)" #enrolled
                , n_pat_c = "default" #complete
                , n_pat_p = "default" #prematurely terminated
                , report_date = format(Sys.Date(), format = "%d/%m/%Y")
                , period_from = as.Date("2020-11-02")
                , period_to = as.Date("2020-11-17")
                , template = system.file("extdata/clino_annual_safety_report_bm.docx", package = "CTUtemplate")
                , international = FALSE
                , trial_type = "imp"
                # dataframe variables
                , var_class = "class"
                , var_sae_n = "sae_n" #sae ID
                , var_part_id = "record_id" #participant ID
                , var_age = "age"
                , var_sex ="sex"
                , var_country = "country"
                , var_site = "site"
                , var_sae = "sae"
                , var_date_onset = "sae_date"
                , var_trt = "trt"
                , var_date_trt_start = "sae_trtstart"
                , var_date_trt_stop = "sae_trtstop"
                , var_outcome = "outcome"
                , var_comment = "comment"
                , var_relation = "related"
                , var_expected = "expected"
                , var_devdef = "devdef"
                , var_devattr = "devattr"
                , var_devint = "devint"
                , var_safteymeasure = "safetymeasure"
){

  # general housekeeping ----
  trial_type <- match.arg(trial_type, c("imp", "medical device", "other"))

  ## all line listing variables exist ----
  if(!all(c(var_class, var_sae_n, var_part_id, var_age, var_sex,
            var_country, var_site, var_sae, var_trt, var_date_onset,
            var_date_trt_start, var_date_trt_stop, var_outcome,
            var_comment) %in% names(data))) stop("one or more variables for line listing missing")

  ## rename variables for easier code ----
  names(data)[names(data) == var_class] <- "class"
  names(data)[names(data) == var_sae_n] <- "sae_n"
  names(data)[names(data) == var_part_id] <- "record_id"
  names(data)[names(data) == var_age] <- "age"
  names(data)[names(data) == var_sex] <- "sex"
  if(international) names(data)[names(data) == var_country] <- "country"
  names(data)[names(data) == var_site] <- "site"
  names(data)[names(data) == var_sae] <- "sae"
  names(data)[names(data) == var_date_onset] <- "sae_date"
  names(data)[names(data) == var_date_trt_start] <- "sae_trtstart"
  names(data)[names(data) == var_date_trt_stop] <- "sae_trtstop"
  names(data)[names(data) == var_outcome] <- "outcome"
  names(data)[names(data) == var_comment] <- "comment"
  names(data)[names(data) == var_relation] <- "related"
  names(data)[names(data) == var_trt] <- "trt"

  ## relevant variables exist
  if(trial_type == "imp"){
    ## relatedness ----
    check_fac_char_log_int(data$related)
    if(is.character(data$related) | is.factor(data$related)){
      if(!all(stringr::str_to_lower(data$related) %in% c("yes", "no")))
        stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
      data$related <- stringr::str_to_lower(as.character(data$related)) == "yes"
    }
    if(is.integer(data$related)){
      if(!all(data$related %in% 0:1))
        stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
      data$related <- data$related == 1
    }
    related_data <- data[data$related, ]

  }
  if(trial_type == "medical device"){
    names(data)[names(data) == var_expected] <- "expected"
    names(data)[names(data) == var_devdef] <- "devdef"
    names(data)[names(data) == var_devattr] <- "dev_attr"
    names(data)[names(data) == var_devint] <- "dev_int"
    names(data)[names(data) == var_safteymeasure] <- "safetymeasure"

    ## safetymeasure ----
    check_fac(data$safetymeasure)
  }
  if(trial_type == "other"){

  }


  ## class ----
  check_fac_char(data$class)
  if(!all(data$class %in% c("SAE", "SADR", "SUSAR")))
    stop("unexpected SAE types encountered (allowed are SAE, SADR and SUSAR)")
  data$class <- as.character(data$class)

  ## age/sex ----
  if(!"numeric" %in% class(data$age))
    stop("'age' should be numeric")
  check_fac_char(data$sex)
  data$sex <- as.character(data$sex)
  ## recode
  data$sex[grepl("^[WwFf]", data$sex)] <- "F"
  data$sex[grepl("^[Mm]", data$sex)] <- "M"
  if(!all(data$sex %in% c("M", "F")))
    stop("unexpected sexes encountered")
  data$agesex <- glue::glue("{sprintf('%1.0f', data$age)}/{data$sex}")

  ## country/site ----
  check_fac_char(data$site)
  data$site <- as.character(data$site)
  if(international){
    check_fac_char(data$country)
    data$country <- as.character(data$country)
    data$country_site <- glue::glue("{data$country}, {data$site}")
  } else {
    data$country_site <- glue::glue("{data$site}")
  }

  ## dates ----
  check_date(data$sae_date)
  check_date(data$sae_trtstart)
  check_date(data$sae_trtstop)
  check_date(period_from)
  check_date(period_to)


  period_data <- subset(data, sae_date >= period_from & sae_date <= period_to)


  data$sae_date <- format(data$sae_date, format = "%d/%m/%Y")
  data$sae_trtstart <- format(data$sae_trtstart, format = "%d/%m/%Y")
  data$sae_trtstop <- format(data$sae_trtstop, format = "%d/%m/%Y")
  data$trt_dates <- ifelse(is.na(data$sae_trtstop),
                           glue::glue("{data$sae_trtstart}", .na = ""),
                           glue::glue("{data$sae_trtstart} - {data$sae_trtstop}", .na = ""))

  ## outcome ----
  check_fac_char(data$outcome)
  data$outcome <- as.character(data$outcome)
  data$outcome <- stringr::str_to_sentence(data$outcome)
  if(!all(data$outcome %in% c("Resolved", "Fatal", "Improved", "Sequel", "Unknown")))
    stop("unexpected outcomes encountered (Resolved, Fatal, Improved, Sequel and Unknown are allowed)")




  ## comment ----
  check_fac_char(data$comment)
  data$comment <- as.character(data$comment)

  ## sae ----
  check_fac_char(data$sae)
  data$sae <- as.character(data$sae)

  ## trt ----
  check_fac_char(data$trt)
  data$trt <- as.character(data$trt)


  # report itself ----
  doc <- read_docx(template)



  # ..general info, details of trial ----
  period <- glue::glue("{period_from} to {period_to}")

  for(i in c("trial_title",
             "protocol_number",
             "basec_number",
             "snctp_number",
             "swissmedic_number",
             "ec_name",
             "product_name",
             "sponsor_contact",
             "inst_name_address",
             "report_date",
             "period",
             "n_centers_t",
             "n_centers_p",
             "n_centers_c",
             "n_centers_o",
             "n_pat_t",
             "n_pat_e",
             "n_pat_c",
             "n_pat_p")){
    x <- get(i)
    doc <- doc %>%
      cursor_bookmark(i) %>%
      body_add_par(x, pos = "after")
  }





  ## participant safety ----
  map <- data.frame(
    col_key = c("x1", "x2", "x3", "x4", "x5"),
    name = c('', 'Fatal cases', 'Serious Adverse Events, SAEs', 'Serious Adverse Drug Reactions, SADRs (only for IMPs)', 'Suspected Unexpected Serious Adverse Reactions, SUSARs (only for IMPs)'),
    stringsAsFactors = FALSE)

  if(n_pat_e == "default (300)") n_pat_e <- 300
  n_pat_e <- as.numeric(n_pat_e)
  ### IMP ----
  if(trial_type == "imp"){

    tab <- table(data$sae[data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")


    doc <- doc %>%
      cursor_bookmark("partsafety_text") %>%
      body_add_par(glue::glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs)."), style = "Text") %>%
      body_add_par(glue::glue("{sum(data$related)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$related)/nrow(data)*100)} %) were classified “related” to the IMP. The most frequent related SAEs documented were {most_freq}."), style = "Text") %>%
      body_add_par(glue::glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period, which have been notified to the Swiss competent authorities."), style = "Text")

    tab <- tibble::tribble(
      ~x1, ~x2, ~x3, ~x4, ~x5,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      sum(period_data$class == "SADR"),
      sum(period_data$class == "SUSAR"),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data),
      sum(data$class == "SADR"),
      sum(data$class == "SUSAR")
    )



  }
  ### medical device ----
  if(trial_type == "medical device"){

    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    doc <- doc %>%
      cursor_bookmark("partsafety_text") %>%
      body_add_par(glue::glue("During the reporting period, a total of {nrow(period_data)} serious adverse events (SAEs) have been reported."), style = "Text") %>%
      body_add_par(glue::glue("In {sum(data$dev_attr)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_attr)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to the medical device under investigation."), style = "Text") %>%
      body_add_par(glue::glue("In {sum(data$dev_int)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_int)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to an intervention undertaken in the clinical trial."), style = "Text") %>%
      body_add_par(glue::glue("The most frequent SAEs documented were {most_freq}."), style = "Text") %>%
      body_add_par(glue::glue("With respect to the expectedness of the event, {sum(data$expected)} ({sprintf('%1.1f', sum(data$expected)/nrow(data)*100)} %) of the SAEs were expected and {sum(!data$expected)} ({sprintf('%1.1f', sum(!data$expected)/nrow(data)*100)} %) were classified as unexpected."), style = "Text") %>%
      body_add_par(glue::glue("{sum(data$devdef)} device deficiencies were observed."), style = "Text") %>%
      body_add_par(glue::glue("{sum(safetymeasure)} health hazards that required safety-related measures occurred."), style = "Text")


    tab <- tibble::tribble(
      ~x1, ~x2, ~x3, ~x4, ~x5,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      NA,
      NA,
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data),
      NA,
      NA
    )

  }

  ### other ----
  if(trial_type == "other"){
    doc <- doc %>%
      cursor_bookmark("partsafety_text") %>%
      body_add_par(glue::glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs; with possible relationship to the intervention)."), style = "Text") %>%
      body_add_par(glue::glue("The most frequent SAEs documented were {most_freq}."), style = "Text")

    tab <- tibble::tribble(
      ~x1, ~x2, ~x3, ~x4, ~x5,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      NA,
      NA,
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data),
      NA,
      NA
    )

  }

  ft <- tab %>%
    flextable() %>%
    set_header_df(mapping = map, key = "col_key") %>%
    valign(part = "head", valign = "top") %>%
    border(border = fp_border(color = "#4FB4E0"), part="all") %>%
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Helvetica", part = "all") %>%
    width(1:5, (18.57/5)*.393)
  doc <- doc %>%
    cursor_bookmark("partsafety_tab") %>%
    body_add_flextable(ft)




  ## line listing ----
  llist <- data[, c("class", "sae_n", "record_id", "agesex", "country_site",
                    "sae", "trt", "sae_date", "trt_dates", "outcome", "comment")]
  ## llist <- llist[c(1,1:nrow(llist)), ]
  names(llist) <- c("SAE/\nSADR/\nSUSAR",
                    "Serious adverse event/ reaction No.",
                    "Participants ID",
                    "Age / Sex (F=female, M=male)",
                    "Country and site in which participant is/was enrolled (for multicentre, international trials)",
                    "Description of event/ reaction",
                    "Description of intervention (dosage, schedule, route, if applicable)",
                    "Date of onset",	"Date of treatment (start and stop)",
                    "Outcome (e.g. resolved, fatal, improved, sequel, unknown)",
                    "Comments, if relevant (e.g. causality assessment, relationship)"
  )

  llist_ft <- flextable(llist) %>%
    add_header_lines("Line listing of SAEs, SADRs and SUSARs, including international cases
(code and version of used standard (e.g. MedDRA or CTCAE) should be indicated, details on SUSARs will be attached as appendices)") %>%
    border(border = fp_border(color = "#4FB4E0"), part="all") %>%
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Helvetica", part = "all")


  doc <- doc %>%
    cursor_bookmark("llist") %>%
    body_add_flextable(llist_ft)

  print(doc, target = target)


}







