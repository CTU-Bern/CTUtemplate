
mykeep()
# Load data
# raw <- read.csv(paths$od("data.csv"))
# dat <- read_secuTrial(paths$od("zipname.zip"))
# dat < redcap_export_byform(readLines("O:/tokens/projNum.txt"),
#                            "https://redcap.ctu.unibe.ch/api/")
# alternative REDCap export code. also saves data as CSV
# if(export_date == Sys.Date()){
#   dat <- redcap_export_byform(readLines("O:/tokens/projNum.txt"),
#                               "https://redcap.ctu.unibe.ch/api/")
#   meta <- redcap_export_meta(readLines("O:/tokens/projNum.txt"),
#                              "https://redcap.ctu.unibe.ch/api/")
#   lapply(names(dat), function(x){
#     write.csv(dat[[x]], paths$od(paste0(x, "-", export_date, ".csv")),
#               na = "", row.names = FALSE)
#     return(NULL)
#   })
#   lapply(names(meta), function(x){
#     write.csv(meta[[x]], paths$od(paste0(x, "-", export_date, ".csv")),
#               na = "", row.names = FALSE)
#     return(NULL)
#   })
#   saveRDS(dat, paths$od(paste0("raw-", export_date, ".RDS")))
#   saveRDS(meta, paths$od(paste0("meta-", export_date, ".RDS")))
# }
#
# dat <- readRDS(paths$od(paste0("raw-", export_date, ".RDS")))
# meta <- readRDS(paths$od(paste0("meta-", export_date, ".RDS")))

# example using mtcars
data(mtcars)
dat <- mtcars


# transform variables
dat$am <- factor(dat$am, 0:1, c("Manual", "Automatic"))
dat$am <- as.factor(dat$cyl)
dat$vs <- factor(dat$vs, 0:1, c("V-shaped", "straight"))
dat$am <- as.factor(dat$gear)
dat$am <- as.factor(dat$carb)


# apply labels (Hmisc::label is compatible with atable)
label(dat$am) <- "Transmission"
label(dat$cyl) <- "Number of cylinders"
label(dat$disp) <- "Displacement"
label(dat$hp) <- "Horse power"
label(dat$drat) <- "Rear axle ratio"
label(dat$wt) <- "Weight"
label(dat$qsec) <- "1/4 mile time"

label(dat$vs) <- "Engine"
label(dat$mpg) <- "Miles per gallon"
label(dat$gear) <- "Number of forward gears"
label(dat$carb) <- "Number of carburetors"





# save prepped data
saveRDS(prepped_data, paths$pd("prepped_data"))

# clear the working space
mykeep()












