
mykeep()
# Load data
raw <- read.csv(paths$od("data.csv"))
dat <- secuTrialR::read_secuTrial(paths$od("zipname.zip"))


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












