
mykeep()

# Load data
dat <- readRDS(paths$pd("prepped_data"))


# run a model
mod <- lm(mpg ~ am + cyl + disp, data = dat)

# get coefficients
tmod <- tidy(mod)

# save coefficients
saveRDS(tmod, paths$td("tmod"))

# clear the working space
mykeep()


