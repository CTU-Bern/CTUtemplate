
mykeep()

# Load data
dat <- readRDS(paths$pd("prepped_data"))


tab <- atable(dat,
              target_cols = c("hp", "mpg", "cyl", "vs"),
              group_col = "am")

tab <- tab[, 1:4] # remove pvalues and effect measures


# save baseline table
saveRDS(tab, paths$td("baselinetab"))

# clear the working space
mykeep()



