canPingSite <- function(test.site) {
  !as.logical(system(paste("ping", test.site, "/n 1"), show.output.on.console = FALSE))
}
# from: https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
