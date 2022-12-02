#importFrom pingr ping
canPingSite <- function(test.site) {
  !is.na(pingr::ping(test.site, count = 1))
}
