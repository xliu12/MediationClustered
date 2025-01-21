#' @export

# update interactions
update.interactions <- function(data_in, varnames) {
  # dat[, unlist(varnames[c("AM","Mint","AMint")])]
  Aname <- varnames$A
  Mnames <- varnames$M
  data_in[, varnames$AM] <- data_in[[Aname]] * data_in[, Mnames]

  data_in[, varnames$Mint] <- data.frame(data_in[, Mnames[1]] * data_in[, Mnames[2]])
  data_in[, varnames$AMint] <- data.frame(data_in[[Aname]] * data_in[, Mnames[1]] * data_in[, Mnames[2]])
  data_in
}
