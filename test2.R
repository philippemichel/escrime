ligx <- function(varx) {
  if (is.numeric(varx)) {
    mm <- round(mean(varx), 3)
    ss <- round(sd(varx), 3)
    llx <- paste0(mm, " ± ", ss)
  }
  else{llx <- NA}
  return(llx)
}