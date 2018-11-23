norminal_to_numeric <- function(x) {
  if(!is.numeric(x)) {
    return(as.integer(factor(x, exclude = NULL)))
  } else {
    return(x)
  }
}

normalize <- function(x) {
  if(min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(-1)
  } else {
    return(2 * (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) - 1)
  }
}

get_lower_tri<-function(x) {
  x[upper.tri(x)] <- NA
  return(x)
}

get_upper_tri <- function(x) {
  x[lower.tri(x)]<- NA
  return(x)
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

fuzzify <- function(x, y, value) {
  x[which(x %in% setdiff(x, y))] <- value
  return(x)
}