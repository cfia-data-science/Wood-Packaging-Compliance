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

test_roc <- function(model, data) {
  # roc: build a ROC curve
  # roc(response,
  #     predictor)
  # response: a factor, numeric or character vector of responses, typically encoded with 0 (controls) and 1 (cases)
  # predictor: a numeric vector of the same length than response, containing the predicted value of each observation
  roc(data$Compliant..Y.N.,
      predict(model, data, type = "prob")[, "N"])
}