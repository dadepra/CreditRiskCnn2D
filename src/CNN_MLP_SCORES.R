# File: foo.R
# Desc: Functionality related to foos.

#' Check the arguments.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  Boolean vector of length one.
#' @export
check_args <- function(y_true, y_pred) {
  return(stopifnot(length(y_true) == length(y_pred)))
}

#' Calculate the number of the total population.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' total_population(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
total_population <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(length(y_true))
}

#' Calculate the number of the true positives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' true_positive(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
true_positive <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum((y_true == y_pred) & (y_true == 1)))
}

#' Calculate the number of the true negatives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' true_negative(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
true_negative <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum((y_true == y_pred) & (y_true == 0)))
}

#' Calculate the number of the false positives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_positive(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_positive <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(!(y_true == y_pred) & (y_true == 1)))
}

#' Calculate the number of the false negatives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_negative(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_negative <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(!(y_true == y_pred) & (y_true == 0)))
}

#' Calculate the number of the condition positives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' condition_positive(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
condition_positive <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(y_true == 1))
}

#' Calculate the number of the condition negatives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' condition_negative(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
condition_negative <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(y_true == 0))
}

#' Calculate the number of the predicted condition positives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' predicted_condition_positive(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
predicted_condition_positive <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(y_true == 1))
}

#' Calculate the number of the predicted condition negatives.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' predicted_condition_negative(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
predicted_condition_negative <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  return(sum(y_pred == 0))
}

#' Calculate the accuracy score.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' accuracy_score(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
accuracy_score <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  tn <- true_negative(y_true = y_true, y_pred = y_pred)
  tot_pop <- total_population(y_true = y_true, y_pred = y_pred)
  return((tp + tn) / tot_pop)
}

#' Calculate the accuracy score.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' accuracy(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
accuracy <- accuracy_score

#' Calculate the prevalance.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' prevalence(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
prevalence <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  cn <- condition_positive(y_true = y_true, y_pred = y_pred)
  tot_pop <- total_population(y_true = y_true, y_pred = y_pred)
  return(cn / tot_pop)
}

#' Calculate the positive predictive value.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' positive_predictive_value(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
positive_predictive_value <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  pcp <- predicted_condition_positive(y_true = y_true, y_pred = y_pred)
  return(tp / pcp)
}

#' Calculate the false discovery rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_discovery_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_discovery_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  fp <- false_positive(y_true = y_true, y_pred = y_pred)
  pcp <- predicted_condition_positive(y_true = y_true, y_pred = y_pred)
  return(fp / pcp)
}

#' Calculate the negative predictive value.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' negative_predictive_value(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
negative_predictive_value <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tn <- true_negative(y_true = y_true, y_pred = y_pred)
  pcn <- predicted_condition_negative(y_true = y_true, y_pred = y_pred)
  return(tn / pcn)
}

#' Calculate the false omission rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_omission_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_omission_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  fn <- false_negative(y_true = y_true, y_pred = y_pred)
  pcn <- predicted_condition_negative(y_true = y_true, y_pred = y_pred)
  return(fn / pcn)
}

#' Calculate the true positive rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' true_positive_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
true_positive_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  cp <- condition_positive(y_true = y_true, y_pred = y_pred)
  return(tp / cp)
}

#' Calculate the true positive rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' Formula: tp / (tp + fn)
#' recall(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
recall_score <- function(y_true, y_pred) {
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  fn <- false_negative(y_true = y_true, y_pred = y_pred)
  return (tp / (tp + fn))
}

#' Calculate the false positive rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_positive_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_positive_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  fp <- false_positive(y_true = y_true, y_pred = y_pred)
  cn <- condition_negative(y_true = y_true, y_pred = y_pred)
  return(fp / cn)
}

#' Calculate the false negative rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' false_negative_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
false_negative_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  fn <- false_negative(y_true = y_true, y_pred = y_pred)
  cp <- condition_positive(y_true = y_true, y_pred = y_pred)
  return(fn / cp)
}

#' Calculate the true negative rate.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' true_negative_rate(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
true_negative_rate <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tn <- true_negative(y_true = y_true, y_pred = y_pred)
  cn <- condition_negative(y_true = y_true, y_pred = y_pred)
  return(tn / cn)
}

#' Calculate the positive likelihood ratio.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' positive_likelihood_ratio(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
positive_likelihood_ratio <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tpr <- true_positive_rate(y_true = y_true, y_pred = y_pred)
  fpr <- false_positive_rate(y_true = y_true, y_pred = y_pred)
  return(tpr / fpr)
}

#' Calculate the negative likelihood ratio.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' negative_likelihood_ratio(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
negative_likelihood_ratio <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  fnr <- false_negative_rate(y_true = y_true, y_pred = y_pred)
  tnr <- true_negative_rate(y_true = y_true, y_pred = y_pred)
  return(fnr / tnr)
}

#' Calculate the diagnostic odds ratio.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' diagnostic_odds_ratio(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
diagnostic_odds_ratio <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  plr <- positive_likelihood_ratio(y_true = y_true, y_pred = y_pred)
  nlr <- negative_likelihood_ratio(y_true = y_true, y_pred = y_pred)
  return(plr / nlr)
}

#' Calculate the F1 score.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' f1_score(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
f1_score <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  fp <- false_positive(y_true = y_true, y_pred = y_pred)
  fn <- false_negative(y_true = y_true, y_pred = y_pred)
  numerator <- 2 * tp
  denominator <- 2 * tp + fp + fn
  return(numerator / denominator)
}

#' Calculate Matthew's correlation coefficient.
#'
#' @param y_true Ground truth (correct) target values.
#' @param y_pred Estimated target values.
#' @return  A numeric vector of length one.
#' @family classification metrics
#' @examples
#' matthews_corrcoef(c(1, 1, 0, 0), c(1, 0, 1, 0))
#' @export
matthews_corrcoef <- function(y_true, y_pred) {
  check_args(y_true = y_true, y_pred = y_pred)
  tp <- true_positive(y_true = y_true, y_pred = y_pred)
  fp <- false_positive(y_true = y_true, y_pred = y_pred)
  tn <- true_negative(y_true = y_true, y_pred = y_pred)
  fn <- false_negative(y_true = y_true, y_pred = y_pred)
  numerator <- (tp * tn) - (fp * fn)
  denominator <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)
  return(numerator / (denominator ^ 0.5))
}



################################################################################
#' Funções utilitárias
#' 
################################################################################

#
#Converter matriz em data frame 
util_ConvMatrix2DfList <- function (matriz) {
  tam_matriz <- dim(matriz)[1]
  qtd_amostras <- dim(matriz)[2]
  qtd_cols <- dim(matriz)[3]
  cat("tam_matriz: ", tam_matriz, ", qtdA: ", qtd_amostras, ", qtdC:", qtd_cols, "\n")
  dfMatriz <-list(NULL)
  
  for (cliIdx in 1:tam_matriz) {
    dfMatriz[[cliIdx]] <- createDfCols(qtd_amostras)
    for (amoIdx in 1:qtd_amostras) {
      for (colIdx in 1:qtd_cols) {
        dfMatriz[[cliIdx]][[colIdx]][[amoIdx]] <- matriz[cliIdx, amoIdx, colIdx, 1]
      }
    }
  }
  dfMatriz
}


#Converter matriz em data frame 
util_CriarDfCols <- function(tam) {
  dfCol <- data.frame(c01=c(seq(1:tam)), 
                      c02=c(seq(1:tam)), 
                      c03=c(seq(1:tam)), 
                      c04=c(seq(1:tam)), 
                      c05=c(seq(1:tam)), 
                      c06=c(seq(1:tam)), 
                      c07=c(seq(1:tam)), 
                      c08=c(seq(1:tam)), 
                      c09=c(seq(1:tam)), 
                      c10=c(seq(1:tam)), 
                      c11=c(seq(1:tam)), 
                      c12=c(seq(1:tam)), 
                      c13=c(seq(1:tam)), 
                      c14=c(seq(1:tam)), 
                      c15=c(seq(1:tam)), 
                      c16=c(seq(1:tam)), 
                      c17=c(seq(1:tam)), 
                      c18=c(seq(1:tam)), 
                      c19=c(seq(1:tam)), 
                      c20=c(seq(1:tam))
  )
  dfCol
}
