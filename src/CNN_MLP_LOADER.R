#Carrega biblioteca do TensorFlow Runs
pkg <- "tensorflow"
if (!requireNamespace(pkg, quietly = TRUE)) {
  install.packages(pkg)
  library(tensorflow)
  install_tensorflow(version = "gpu")
}  
library(pkg, character.only = TRUE)
#Workflow para contornar bug do Tensorflow 2.0
tf$reset_default_graph = tf$compat$v1$reset_default_graph
#tf$reset_default_graph <- function (args...) {}

pkg <- "keras"
if (!requireNamespace(pkg, quietly = TRUE)) {
  install.packages(pkg)
  install_keras()
}  
library(pkg, character.only = TRUE)

pkg <- "tfruns"
if (!requireNamespace(pkg, quietly = TRUE)) {
  install.packages(pkg)
}
library(pkg, character.only = TRUE)

if (Sys.getenv("RSTUDIO") == 1) {
  #install.packages("rstudioapi")
  library(rstudioapi)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(getSrcDirectory(function(){})[1])  
}
cat ("Current path: ", getwd())

#mostrarResultados
import::from("./CNN_MLP_SHOW_PERF.R", mostrarResultados)

#
#runs.best <- ls_runs(order = metric_val_accuracy, decreasing=T)$run_dir
local <- "./../runs/2021-03-16T14-30-34Z"
modelo <- file.path(local, list.files(local, pattern = "*.tf"))
model_cnn <- load_model_tf(modelo)
summary(model_cnn)

#------------------------------------------------------------------------
ds.train_eval <<- model_cnn %>% evaluate(ds.train,ds.train_lab) 
#-----Classification
ds.train_pclass <- model_cnn%>% predict_classes(ds.train)
ds.train_proba <<- model_cnn %>% predict_proba(ds.train)
ds.train_rmse <<- mean(sqrt((ds.train_pclass - ds.train_y) ** 2))
#----Results
ds.train_xtab <<- table(Predicted = ds.train_pclass, Actual = ds.train_y) 

#test
ds.test_eval <<- model_cnn %>% evaluate(ds.test, ds.test_lab) #-----Evaluation of test set
ds.test_pclass <<- model_cnn  %>% predict_classes(ds.test)   #-----Classification
ds.test_proba <<- model_cnn %>% predict_proba(ds.test)
ds.test_rmse <<- mean(sqrt((ds.test_pclass - ds.test_y) ** 2))
ds.test_xtab <<- table(Predicted = ds.test_pclass, Actual = ds.test_y) #-----Results
# predict probabilities for test set
ds.test_yprobs <<- model_cnn %>% predict(ds.test, verbose=0)
# reduce to 1d array
ds.test_yprobs <<- ds.test_yprobs[, 1]

# Ajusta nomes
rownames(ds.train_xtab) <- rownames(ds.test_xtab) <- colnames(ds.train_xtab) <- colnames(ds.test_xtab) <- c("Ruins", "Bons")

#Pacote CARET
caret.confmat_train <- caret::confusionMatrix(ds.train_xtab, positive = "Bons")
caret.confmat_test <- caret::confusionMatrix(ds.test_xtab, positive = "Bons")

  #yardstick performance analysis
  yard.keras_tbl <- tibble::tibble(
    truth      = as.factor(ds.test_y) %>% forcats::fct_recode(yes = "1", no = "0"),
    estimate   = as.factor(ds.test_pclass) %>% forcats::fct_recode(yes = "1", no = "0"),
    class_prob = ds.test_proba
  )
  options(yardstick.event_first = FALSE)
  yard.confmat <- yard.keras_tbl %>% yardstick::conf_mat(truth, estimate)
  
  yard.results <- tibble::tibble(
    acc = yard.keras_tbl %>% yardstick::accuracy(truth, estimate),
    prec = yard.keras_tbl %>% yardstick::precision(truth, estimate),
    rec = yard.keras_tbl %>% yardstick::recall(truth, estimate),
    sens = yard.keras_tbl %>% yardstick::sens(truth, estimate),
    spec = yard.keras_tbl %>% yardstick::spec(truth, estimate),
    fmeas = yard.keras_tbl %>% yardstick::f_meas(truth, estimate, beta = 1),
    kap =  yard.keras_tbl %>% yardstick::kap(truth, estimate)
  )
  
  #Metrics com pacote ROCR
  rocr.pred <- ROCR::prediction(ds.test_proba, ds.test_lab)
  rocr.acc <- ROCR::performance(rocr.pred, "acc")
  rocr.prec <- ROCR::performance(rocr.pred, "prec")
  rocr.rec <- ROCR::performance(rocr.pred, "rec")
  rocr.prec_rec <- ROCR::performance(rocr.pred, "prec", "rec")
  rocr.sens <- ROCR::performance(rocr.pred, "sens")
  rocr.spec <- ROCR::performance(rocr.pred, "spec")
  rocr.sens_spec <- ROCR::performance(rocr.pred, "spec", "sens")
  rocr.fmeas <- ROCR::performance(rocr.pred, "f")
  rocr.tpr_fpr <- ROCR::performance(rocr.pred, "tpr", "fpr")
  rocr.auc <- ROCR::performance(rocr.pred, "auc")
  rocr.rmse <- ROCR::performance(rocr.pred, "rmse")
  # KS
  rocr.ks <- max(attr(rocr.tpr_fpr, "y.values")[[1]] - (attr(rocr.tpr_fpr, "x.values")[[1]]))
  
  #InformationValue metrics
  infov.prec <- InformationValue::precision(ds.test_y, ds.test_pclass)
  infov.sens <- InformationValue::sensitivity(ds.test_y, ds.test_pclass)
  infov.spec <- InformationValue::specificity(ds.test_y, ds.test_pclass)
  infov.kap <- InformationValue::kappaCohen(ds.test_y, ds.test_pclass)
  infov.ks <- InformationValue::ks_stat(ds.test_y, ds.test_pclass)
  infov.auc <- InformationValue::AUROC(ds.test_y, ds.test_pclass)
  
  #ROC
  ds.test_auc <- pROC::auc(ds.test_y, ds.test_yprobs)
  
  tprFromRocr <- unlist(rocr.tpr_fpr@y.values)
  fprFromRocr <- unlist(rocr.tpr_fpr@x.values)
  plot(tprFromRocr ~ fprFromRocr, col="deepskyblue", main="ROC plot- from ROCR package",type="o", pch=16);abline(0,1);
  grid()
  print(performance(rocr.pred, "auc")@y.values[[1]])
  diff_tprFpr <- tprFromRocr - fprFromRocr
  ksmax <- max(diff_tprFpr)
  cutoffKs <- unlist(rocr.tpr_fpr@alpha.values)[which.max(diff_tprFpr)]
  cat("KSstat: ", ksmax, "CutOffKS: ", cutoffKs, "\n")
  
  mostrarResultados()
  