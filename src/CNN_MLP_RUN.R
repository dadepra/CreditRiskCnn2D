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

#Define o diretório de trabalho
setwd("./")
arqScript <- "./CNN_MLP.R"
#tfruns::training_run(arqScript, echo = FALSE)

#Comparar diferentes parâemtros HyperTuning
cdropout1 <- c(0.25, 0.35, 0.45)
cdropout2 <- c(0.20, 0.30, 0.40) 
cdense1Units <- c(60, 96, 128, 196) 
cdense2Units <- c(32, 64, 96, 128)
ckernel_reg1 <- c(0.01, 0.02, 0.04)
ckernel_reg2 <- c(0.01, 0.02, 0.03)
cepocas <- c(70, 120, 150)

cdropout1 <- c(0.25, 0.35, 0.40, 0.45)
cdropout2 <- c(0.20, 0.30, 0.35, 0.40) 
cverbose <- c(0)
cepocas <- c(500)

CNN_MLP_INIT <- FALSE
CNN_MLP_SHOW_PLOTS <<- FALSE

if (!is_run_active()) {
    tuning_run(arqScript, 
                     flags = list( 
                      dropout1 = cdropout1, 
                      dropout2 = cdropout2, 
                      dense1Units = cdense1Units, 
                      dense2Units = cdense2Units,
                      kernel_reg1 = ckernel_reg1,
                      kernel_reg2 = ckernel_reg2,
                      verbose = cverbose,
                      epocas = cepocas
                     ),
                     echo = FALSE,
                     confirm = FALSE
             )
}
#
latest_run()

#You can view the report for any given run using the view_run()
#tfruns::view_run("runs/2021-02-14T00-40-00Z")

#https://tensorflow.rstudio.com/tools/tfruns/overview/#comparing-runs
runs.best <- ls_runs(order = metric_val_accuracy, decreasing=T)$run_dir

#runs.lasts <- ls_runs(latest_n = 576)
#runs_lasts.best <- runs.lasts %>% dplyr::filter(metric_val_accuracy > 0.97)
#runs_lasts.besto <- runs_lasts.best %>% dplyr::arrange(dplyr::desc(metric_val_accuracy))
run_info(runs.best[1])
view_run(runs.best[1])
compare_runs(c(runs.best[1], runs.best[2]))

run_info(runs.best[1])

#https://tensorflow.rstudio.com/tools/tfruns/overview/#analyzing-runs
#ls_runs()

#clean_runs() # archives all runs in the "runs" directory
#purge_runs()
