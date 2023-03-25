#Função que exibe a performance do modelo para diversos pacotes de medicação
mostrarResultados <- function (geral=TRUE, yard=TRUE, rocr=TRUE, info=TRUE) {

  #Modelo 
  summary(model_cnn)
  
  #
  print(history)
  
  cat("\nEval treino:\n")
  cat("\t precisão: ", ds.train_eval[2], "\n")
  cat("\t loss: ", ds.train_eval[1], "\n")
  cat("\t rmse: ", ds.train_rmse, "\n")

  cat("\nEval teste:\n")
  cat("\t precisão: ", ds.test_eval[2], "\n")
  cat("\t loss: ", ds.test_eval[1], "\n")
  cat("\t rmse: ", ds.test_rmse, "\n")

  if (yard) {
    cat("\nYardStick:\n")
    cat("\t acc: ", yard.results$acc$.estimate, "\n")
    cat("\t prec: ", yard.results$prec$.estimate, "\n")
    cat("\t sens: ", yard.results$sens$.estimate, "\n")
    cat("\t spec: ", yard.results$spec$.estimate, "\n")
    cat("\t rec: ", yard.results$rec$.estimate, "\n")
    cat("\t f1: ", yard.results$fmeas$.estimate, "\n")
    cat("\t kappa: ", yard.results$kap$.estimate, "\n")
  }
  
  if (rocr) {
    cat("\nROCR:\n")
    cat("\t ks:", rocr.ks, "\n")
    cat("\t rmse:", attr(rocr.rmse, "y.values")[[1]], "\n")
    cat("\t auc:", attr(rocr.auc, "y.values")[[1]], "\n")
  }
  
  if (info) {
    cat("\nInformationValue:\n")
    cat("\t prec:", infov.prec, "\n")
    cat("\t sens:", infov.sens, "\n")
    cat("\t spec:", infov.spec, "\n")
    cat("\t ks:", infov.ks, "\n")
    cat("\t auc:", infov.auc, "\n")
  }
  
  cat("\npROC\n")
  cat("\t auc: ", ds.test_auc, "\n")
  
  cat("\nCALC_SCORE:\n")
  cat("\t accuracy: ", accuracy_score(ds.test_y, ds.test_pclass), "\n")
  cat("\t rec: ", recall_score(ds.test_y, ds.test_pclass), "\n")
  cat("\t f1: ", f1_score(ds.test_y, ds.test_pclass), "\n")
  
  cat ("\ncaret analisys para Treino\n")
  print(caret.confmat_train)
  cat ("\ncaret analisys para Teste\n")
  print(caret.confmat_test)
  
}
