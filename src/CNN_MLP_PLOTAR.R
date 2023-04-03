##############################################
draw_confusion_matrix <- function(cm, titulo, detalhe) {
  
  classes = colnames(cm$table)
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(titulo, cex.main=2)
  
  cor_erros <- "#FF4040"
  cor_acertos <- "#3CB371"
  # create the matrix 
  rect(150, 430, 240, 370, col=cor_acertos)
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=cor_erros)
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predição', cex=1.3, srt=90, font=2)
  text(245, 450, 'Real', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=cor_erros)
  rect(250, 305, 340, 365, col=cor_acertos)
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  total_ruins <- res[1] + res[2]
  total_bons <- res[3] + res[4]
  
  text(195, 400, sprintf("%0d (%0.2f%%)", res[1], (res[1] / total_ruins) * 100), cex=1.6, font=2, col='white')
  text(195, 335, sprintf("%0d (%0.2f%%)", res[2], (res[2] / total_ruins) * 100), cex=1.6, font=2, col='white')
  text(295, 400, sprintf("%0d (%0.2f%%)", res[3], (res[3] / total_bons) * 100), cex=1.6, font=2, col='white')
  text(295, 335, sprintf("%0d (%0.2f%%)", res[4], (res[4] / total_bons) * 100), cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = detalhe, xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

##############################################
#'Plotar correções de uma coluna Correlações
plotarCorrelacoes <- function(conj, col) {
  posi <- ((col - 1) * 12) + 1
  posf <- col * 12
  df_test_cols <- conj[, posi:posf]
  c1 <- corrplot::cor.mtest(df_test_cols)
  test_cor_log <- cor(log(df_test_cols), method="spearman", use = "complete.obs")
  par(mfrow=c(1,2))
  corrplot::corrplot(cor(df_test_cols, use = "complete.obs"), add=FALSE,
                     method="shade",shade.col=NA, tl.col="black", tl.srt=45,
                     title = paste0('FPC-COR: Transform - Coluna: ', col), mar=c(0, 0, 2, 0))
#  corrplot::corrplot (test_cor_log,  addrect = 10, add=FALSE,
#                      method = 'ellipse', title = paste0('FPC: Log-Transform - Coluna: ', col), mar=c(0, 0, 2, 0))
 corrplot::corrplot(test_cor_log, type = "upper", method = "ellipse", add=FALSE,
                     p.mat = c1$p , sig.level = 0.5, insig = "blank",
                     title = paste0('Correlation Matrix - Coluna: ', col), mar=c(0, 0, 2, 0))
}

#'Prepara um conjunto de testes para usar correlações
prepararCorM <- function (conj, conj_nomes, posi, tam, col=TRUE, lab="c", ind=1) {
  posf <- (posi + tam) - 1
  recorte <- if (col) conj[posi:posf, ind, 1:tam, 1] else conj[posi:posf, 1:tam, ind, 1]
  M <- cor(recorte)[1:tam, 1:tam]
  cn <- list(NULL)
  for (i in 1:tam) cn[[i]] <- sprintf("%s%02d", lab, i)
  colnames(M) <-cn
  
  cn <- list(NULL)
  idx <- 1
  for (i in posi:posf) {
    cn[[idx]] <- conj_nomes[i]
    idx <- idx + 1
  }
  rownames(M) <-cn
  M
}

##############################################
#LIME
#'Converte os dados do data frame para o formato de matrix 4d para que o modelo
#'possa entender e fazer o tratamento
convDf2Mx4d <- function(x) {
  print ('confDf2Mx4d\n')
  mtest <- data.matrix(x)
  ar1 <- array_reshape(mtest, c(dim(mtest)[1], 12, 20, 1))
  ar1
}

#Interver predições de bons e ruim para a segunda parte do conjunto
ajusteLabelProb <- function(explain1, nroLabels, nroCasos, nroFeat) {
  if (nroLabels > 1) {
    posIni <- 1 + (nroCasos * nroFeat)
    posFim <- (1 + nroCasos) * nroFeat
    for (i in posIni:posFim) {
      idxRuim = i + nroFeat
      label_prob <- explain1$label_prob[[i]]
      explain1$label_prob[[i]] <- explain1$label_prob[[idxRuim]]
      explain1$label_prob[[idxRuim]] <- label_prob
    }
  } else if (nroCasos > 1) {
    posIni <- 1 + ((nroCasos * nroFeat) / 2)
    posFim <- nroCasos * nroFeat
    for (i in posIni:posFim) {
      explain1$label_prob[[i]] <- (explain1$label_prob[[i]] - 1)
      explain1$feature_weight[[i]] <- - abs(explain1$feature_weight[[i]])
    }
  }
}


#Plotar funções do lime
plotarLime <- function (df_train, df_test, cli_good, cli_bad) {
  par(mfrow=c(1,1))
  
  lbr <- c("Bom", "Ruim")
  lrb <- c("Ruim", "Bom")
  explainer <- lime::lime(
    x = df_train, 
    lime::as_classifier(model_cnn, unlist(lrb)),
    convDf2Mx4d,
    bin_continuous = TRUE,
    quantile_bins = FALSE
  )
  #print("plotarLime, step3")
  #Explain
  print(summary(explainer))
  
  df_test_explain <- df_test[c(cli_good[[1]], cli_bad[[1]]), ]
  pt1 <- predict(model_cnn, convDf2Mx4d(df_test_explain))
  
  # Run explain() on explainer
  nroFeat <- 10
  nroLabels <- 1
  nroCasos <- dim(df_test_explain)[1]
  explain_feat <- lime::explain(
    x = df_test_explain,
    explainer = explainer,
    feature_select  = "forward_selection",
    labels = c("Bom"),
    n_features = nroFeat,
    weight = .54,
  )
#  if (INVERTER_EXPLAIN_PROBS) {
#    ajusteLabelProb(explain_feat, nroLabels, nroCasos, nroFeat)
#  }
  #print("plotarLime, step4")
  
  #plot 
  print(lime::plot_features(explanation = explain_feat, ncol = 2) + 
    labs(title = "LIME Visão da Importância por Característica",
         subtitle = "Conjunto de Teste"))
  
  #print("plotarLime, step5")
  
  nroLabels <- 2
  nroFeat <- 3
  heat_amostras <- sample(dim(df_test)[1], 10)[4:7]
  df_test_heat <- df_test[heat_amostras, ]
  explain_heatmap <- lime::explain(
    x = df_test_heat,
    explainer = explainer,
    n_features = nroFeat,
    n_labels = nroLabels,
#    n_permutations  = 5000,
#    dist_fun        = "manhattan",
#    feature_select  = "highest_weights",
    feature_select  = "forward_selection",
#    labels = c("Ruim"),
#    weight = .00050,
  )
  #if (INVERTER_EXPLAIN_PROBS) {
  # ajusteLabelProb(explain_heatmap, nroLabels, nroCasos, nroFeat)
  #}
  
  #print("plotarLime, step6")
  
  print(lime::plot_explanations(explain_heatmap) +
    labs(title = "LIME Visão da Importância em Mapa de Calor",
         subtitle = "Conjunto de Teste (2)"))
  #print("plotarLime, step7")
  
  nroFeat <- 16
  nroLabels <- 1
  explain_feat1 <- lime::explain(
    x               = df_test_explain, 
    explainer       = explainer, 
    n_permutations  = 5000,
    dist_fun        = "manhattan",
    feature_select  = "lasso_path",
    labels = c("Bom"),
    kernel_width    = 28,
    n_features      = nroFeat,
    weights = 06
  )
  INVERTER_EXPLAIN_PROBS <- FALSE
  if (INVERTER_EXPLAIN_PROBS) {
    #Interver labels
    posIni <- 1 + nroFeat
    posFim <- dim(explain_feat1)[1]
    for (i in posIni:posFim) { 
      w <- explain_feat1$feature_weight[[i]]
      explain_feat1$feature_weight[[i]] <- -w
      explain_feat1$label_prob[[i]] <- 1 - explain_feat1$label_prob[[i]]
    }
  }
  #print("plotarLime, step8")
  
  #plot 
  print(lime::plot_features(explanation = explain_feat1, ncol = 2) + 
    labs(title = "LIME Visão da Importância por Característica",
         subtitle = "Conjunto de Teste"))
  #print("plotarLime, step9")
  
}

#plotar ROCR
rocr.perfPlot <- function (pred, titulo) {
  par(mfrow=c(1,1))
  rocr.plot <- ROCR::plot(pred, 
                          avg = "threshold", 
                          spread.estimate = "stddev",
                          colorize = TRUE,
                          lwd = 3,
                          add = FALSE,
                          print.cutoffs.at = seq(0,1,by=0.1),
                          text.adj = c(1.2,1.2),
                          main = titulo)
  print(plot(pred,
       lty=3,
       col="grey78",
       add=TRUE))
  
  rocr.plot
}


########
# Mapa de correc por periodo
plotarCorrPeriodo <- function (cliIni, conj, conjNomes) {
  arqName <- sprintf("%scorr_20cli_carac_%04d_%d.png", PATH_PLOTS, cliIni, as.integer(Sys.time()))
  print(arqName)

  png(filename = arqName, bg = "transparent", width = 4096, height = 3200)
  par(mfrow=c(4,6))
  for (i in 1:12) {
    corrplot::corrplot(conj[cliIni:(cliIni + 20), i, 1:20, 1], is.corr = FALSE, method="circle", main=paste0("Valor da características - 20 clientes - Período ", i), mar=c(0,0,2,0))
    cm1 <- prepararCorM(conj, conjNomes, cliIni, 20, col=TRUE, lab="c", ind=i)
    corrplot::corrplot(cm1, method="circle", na.label = " ", main=paste0("Correlação características 20 clientes - Período ", i), mar=c(0,0,2,0), diag = FALSE)
  }
  dev.off()
}

#Mapa coorr por carac
plotarCorrCaract <- function (cliIni, conj, conjNomes) {
  arqName <- sprintf("%scorr_20cli_periodo_%04d_%d.png", PATH_PLOTS, cliIni, as.integer(Sys.time()))
  print(arqName)
  
  png(filename = arqName, bg = "transparent", width = 4096, height = 3200)
  par(mfrow=c(4,5))
  for (i in 1:20) {
    cm2 <- prepararCorM(conj, conjNomes, cliIni, 12, col=FALSE, lab="a", ind=i)
    corrplot::corrplot(cm2, method="circle", na.label = " ", main=paste0("Correlação períodos - 12 clientes - Característica ", i), mar=c(0,0,2,0), diag = FALSE)
  }
  dev.off()
}

#Plotar correções de uma coluna Correlações
plotarCorrCols <- function(conj) {
  for (col in 1:20) {
    plotarCorrelacoes(conj, col)
  }
}

#Função que plotar imagens de resultados para diversas métricas
plotarResultados <- function (cliIni=185, plot_rocr=FALSE, plot_info=FALSE, plot_lime=TRUE, plot_corr_periodo=FALSE, plot_corr_carac=FALSE, plot_corr_cols=FALSE, display_clientes=FALSE) {

  cat("Cliente IDX: ", cliIni, "\n")  
  ############################################
  # encontrar primeiros clientes bons e ruins
  cli_good <- list(NULL)
  cli_bad <- list(NULL)
  idx_good <- 0
  idx_bad <- 0
  for (i in 1:length(ds.test_y)) {
    if (ds.test_y[i] == 1 && (idx_good < 2)) {
      idx_good <- idx_good + 1
      cli_good[idx_good] <- i
    }
    if (ds.test_y[i] == 0 && (idx_bad < 2)) {
      idx_bad <- idx_bad + 1
      cli_bad[idx_bad] <- i
    }
    if (idx_bad == 2 && idx_good == 2) {
      break
    }
  }
  
  # Converter conjuntos (treino e teste) para dataFrames
  df_train <- data.frame(ds.train[, 1:12, 1:20, 1])
  df_test <- data.frame(ds.test[, 1:12, 1:20, 1])
  #Nomear colunas dos data frames
  colnames(df_train) <- colnames(df_test) <- c(
    paste0(sprintf("a%02d", 01:12), "c01"),
    paste0(sprintf("a%02d", 01:12), "c02"),
    paste0(sprintf("a%02d", 01:12), "c03"),
    paste0(sprintf("a%02d", 01:12), "c04"),
    paste0(sprintf("a%02d", 01:12), "c05"),
    paste0(sprintf("a%02d", 01:12), "c06"),
    paste0(sprintf("a%02d", 01:12), "c07"),
    paste0(sprintf("a%02d", 01:12), "c08"),
    paste0(sprintf("a%02d", 01:12), "c09"),
    paste0(sprintf("a%02d", 01:12), "c10"),
    paste0(sprintf("a%02d", 01:12), "c11"),
    paste0(sprintf("a%02d", 01:12), "c12"),
    paste0(sprintf("a%02d", 01:12), "c13"),
    paste0(sprintf("a%02d", 01:12), "c14"),
    paste0(sprintf("a%02d", 01:12), "c15"),
    paste0(sprintf("a%02d", 01:12), "c16"),
    paste0(sprintf("a%02d", 01:12), "c17"),
    paste0(sprintf("a%02d", 01:12), "c18"),
    paste0(sprintf("a%02d", 01:12), "c19"),
    paste0(sprintf("a%02d", 01:12), "c20")
  )
  #nomear as linhas do dataframe de treino
  rownames(df_train) <- ds.train_nomes
  
  #nomear as linhas do dateframe de testes
  rownames(df_test) <- ds.test_nomes
  
  #Imprimir histograma
  par(mfrow=c(1,1))
  arqName <- sprintf("%shistograma_%d.png", PATH_PLOTS, as.integer(Sys.time()))
  print(arqName)
  png(filename = arqName, bg = "transparent", width = 800, height = 600)
  base::plot(history) + ggplot2::geom_point()
  dev.off()

  if (plot_rocr) {
    vpclass <- matrix(ds.test_pclass)[, 1]
    roc.info1 <- pROC::roc(vpclass, 
                          as.vector(ds.test_y), 
                          plot=TRUE, 
                          legacy.axes=TRUE, 
                          percent=TRUE, 
                          xlab="Falso Positivo % (Specificity)", 
                          ylab="Verdadeiro Positivo % (Sensitivity)", 
                          col="#0000FF", 
                          lwd=4,
                          print.auc=TRUE,
                          auc=auc(roc(vpclass, ds.test_y))
    )
    
    roc.info <- pROC::roc(as.vector(ds.test_y), 
                          as.vector(ds.test_proba), 
                          plot=TRUE, 
                          legacy.axes=TRUE, 
                          percent=TRUE, 
                          xlab="Falso Positivo % (Specificity)", 
                          ylab="Verdadeiro Positivo % (Sensitivity)", 
                          col="#0000FF", 
                          lwd=4,
                          print.auc=TRUE,
                          auc=ds.test_auc
    )
    roc.df <- data.frame(
      tpp=roc.info$sensitivities*100,
      fpp=(1 -  roc.info$specificities*100),
      thresholds=roc.info$thresholds
    )
    #ROC data frame
    head(roc.df)
    tail(roc.df)

    #Gráfico que mostra as probabilidades de evasão separada entre evadidos e concluídos
    print(plot(ds.train_proba[, 1], 
         pch=c(16),
         #pch=c(17,4),
         main="Conjunto de Treino",
         xlab = "Indíces",
         ylab = "Probabilidades",
         add=FALSE,
         col=c("red", "blue")[as.factor(ds.train_y)], mar=c(0,0,20,0)))
    h1 <- 1 - ds.train_eval[[1]]
    h2 <- 1 - ds.train_rmse
    print(abline(h=h1, col="orange"))
    print(abline(h=h2, col="cyan"))
    legend("center", legend=c("Bons", "Ruins", "corte RMSE", "corte Erro"), col=c("blue", "red", "orange", "cyan"), lwd=3, border=FALSE)
    
    print(plot(ds.test_proba[, 1], 
         pch=c(16),
         #pch=c(17,4),
         main="Conjunto de Teste",
         xlab = "Indíces",
         ylab = "Probabilidades",
         add=FALSE,
         col=c("red", "blue")[as.factor(ds.test_y)]))
    h2 <- 1 - ds.test_eval[[1]]
    h1 <- 1 - ds.test_rmse
    print(abline(h=h1, col="orange"))
    print(abline(h=h2, col="cyan"))
    legend("center", legend=c("Bons", "Ruins", "corte RMSE", "corte Erro"), col=c("blue", "red", "orange", "cyan"), lwd=3, border=FALSE)
    
    smoothScatter(pch=15, ds.test_proba[, 1], 
                  col=c("red", "blue")[as.factor(ds.test_y)], 
                  nrpoints=40,
                  main="Conjunto de Teste",
                  xlab = "Indíces",
                  ylab = "Probabilidades",
                  add=FALSE
    )
    legend("center", legend=c("Bons", "Ruins", "Corte"), col=c("blue", "red", "orange"), lwd=3, border=FALSE)
    abline(h=h2, col="orange")

  # ROCR
    rocr.plot_acc <- ROCR::plot(rocr.acc, main="Acurácia", add=FALSE)
    rocr.plot_prec <- ROCR::plot(rocr.prec, main="Precisão", add=FALSE)
    rocr.plot_rec <- ROCR::plot(rocr.rec, main="Recordação", add=FALSE)
    rocr.plot_prec_rec <- rocr.perfPlot(rocr.prec_rec, "Performance Precisão X Recordação")
    rocr.plot_sens <- ROCR::plot(rocr.sens, main="Sensibilidade", add=FALSE)
    rocr.plot_spec <- ROCR::plot(rocr.spec, main="Especificidade", add=FALSE)
    rocr.plot_sens_spec <- rocr.perfPlot(rocr.sens_spec, "Performance Sensibilidade X Especificidade")
    rocr.plot_fmeas <- ROCR::plot(rocr.fmeas, main="F1", add=FALSE)
    
    print(plot(rocr.tpr_fpr, col = "orange", lwd = 3, add=FALSE,  main = "Performance TPR x FPR"))
  }
  
  #
  #
  if (plot_info) {
    vpclass <- matrix(ds.test_pclass)[, 1]
    infov.pks <- InformationValue::ks_plot(ds.test_y, vpclass)
    infov.proc1 <- InformationValue::plotROC(ds.test_y, vpclass)
    infov.proc2 <- InformationValue::plotROC(ds.test_y, ds.test_proba[, 1], returnSensitivityMat = TRUE, Show.labels = TRUE)
    infov.proc3 <- InformationValue::plotROC(ds.test_y, vpclass)
  }

  ########
  # Mapa de correlação
  if (plot_corr_periodo) {
    plotarCorrPeriodo(cliIni, ds.test, ds.test_nomes)
  }
  if (plot_corr_carac) {
    plotarCorrCaract(cliIni, ds.test, ds.test_nomes)
  }
  ##############################################
  #Plotar correções de uma coluna Correlações
  if (plot_corr_cols) {
    plotarCorrCols(df_test)
  }
  
  #Plotar gráficos do lime
  if (plot_lime) {
    plotarLime(df_train, df_test, cli_good, cli_bad)
  }
  
  #exibir imagens dos clientes
  if (display_clientes) {
    n <- cli_good[[1]]
    #Dados do cliente como uma imagem
    arrImg <- ds.test[n, 1:12, 1:20, 1] / LIMITE_NORM
    EBImage::display(arrImg, method = "raster")
    text(x = -1, y = 12, label = ds.test_nomes[n], adj = c(0,1), srt=90, col = "orange", cex = 2)
    img <- EBImage::Image(arrImg)
    hist(img, main=paste0("Histograma para ", ds.test_nomes[n]))
    # Mapa de correlação
    corrplot::corrplot(ds.test[n, 1:12, 1:20, 1], is.corr = FALSE, method="circle", main=paste0(ds.test_nomes[n], " - Correlação entre características e pesos"), mar=c(0,0,2,0))
    
    cat(ds.test_nomes[n], "\n");  
    print(trunc (ds.test[n, 1:12, 1:20, 1] * (10^4), prec = 4) / (10 ^ 4))
    print(csArqs$dados[[ds.test_idx[n]]][1:12, 1:20])
    
    n <- cli_good[[2]]
    #Dados do cliente como uma imagem
    arrImg <- ds.test[n, 1:12, 1:20, 1] / LIMITE_NORM
    EBImage::display(arrImg, method = "browser", drawGrid = TRUE)
    text(x = -1, y = 12, label = ds.test_nomes[n], adj = c(0,1), srt=90, col = "orange", cex = 2)
    
    n <- cli_bad[[1]]
    #Dados do cliente como uma imagem
    arrImg <- ds.test[n, 1:12, 1:20, 1] / LIMITE_NORM
    EBImage::display(arrImg, method = "raster")
    text(x = -1, y = 12, label = ds.test_nomes[n], adj = c(0,1), srt=90, col = "orange", cex = 2)
    img <- EBImage::Image(arrImg)
    hist(img, main=paste0("Histograma para ", ds.test_nomes[n]))
    
    # Mapa de correlação
    corrplot::corrplot(ds.test[n, 1:12, 1:20, 1], is.corr = FALSE, method="circle", main=paste0(ds.test_nomes[n], " - Correlação entre características e pesos"), mar=c(0,0,2,0))
    
    cat(ds.test_nomes[n], "\n");  
    print(trunc (ds.test[n, 1:12, 1:20, 1] * (10^4), prec = 4) / (10 ^ 4))
    print(csArqs$dados[[ds.test_idx[n]]][1:12, 1:20])
    
    n <- cli_bad[[2]]
    #Dados do cliente como uma imagem
    arrImg <- ds.test[n, 1:12, 1:20, 1] / LIMITE_NORM
    EBImage::display(arrImg, method = "raster")
    text(x = -1, y = 12, label = ds.test_nomes[n], adj = c(0,1), srt=90, col = "orange", cex = 2)
  }
  
}
