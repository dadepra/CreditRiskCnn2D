################################################################################
#'@title CNN_MLP - Rede Neural Convulacional para dados de CreditScore 
#'@name CNN_MLP
#'
#'@description Script que gera uma rede neural convulacional para processamento
#' de dados em série temporal do histórico financeiro de cliente a partir da ba-
#' se de dados SouthCreditScore.asc
#'
#'@author Dieison Deprá
#'@seealso https://towardsdatascience.com/a-laymans-guide-to-building-your-first-image-classification-model-in-r-using-keras-b285deac6572
#'Para fazer tunning dos hyper parametros ver
#'@seealso https://towardsdatascience.com/simple-guide-to-hyperparameter-tuning-in-neural-networks-3fe03dad8594
#'Para análise e explicação das resultados do modelo:
#'@seealso https://blogs.rstudio.com/ai/posts/2018-01-11-keras-customer-churn/
################################################################################

#define o diretório corrente com base no arquivo que está sendo executado...
if (Sys.getenv("RSTUDIO") == 1) {
  #install.packages("rstudioapi")
  library(rstudioapi)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(getSrcDirectory(function(){})[1])  
}
cat ("Current path: ", getwd())

# Instalação do TensorFlow
pkg <- "tensorflow"
if (!requireNamespace(pkg, quietly = TRUE)) {
  install.packages(pkg)
  library(tensorflow)
  install_tensorflow()
}  
#library(pkg, character.only=TRUE)

# Instalação do Keras
pkg <- "keras"
if (!requireNamespace(pkg, quietly = TRUE)) {
  install.packages(pkg)
  install_keras()
}  
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

#Funções usadas: keras_model_sequential, layer_conv_2d, layer_max_pooling_2d, layer_dropout, layer_flatten, layer_dense
library(pkg, character.only=TRUE)

#Demais pacotes com instalação padrão, mas sem fazer load da biblioteca
pkgs <- c("BiocManager", "corrplot", "corrr", "dplyr", 
          "e1071", "forcats", "ggplot2", "heatmaply", "np",
          "lime", "magick", "magrittr", "shapper", "pryr", 
          "tidyverse", "tfruns", "tfestimators", "yardstick", 
          "rstudioapi", "libcoin",  "ggrepel", "shapr")

# Install packages not yet installed
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
  
  # Packages loading (opção por não carregar os pacotes)
  #invisible(lapply(pkgs, library, character.only = TRUE))
}

#library(BiocSklearn)
#library(gdata)

#ROCR::prediction
#ROCR::performance
library(ROCR)

#pROC::roc
library(pROC)

library(caret)

#função %<>% 
library(magrittr)

#funções do tensorflow
library(tfruns)

#corrplot::cor.mtest
#corrplot::corrplot
#library(corrplot)

cat ("Memory começo: ", pryr::mem_used())

#accuracy_score
#f1_score
#recall_score
import::from("./CNN_MLP_SCORES.R", accuracy_score, f1_score, recall_score)

#mostrarResultados
import::from("./CNN_MLP_SHOW_PERF.R", mostrarResultados)

#plotarResultados
import::from("./CNN_MLP_PLOTAR.R", plotarResultados, draw_confusion_matrix, plotarCorrPeriodo, plotarCorrCaract, plotarCorrCols, plotarLime)

cat ("Memory depois imports: ", pryr::mem_used())

################################################################################
#'Declaração de constantes
#'
#'#'Salvar o modelo da rede neural já treinada em arquivo
SALVAR_MODELO <- FALSE
#'
#'#'Número de características avaliadas:
FEATURES <- 20
#'
#'#'Número de amostras temporais
PERIODOS <- 12
#'
#'#'Pesos para noramalização de cada característica
PESOS <- c(4, 72, 4, 10, 18424, 5, 5, 4, 4, 3, 4, 4, 75, 3, 3, 4, 4, 2, 2, 2)
#'
#'Dimensionar normalização para o limite
LIMITE_NORM <- 255
#'
#'Interver probalidades para o case RUIM nos gráficos do LIME
INVERTER_EXPLAIN_PROBS <- TRUE #5
#
#'Exibir representação gráfica (imagem) dos clientes
DISPLAY_CLIENTS <- TRUE
#PLOTAR
PLOTAR <- TRUE
PLOTAR_CORR_PERIDO <- TRUE
PLOTAR_CORR_CARA <- TRUE
PLOTAR_CORR_COLS <- TRUE
PLOTAR_ROCR <- PLOTAR
PLOTAR_INFO <- FALSE
PLOTAR_LIME <- TRUE

PATH_PLOTS <<- paste0("./", if (tfruns::is_run_active()) paste0(tfruns::run_dir(), "../plots/", sep="") else "../figures/", sep="")
cat("tfruns_is_active? ", tfruns::is_run_active(), "\n")
cat("tfruns_run_dir? ", tfruns::run_dir(), "\n")
cat ("PATHS: ", PATH_PLOTS, "\n");
################################################################################

################################################################################
#' Funções genéricas
#' 
#'Faz leitura dos arquivos do diretório informado de acordo com o filtro
loadFiles <- function (local, filtro, expansao=1) {
  cat("local: ", local, ", filtro: ", filtro, "\n")
  arq.names <- list.files(path=local, pattern=glob2rx(filtro), full.names = F)
  qtd_arqs <- length(arq.names)
  retorno <- list(dados=list(NULL), nomes=c(NA, length(qtd_arqs)), y=c(rep(0), length(qtd_arqs)))
  
  qtdBons <- qtdRuins <- 0
  for (i in 1:qtd_arqs) {
    nome <- paste0(local, arq.names[i]);
    rt1 <- read.table(nome, colClasses = "integer")
    nome <- unlist(strsplit(arq.names[i], "\\."))[1]
    ruimOuBom <- 0
    if (startsWith(nome, "cli_good")) {
      ruimOuBom <- 1
      qtdBons <- qtdBons + 1
    } else {
      qtdRuins <- qtdRuins + 1
    }
    retorno$y[[i]] <- ruimOuBom
    retorno$nomes[[i]] <- nome
    retorno$dados[[i]] <- matrix(unlist(rt1), PERIODOS, FEATURES)
  }
  
  cat ('qtdArqs:', qtd_arqs, ', qtdBons: ', qtdBons, " qtdRuins: ", qtdRuins, '\n')
  
  retorno
}

#'Cria uma matrix de dados e um vetor de indices
prepareData <- function (pool, indices) {
  #'Cria matrix par ao conjunto de dados
  conjunto <- array(0, dim=c(length(indices), PERIODOS, FEATURES, 1))
  
  #'Popula conjuntos de dados e respostas
  for (i in 1:length(indices)) {
    for (j in 1:PERIODOS) {
      for (k in 1:FEATURES) {
        #'Normaliza o conjunto de dados de acordo com o pesos e o limite de normalização
        v1 <- pool[[indices[i]]][j, k]
        v2 <- v1 * LIMITE_NORM
        v3 <- v2 / PESOS[k]
        #v4 <- v3 / LIMITE_NORM
        conjunto[i, j, k, 1] <- v3
      }
    }
  }
  #normalize(conjunto)
  conjunto
}

################################################################################

#'marca inicio Script
scriptIniTime <- Sys.time()
cat ("Memory antes carregar arquivos: ", pryr::mem_used())

#'carregar arquivos com crédito score dos clientes
#'WARN: observar que dentro do IF as variávies são atribuídas com <<- ao invés de <-
#'      isso se deve ao fato que estamos criando variáveis globais
#'      para evitar que o TFRUNS limpe todo o contexto entre duas execuções e,
#'      dessa forma, evitamos de fazer a recarga dos arquivos de arquivos,
#'      otimizando o tempo de execução das simulações
if (!exists("CNN_MLP_LOADED") || (exists("CNN_MLP_NEED_RELOAD") && CNN_MLP_NEED_RELOAD == TRUE)) {
  CNN_MLP_LOADED <<- TRUE
  
  #'marca inicio carga e preparação dos dados
  loadPrepareIniTime <<- Sys.time()
  
  csArqs <<- loadFiles("./../data/", "cli_*")
  totalArqs <- length(csArqs$nomes)
  
  #'Indices em ordem aletória
  idx_random <- sample(totalArqs, totalArqs)
  idx_corte <-  ceiling(totalArqs * .60)
  
  #'Dados para o treino
  ds.train_idx <<- idx_random[1:idx_corte]
  ds.train <<- prepareData(pool = csArqs$dados, indices = ds.train_idx) 
  ds.train_y <<- csArqs$y[ds.train_idx]
  ds.train_nomes <<- csArqs$nomes[ds.train_idx]
  
  #'Dados para o teste
  ds.test_idx <<- idx_random[idx_corte+1:(totalArqs - idx_corte)]
  ds.test <<- prepareData(pool = csArqs$dados, indices = ds.test_idx) 
  ds.test_y <<- csArqs$y[ds.test_idx]
  ds.test_nomes <<- csArqs$nomes[ds.test_idx]
  
  #classes
  #ds.train_lab <<- to_categorical(ds.train_y) #Catagorical vector for training 
  #ds.test_lab <<- to_categorical(ds.test_y)#Catagorical vector for test classes
  
  #marca Fim
  loadPrepareFimTime <<- Sys.time()
}
cat ("Memory depois de carregar arquivos: ", pryr::mem_used())

# Hyperparameter flags ---------------------------------------------------
FLAGS <- flags(
  flag_numeric("dropout1", 0.25, 'Parâmetro dropout após camadas conv2d'),
  flag_numeric("dropout2", 0.45, 'Parâmetro dropout após camadas ocultas '),
  flag_integer("dense1Units", 196, 'Número de neurônios na primeira camada oculta'),
  flag_integer("dense2Units", 196, 'Número de neurônios na segunda camada oculta'),
  flag_numeric("validSplit", .21, 'Percentual de amostras utilizadas para validação'),
  flag_integer("epocas", 300, 'Número de épocas para treino'),
  flag_integer("verbose", 1, "Define se deve exibir gráfico de evolução do treino"),
  flag_numeric("kernel_reg1", 0.01, "Regularizador de kernel do tipo l2 - 1 comada conv2d"),
  flag_numeric("kernel_reg2", 0.02, "Regularizador de kernel do tipo l2 - 2 comada conv2d")
)

# Model Building
model_cnn <- keras_model_sequential() #-Keras Model composed of a 
#sgd <- optimizer_sgd(lr = 0.07)

#TODO: modificar estrutura pra o modelo de https://towardsdatascience.com/convolutional-neural-network-on-a-structured-bank-customer-data-358e6b8aa759
#Estrutura => conv2d > maxpool2d > dropout > conv2d > dropout > flatten > dense

#-----linear stack of layers
model_cnn %>%                  #---------Initiate and connect to #------------------------------------------------------------------#
  layer_conv_2d(filters = FEATURES,       #----------First convoluted layer
                kernel_size = c(6,4),             #---20 Filters with dimension 4x4
                activation = 'relu',              #-with a ReLu activation function
                kernel_initializer='random_normal',
                kernel_regularizer = keras::regularizer_l2(FLAGS$kernel_reg1),
                input_shape = c(PERIODOS,FEATURES,1)) %>%   
  #-----------------------------------------------------------------#
  layer_conv_2d(filters = FEATURES,       #---------Second convoluted layer
                kernel_size = c(4,4),             #---20 Filters with dimension 4x4
                #kernel_regularizer = keras::regularizer_l2(0.01),
                kernel_initializer='random_normal',
                activation = 'relu') %>%   
  #-----------------------------------------------------------------#
  layer_max_pooling_2d(pool_size = c(1,2)) %>%  #---------Max Pooling
  #-----------------------------------------------------------------#
  layer_dropout(rate = FLAGS$dropout1) %>%   #-------------------Drop out layer
  #-----------------------------------------------------------------#
  layer_flatten()%>%   #---Flattening the final stack of feature maps
  #-----------------------------------------------------------------#
  layer_dense(units = FLAGS$dense1Units, 
              kernel_regularizer = keras::regularizer_l2(FLAGS$kernel_reg2),
              kernel_initializer='random_normal',
              activation = 'relu'
  )%>% #-----Hidden layer
  #-----------------------------------------------------------------#
  layer_dense(units = FLAGS$dense2Units, 
              #kernel_regularizer = keras::regularizer_l1(0.01),
              kernel_initializer='random_normal',
              activation = 'softmax')%>% #-----Hidden layer
  #-----------------------------------------------------------------#
  layer_dropout(rate = FLAGS$dropout2)%>%     #-------------------Drop-out layer
  #-----------------------------------------------------------------#
  layer_dense(units = 1, activation = "sigmoid")%>% #-----Final Layer
  #-----------------------------------------------------------------#
  
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_adam(), #optimizer_adadelta(), #optimizer_adam(), #sgd,
          metrics = c('accuracy')
  )   # Compiling the architecture
#
cat ("Memory depois compilado: ", pryr::mem_used())

#---------------------------------fit model----------------------------

es <- callback_early_stopping(
  monitor = "val_accuracy",
  min_delta = 0.01,
  patience = 40,
  verbose = 0,
  mode = c("auto"),
  baseline = NULL,
  restore_best_weights = FALSE
)

lr <- callback_reduce_lr_on_plateau(monitor = "val_accuracy", factor = 0.1)

#marca inicio
trainIniTime <- Sys.time()
history <- model_cnn %>%
  fit(ds.train, 
      ds.train_y, 
      callbacks = c(es, lr),
      epochs = FLAGS$epocas,
      validation_split = FLAGS$validSplit,
      verbose = FLAGS$verbose
  )
#marca fim
trainFimTime <- Sys.time()
#se o treinamento foi interrompido pela função early_stopping então precisa ajustar o número de éopoca no history
history$params$epochs <- es$stopped_epoch + 1

cat ("Memory depois treino: ", pryr::mem_used())

#Limpar a saída de console
#cat("\014")

#------------------------------------------------------------------------
ds.train_eval <<- model_cnn %>% evaluate(ds.train, ds.train_y) 
#-----Classification
ds.train_pclass <- model_cnn %>% predict(ds.train) %>% `>`(0.5) %>% k_cast("int32")
ds.train_proba <<- model_cnn %>% predict(ds.train)
ds.train_rmse <<- mean(sqrt((matrix(ds.train_pclass) - ds.train_y) ** 2))
#----Results
ds.train_xtab <<- table(Predicted = matrix(ds.train_pclass), Actual = ds.train_y) 

#test

ds.test_eval <<- model_cnn %>% evaluate(ds.test, ds.test_y) #-----Evaluation of test set
ds.test_pclass <- model_cnn %>% predict(ds.test) %>% `>`(0.5) %>% k_cast("int32")
ds.test_proba <<- model_cnn %>% predict(ds.test)
ds.test_rmse <<- mean(sqrt((matrix(ds.test_pclass) - ds.test_y) ** 2))
ds.test_xtab <<- table(Predicted = matrix(ds.test_pclass), Actual = ds.test_y) #-----Results
# predict probabilities for test set
ds.test_yprobs <<- model_cnn %>% predict(ds.test, verbose=0)
# reduce to 1d array
ds.test_yprobs <<- ds.test_yprobs[, 1]

# Ajusta nomes
rownames(ds.train_xtab) <- rownames(ds.test_xtab) <- colnames(ds.train_xtab) <- colnames(ds.test_xtab) <- c("Ruins", "Bons")

#Pacote CARET
caret.confmat_train <- caret::confusionMatrix(ds.train_xtab, positive = "Bons")
caret.confmat_test <- caret::confusionMatrix(ds.test_xtab, positive = "Bons")

if ((!exists("CNN_MLP_SHOW_PLOTS") || CNN_MLP_SHOW_PLOTS == TRUE)) {
  
  #yardstick performance analysis
  yard.keras_tbl <- tibble::tibble(
    truth      = as.factor(ds.test_y) %>% forcats::fct_recode(yes = "1", no = "0"),
    estimate   = as.factor(matrix(ds.test_pclass)[, 1]) %>% forcats::fct_recode(yes = "1", no = "0"),
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
  rocr.pred <- ROCR::prediction(ds.test_proba, ds.test_y)
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
  if (PLOTAR_INFO) {
    infov.prec <- InformationValue::precision(ds.test_y, ds.test_pclass)
    infov.sens <- InformationValue::sensitivity(ds.test_y, ds.test_pclass)
    infov.spec <- InformationValue::specificity(ds.test_y, ds.test_pclass)
    infov.kap <- InformationValue::kappaCohen(ds.test_y, ds.test_pclass)
    infov.ks <- InformationValue::ks_stat(ds.test_y, ds.test_pclass)
    infov.auc <- InformationValue::AUROC(ds.test_y, ds.test_pclass)
  }
  
  #ROC
  ds.test_auc <- pROC::auc(ds.test_y, ds.test_yprobs)
  
  ################################################################################
  #Saída de texto 
  ################################################################################
  #Limpar console
  #cat("\014")
  
  #Plotar gráficos de resultado apenas se não estiver num processo de tunnig
  plotarResultados(
    494, #sample(dim(ds.test)[1] - 20, 1), 
    PLOTAR_ROCR, PLOTAR_INFO, PLOTAR_LIME, PLOTAR_CORR_PERIDO, PLOTAR_CORR_CARA, PLOTAR_CORR_COLS, DISPLAY_CLIENTS)
  draw_confusion_matrix(caret.confmat_train, "Matriz de resultado do Treino", "Performance do Treino")
  draw_confusion_matrix(caret.confmat_test, "Matriz de resultado do Teste", "Performance do Teste")
  mostrarResultados(rocr = PLOTAR_ROCR, info = PLOTAR_INFO)
  
  if (exists("FLAGS")) {
    print(FLAGS)
  }
  
  #
  cat("Tempo de carga e preparação (", format(loadPrepareFimTime,"%d/%m/%Y %H:%M:%S"), " - ", format(loadPrepareIniTime,"%d/%m/%Y %H:%M:%S"), "): ", (loadPrepareFimTime - loadPrepareIniTime), '\n')
  
}

#marca fim script
scriptFimTime <- Sys.time()

if (!exists("ultValAccuray")) {
  ultValAccuray <-- 0
}

nome_modelo <- "cnn_mlp"
tfrunsSave <- FALSE
valAccuracy <- history$metrics$val_accuracy[history$params$epochs]
if (tfruns::is_run_active() && ultValAccuray < valAccuracy) { # && valAccuracy > 0.973) {
  tfrunsSave <- TRUE
  ultValAccuray <-- valAccuracy
  nome_modelo <- file.path(tfruns::run_dir(), nome_modelo)
}

if (SALVAR_MODELO || tfrunsSave) {
  momento <- format(Sys.time(), "_%Y%m%d_%H%M%S")
  cat ("\ncaret analisys para Treino\n")
  print(caret.confmat_train)
  cat ("\ncaret analisys para Tests\n")
  print(caret.confmat_test)
  
  cat ("gravando modelo: ", nome_modelo, momento, '\n')
  model_cnn %>% save_model_tf(paste(nome_modelo, momento, ".tf", sep=""))
  model_cnn %>% save_model_hdf5(paste(nome_modelo, momento, ".h5", sep=""))
}

#
cat("Tempo de treino (", format(trainFimTime, "%d/%m/%Y %H:%M:%S"), " - ", format(trainIniTime,"%d/%m/%Y %H:%M:%S"), "): ", (trainFimTime - trainIniTime), '\n')
cat("Tempo do script (", format(scriptFimTime, "%d/%m/%Y %H:%M:%S"), " - ", format(scriptIniTime, "%d/%m/%Y %H:%M:%S"), "): ", (scriptFimTime - scriptIniTime), '\n') 
