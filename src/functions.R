# Return the confusion matrix
confusionMatrix <- function(model) {
  coluns_names <- colnames(base_teste)
  db_without_class <- match("class", coluns_names)
  test_db <- base_teste[, -db_without_class]
  type <- 'class'
  class_test_bd <- base_teste$class
  confusion <- table(predict(model, test_db, type), class_test_bd)
  return(confusion)
}

# Convert each sample in prob_preds in character
convertProbPreds <- function(prob_preds) {
  aux <- sapply(prob_preds, is.factor)
  prob_preds[aux] <- lapply(prob_preds[aux], as.character)
  return(prob_preds)
}

generateModel <- function(learner, form, data, sup) {
  model <- runLearner(learner, form, data[sup, ])
  return(model)
}

# Generate a matrix with the sample, class and confidence value
generateProbPreds <- function(pred_func, model, data, sup) {
  prob_preds <- do.call(pred_func, list(model, data[-sup, ]))
  return(prob_preds)
}

# Calculate the acc and return
getAcc <- function(matrix, all) {
  acc <- ((sum(diag(matrix)) / all) * 100)
  return(acc)
}

#' @description This function set the class atribute to NA without change the
#' class of selected samples
#'
#' @usage newBase(base_rotulada, ids_treino_rot)
#'
#' @param base_rotulada the full dataset without changes
#' @param ids_treino_rot the vector with the selected samples
#'
#' @return a new dataset with some percents of the samples have the NA in class
#' atribute
#'
#' @examples
#' data(iris)
#'
#' H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100),
#' mode = "stratified")
#' base <- newBase(base_rotulada_treino, ids_treino_rot)
#' ids_treino_rot <- H2$tr
#'
#' @seealso rminer.holdout
#'
newBase <- function(base_rotulada, ids_treino_rot){
  base_rotulada[-ids_treino_rot, "class"] <- NA
  return(base_rotulada)
}

# Return the new confidence value changed by the cr value
#cr=5 nem umas das condiçoes vao ser aceitas
newConfidence <- function(acc_local, limiar, tx_conf) { 
  if ((acc_local > (limiar + 1)) && ((tx_conf - cr / 100) > 0.0)) {
    tx_conf <- tx_conf - cr / 100
  } else if ((acc_local < (limiar - 1)) && ((tx_conf + cr / 100) <= 1)) {
    tx_conf <- tx_conf + cr / 100
  }
  return(tx_conf)
}

# Search in the 'moda' vector the higger value of the sample (sum or vote)
searchClass <- function(i, moda) {
  return(colnames(moda)[which.max(moda[i, ])])
}

# Storage the vote of the classifier each iteration
storageFashion <- function(prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.numeric(prob_preds[x, ncol(prob_preds)])
    for (y in 1:(length(dist_classes))) {
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

storagePred <- function(predic, iterac) {
  if (iterac == 1) {
    soma <<- predic
    cat("criar vetor com o voto e a soma")
  } else {
    cat("incrementar o voto e a soma")
  }
}

# Storage the sum of the confidence for each iteration
storageSum <- function(prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.numeric(prob_preds[x, ncol(prob_preds)])
    for (y in 1:length(dist_classes)) {
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + prob_preds[x, 2]
        break
      }
    }
  }
  return (moda)
}


#' @description Make a supervised model and get the accuracy of this.
#'
#' @param cl the choosen classifier
#' @param base_rotulados_ini the dataset with the initial samples labeled.
#' @param baase_tst base de teste = dados inicialmente rotulados.
#'
#' @return Return the accuracy of the dataset with the initial samples
#' labeled.
#'
supAcc <- function(cl, base_rotulados_ini, base_tst){
  std <- supModel(cl, base_rotulados_ini)
  matriz_confusao_supervisionado <- confusionMatrix(std, base_tst)
  # acc_sup_3 <- getAcc(matriz_confusao_supervisionado, sum(matriz_confusao_supervisionado))
  return (getAcc(matriz_confusao_supervisionado, sum(matriz_confusao_supervisionado)))
}

#' @description A supervised model trained with the initial samples.
#'
#' @param cl the classifier to be used.
#' @param base_rotulados_ini the dataset with the initial samples labeled.
#'
#' @return Return a supervised classifier.
#'
supModel <- function(cl, base_rotulados_ini){
  switch (cl,
          "naiveBayes" = std <- naiveBayes(as.formula(paste(classe, '~', '.')),
                                           base_rotulados_ini),
          "rpartXse" = std <- rpartXse(as.formula(paste(classe, '~', '.')),
                                       base_rotulados_ini, se = 0.5),
          "JRip" = std <- JRip(as.formula(paste(classe, '~', '.')),
                               base_rotulados_ini),
          "IBk" = std <- IBk(as.formula(paste(classe, '~', '.')),
                             base_rotulados_ini,
                             control = Weka_control(K = as.integer(sqrt(
                               nrow(base_rotulados_ini))), X = TRUE))
  )
  return(std)
}

#' @description Check if the classification if valid.
#' se o treino for válido, a função apenas atribui o conj de treinamento antigo
#' ao novo conj. de treinamento e limpa o conj. antigo.
#' se o treino não for válido, a função junta o conj de treinamento antigo com o
#' novo e chama a funcao validTraining para validar se os dois conjuntos juntos 
#' podem ser treinados.
#'
#' @param treino_valido_i boolean for check if it's a valid train.
#' @param in_conj_treino vector with the samples to train.
#' @param id_conj_treino_antigo old vector whit the samples to train.
#' @param data the dataset with all samples.
#' @param N_classes the total of the classes in the dataset.
#' @param min_exem_por_classe the min samples of each class for training.
#'
#' @return a boolean to say if the classification is valid.
#'
validClassificationAntigo <- function(treino_valido_i, id_conj_treino,
                                      id_conj_treino_antigo, data, N_classes,
                                      min_exem_por_classe) {
  if (treino_valido_i) {
    conj_treino <<- data[id_conj_treino, ]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
    mudou_conj_treino <<- TRUE
  } else if (length(id_conj_treino_antigo) >= 1) {
    conj_treino <<- rbind(data[id_conj_treino, ], data[id_conj_treino_antigo, ])
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validTraining(data, id_conj_treino1, N_classes, min_exem_por_classe)
    mudou_conj_treino <<- TRUE
    if (treino_valido) {
      classificar <- TRUE
    } else {
      classificar <- FALSE
    }
  } else {
    classificar <- FALSE
  }
  return(classificar)
}

validClassification <- function(treino_valido_i, conj_treino_antigo_local,
                                conj_treino_local, N_classes, min_exem_por_classe) {
  if (treino_valido_i) {
    conj_treino <<- conj_treino_local
    conj_treino_antigo <<- c()
    classificar <- TRUE
    mudou_conj_treino <<- TRUE
  } else if (!is.null(nrow(conj_treino_antigo_local))) {
    conj_treino <<- rbind(conj_treino_local, conj_treino_antigo_local)
    treino_valido_i <- validTraining(conj_treino, N_classes, min_exem_por_classe)
    mudou_conj_treino <<- TRUE
    if (treino_valido_i) {
      classificar <- TRUE
    } else {
      classificar <- FALSE
    }
  } else {
    classificar <- FALSE
  }
  return(classificar)
}
#' @description Check if exists a min accetable samples per class.
#' o treino só é válido se todas as classes estiverem representadas no conj.
#' de treimento e se a quantidade de exemplos de cada classe for no mínimo (a qtdade
#' de exemplos da classe com menor representação no conj. ini. rot.?)
#'
#' @param data the all dataset.
#' @param id_conj_treino vector with the samples selectedes to train.
#' @param Nclasses the total of the classes in the dataset.
#' @param min_exem_por_classe the min samples of each class for training.
#'
#' @return a boolean to say if the training is valid.
#'
validTrainingAntigo <- function(data, id_conj_treino, Nclasses, min_exem_por_classe) {
  exemplos_classe <- ddply(data[id_conj_treino, ], ~class, summarise,number_of_distinct_orders = length(class))
  
  treino_valido <- FALSE
  if ((NROW(exemplos_classe)-1) == Nclasses) {
    for (x in 1:(NROW(exemplos_classe)-1)) {
      if (exemplos_classe$number_of_distinct_orders[x] >= min_exem_por_classe) {
        treino_valido <- TRUE
      } else {
        treino_valido <- FALSE
        return (treino_valido)
      }
    }
    return (treino_valido)
  }
  return (treino_valido)
}

validTraining <- function(data, Nclasses, min_exem_por_classe) {
  exemplos_classe <- ddply(data, ~class, summarise,number_of_distinct_orders = length(class))
  
  treino_valido <- FALSE
  if (NROW(exemplos_classe) == Nclasses) {
    for (x in 1:(NROW(exemplos_classe))) {
      if (exemplos_classe$number_of_distinct_orders[x] >= min_exem_por_classe) {
        treino_valido <- TRUE
      } else {
        treino_valido <- FALSE
        return (treino_valido)
      }
    }
    return (treino_valido)
  }
  return (treino_valido)
}

whichDB <- function(pattern) {
  tryCatch({  
    file <- list.files(pattern = pattern)
    bd <- readFile(file)
    return (as.integer((nrow(bd) / 10) + 1))
  }, 
  error = function(setIniBd){return(1)})
}