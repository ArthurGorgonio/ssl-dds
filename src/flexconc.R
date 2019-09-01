# Check which samples in data_x_it have equal classes than data_1_it
# Check in both matrixes if the confidence value are higger than thr_conf
classCheck <- function(data_1_it, data_x_it, thr_conf) {
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return(examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
confidenceCheck <- function(data_1_it, data_x_it, thr_conf) {
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return(examples)
}

# Return the confusion matrix
confusionMatrix <- function(model) {
  coluns_names <- colnames(base_teste)
  db_without_class <- match("class", coluns_names)
  test_db <- base_teste[, - db_without_class]
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

# Check in both matrixes if both confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentClassesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- searchClass(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return(examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentConfidencesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- searchClass(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return(examples)
}

# FlexCon-C the base algorithm
flexConC <- function(learner, pred_func, min_exem_por_classe, limiar, method) {
  # Initial setup, this is equal in all methods FlexCon-C1 and FlexCon-C2
  form <- as.formula(paste(classe, '~', '.'))
  data <- base
  thr_conf <- 0.95
  max_its <- 100
  verbose <- TRUE
  it <- 0
  N <- NROW(data)
  n_instancias_por_classe <- ddply(data, ~class, summarise,
                                   number_of_distinct_orders = length(class))
  n_classes <- NROW(n_instancias_por_classe) - 1
  qtd_exemplos_rot <- 0
  total_rot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  # FlexCon-C1 only
  if ((method == "1") || (method == "2")) {
    moda <- matrix(data = rep(0, length(base_original$class)),
                   ncol = length(levels(base_original$class)),
                   nrow = NROW(base_original), byrow = TRUE,
                   dimnames = list(row.names(base_original),
                   sort(levels(base_original$class), decreasing = FALSE)))
  }
  # FlexCon-C2 only
  add_rot_superv <- FALSE
  repeat {
    new_samples <- c()
    acertou <- 0
    it = it + 1
    if (qtd_exemplos_rot > 0) {
      qtd_exemplos_rot = 0
      treino_valido <- validTraining(data, id_conj_treino, n_classes,
                                     min_exem_por_classe)
      classificar <- validClassification(treino_valido, id_conj_treino,
                                         id_conj_treino_antigo, data, n_classes,
                                         min_exem_por_classe)
      if (classificar) {
        acc_local <- calcLocalAcc()
        thr_conf <- newConfidence(acc_local, limiar, thr_conf)
      }
    }
    model <- generateModel(learner, form, data, sup)
    prob_preds <- generateProbPreds(pred_func, model, data, sup)
    switch(method,
            "1" = {
              moda <- storageSum(prob_preds, moda)
              new_samples <- flexConC1(prob_preds, thr_conf, moda, it)
            },
            "2" = {
              moda <- storageFashion(prob_preds, moda)
              new_samples <- flexConC1(prob_preds, thr_conf, moda, it)
            },
            "3" = {
              model_superv <- generateModel(learner, form, data, sup)
              prob_preds_superv <- generateProbPreds(pred_func, model_superv,
                                                     data, sup)
              new_samples <- flexConC2(prob_preds, prob_preds_superv, thr_conf)
            }
    )
    if (length(new_samples)) {
      new_data <- data[(1:N)[-sup][new_samples], as.character(form[2])]
      if (add_rot_superv) {
        add_rot_superv <- FALSE
        new_data <- as.character(prob_preds_superv[new_samples, 1])
      } else {
        new_data <- as.character(prob_preds[new_samples, 1])
      }
      qtd_exemplos_rot <- length(new_data)
      total_rot <- total_rot + qtd_exemplos_rot
      acertou <- 0
      acerto <- (treinamento[(1:N)[-sup][new_samples], as.character(form[2])]
                 == new_data)
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto) {
        if (acerto[w] == TRUE) {
          acertou <<- acertou + 1
        }
      }
      id_conj_treino_antigo <- appendVectors(id_conj_treino_antigo,
                                             id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new_samples]
      sup <- c(sup, id_conj_treino)
    } else {
      thr_conf <- max(prob_preds[ , 2])
    }
    if ((it == max_its) || ((length(sup) / N) >= 1)) {
      break
    }
  }
  return(model)
}

flexConC1 <- function(prob_preds, thr_conf, moda, it) {
  if (it == 1) {
    prob_preds_1_it <<- prob_preds
    new_samples <- which(prob_preds[ , 2] >= thr_conf)
    rotulados <- data.frame(id = prob_preds[new_samples, 3],
                            cl = prob_preds[new_samples, 1])
  } else {
    rotulados <- classCheck(prob_preds_1_it, prob_preds, thr_conf)
    len_rotulados <- length(rotulados$id)
    if (len_rotulados == 0) {
      rotulados <- confidenceCheck(prob_preds_1_it, prob_preds, thr_conf)
      len_rotulados <- length(rotulados$id)
      if (len_rotulados == 0) {
        rotulados <- differentClassesCheck(prob_preds_1_it, prob_preds,
                                           thr_conf, moda)
        len_rotulados <- length(rotulados$id)
        if (len_rotulados == 0) {
          rotulados <- differentConfidencesCheck(prob_preds_1_it, prob_preds,
                                                 thr_conf, moda)
        }
      }
    }
  }
  new_samples <- rotulados$id
  return(new_samples)
}

# FlexCon-C2 funtion
flexConC2 <- function(prob_preds, prob_preds_superv, thr_conf) {
  prob_preds <- convertProbPreds(prob_preds)
  prob_preds_superv <- convertProbPreds(prob_preds_superv)
  prob_preds_con <- (prob_preds[, 2] >= thr_conf)
  prob_preds_superv_con <- (prob_preds_superv[, 2] >= thr_conf)
  prob_preds_cl <- prob_preds[, 1]
  prob_preds_superv_cl <-  prob_preds_superv[, 1]
  new_samples <- which((prob_preds_con & prob_preds_superv_con)
                       & (prob_preds_cl == prob_preds_superv_cl))
  if (length(new_samples) == 0) {
    new_samples <- which((prob_preds_con | prob_preds_superv_con)
                         & (prob_preds_cl == prob_preds_superv_cl))
    if (length(new_samples) == 0) {
      new_samples <- which((prob_preds_con & prob_preds_superv_con)
                           & (prob_preds_cl != prob_preds_superv_cl))
      if (length(new_samples)) {
        add_rot_superv <<- TRUE
      }
    }
  }
  return(new_samples)
}

