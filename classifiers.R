# Função para definir constantes ao longo do código
# Function to define constants in all code
defines <- function() {
  classe <<- "class"
  classifiers <<- c("naiveBayes", "rpartXse", "JRip", "IBk")
  extention <<- ".csv"
  funcs <<- c('func', 'f', 'f2', 'f2')
  obj <<- c(learner(classifiers[1], list()),
            learner(classifiers[2], list(se = 0.5)),
            learner(classifiers[3], list()),
            learner(classifiers[4], list(control = Weka_control(K = 3,
                                                                X = TRUE))))
}

