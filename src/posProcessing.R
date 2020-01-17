data <- read.csv("results/archive.csv", header = F)

fold <- 10


mean <- c()
sd <- c()
name <- c()
i <- 1
for (i in 1:(nrow(data) / 10)) {
  start <- (((i - 1) * fold) + 1)
  end <- (i - 1) * fold + fold
  aux <- data$V4[start:end]
  mean <- c(mean, round(mean(aux), 2))
  sd <- c(sd, round(sd(aux), 2))
  name <- c(name, paste("dataset", i, sep = ""))
}

output <- data.frame(name, mean, sd)

colnames(output) <- c("", "Recall", "desv pad")


write.csv(output, "resultadosCephasRecall.csv", row.names = F)
