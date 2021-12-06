library(RWeka)
library(RMOA)
source('src/database.R')

lengthBatch <- c(100, 250, 750, 500, 1000, 2500, 5000)

databases <- list.files(path = 'datasets/')
for (batch in lengthBatch) {
  for (dataset in databases) {
    data_name <- strsplit(dataset, '.', T)[[1]][1]
    file_name <- paste('balanced/', data_name, '_', batch, '_proportion.txt',
                       sep='')
    cat(file_name, '\n')
    train <- readData(dataset, path = 'datasets/')
    total_instances <- nrow(train$data)
    total_ratio <- proportions(table(train$data$class)) * 100
    it <- 1
    all_classes <- paste(c('batch', levels(train$data$class)), collapse=',')
    cat(all_classes, '\n', file=file_name, append=F)
    while (total_instances > (train$state)) {
      data <- getBatch(train, batch)
      x <- proportions(table(data$class[1:batch]))
      cat(format(it, width = 3, justify = 'right'), format(x, nsmall=2), '\n',
          sep = ',', file=file_name, append=T)
      it <- it + 1
    }
    cat(paste(c('True Ratios',
                format(l, nsmall = 4, digits=1, scientific = F)),
              collapse=','), '\n', file=file_name, append=T)
  }
}
