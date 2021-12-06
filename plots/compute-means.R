setwd('~/workspace/ssl-dds/plots/')

paths <- c('acc/', 'fscore/', 'kappa/')
for (path in paths) {
  media_data_frame <- data.frame()
  files <- list.files(path = path, pattern = '*.csv')
  for (file in files) {
    data <- read.csv(paste(path, file, sep=''), sep='\t', row.names = 1, header = F)
    media <- c()
    for (i in 1:3){
      media <- c(media, round(mean(data[,i]) * 100, 2))
    }
    media_data_frame <- rbind(media_data_frame, media)
    media <- c()
    for (i in 1:3){
      media <- c(media, paste(round(mean(data[,i]) * 100, 2), ' ± ',
                              round(sd(data[,i]) * 100, 2), sep=''), ',')
    }
    cat(file, '\n', format(c('DyDaSL - N', 'DyDaSL - FT', 'DyDaSL - W'), width=13,
                           justify='centre'),
        '\n', media[1:5], '\n', rep('-', 30), '\n', append=T, file='MEDIA-DESVIO.txt')
  }
  cat('\n\n\nPATHHHH=', path)
  if (path == 'fscore/') {
    path <- 'f1/'
  }
  datasets <- c()
  batches <- c()
  for (file in files) {
    datasets <- c(datasets, strsplit(file, '_')[[1]][1])
    name <- paste(strsplit(path, '/')[[1]][1], '-', sep='')
    batches <- c(batches, strsplit(strsplit(file, '[.]')[[1]][1], name)[[1]][2])
  }
  datasets <- unique(datasets)
  batches <- unique(batches)
  
  # aux <- seq((((j - 1) * length(batches)) + 1), (j * length(batches)))
  index_dataset <- 1:length(files)
  index_batch <- c()
  
  for (j in 1:length(batches)) {
    aux <- seq(j, length(files), length(batches))
    index_batch <- c(index_batch, aux)
  }
  
  for (k in 1:length(datasets)) {
    ini <- ((k - 1) * length(batches) + 1)
    fim <- k * length(batches)
    media <- c()
    for (i in 1:3) {
      media <- c(media, paste(round(mean(media_data_frame[ini:fim, i]), 2),
                              ' ± ', round(sd(media_data_frame[ini:fim, i]), 2),
                              sep=''), ',')
    }
    cat(path, '\t', datasets[k], '\n',
        format(c('DyDaSL - N', 'DyDaSL - FT', 'DyDaSL - W'), width=13,
                           justify='centre'),
        '\n', media[1:5], '\n', rep('-', 30), '\n', append=T,
        file='MEDIA-DESVIO-BASEDEDADOS.txt')
  }
  
  for (k in 1:length(batches)) {
    ini <- ((k - 1) * length(datasets) + 1)
    fim <- k * length(datasets)
    media <- c()
    for (i in 1:3) {
      media <- c(media, paste(
        round(mean(media_data_frame[index_batch[ini:fim], i]), 2), ' ± ',
        round(sd(media_data_frame[index_batch[ini:fim], i]), 2), sep=''),
        ',')
    }
    cat(path, '\t', batches[k], '\n',
        format(c('DyDaSL - N', 'DyDaSL - FT', 'DyDaSL - W'), width=13,
               justify='centre'),
        '\n', media[1:5], '\n', rep('-', 30), '\n', append=T,
        file='MEDIA-DESVIO-BATCHSIZE.txt')
  }
}

path='drift/'

media_data_frame <- data.frame()
files <- list.files(path = path, pattern = '*.csv')
for (file in files) {
  data <- read.csv(paste(path, file, sep=''), sep='\t', row.names = 1, header = F)
  media <- c()
  for (i in 1:3) {
    media <- c(media, round(sum(data[,i]), 2))
  }
  media_data_frame <- rbind(media_data_frame, media)
}
for (k in 1:length(datasets)) {
  ini <- ((k - 1) * length(batches) + 1)
  fim <- k * length(batches)
  media <- c()
  for (i in 1:3) {
    media <- c(media, paste(round(mean(media_data_frame[ini:fim, i]), 2),
                            ' ± ', round(sd(media_data_frame[ini:fim, i]), 2),
                            sep=''), ',')
  }
  cat(path, '\t', datasets[k], '\n',
      format(c('DyDaSL - N', 'DyDaSL - FT', 'DyDaSL - W'), width=13,
             justify='centre'),
      '\n', media[1:5], '\n', rep('-', 30), '\n', append=T,
      file='MEDIA-DESVIO-BASEDEDADOS.txt')
}

for (k in 1:length(batches)) {
  ini <- ((k - 1) * length(datasets) + 1)
  fim <- k * length(datasets)
  media <- c()
  for (i in 1:3) {
    media <- c(media, paste(
      round(mean(media_data_frame[index_batch[ini:fim], i]), 2), ' ± ',
      round(sd(media_data_frame[index_batch[ini:fim], i]), 2), sep=''),
      ',')
  }
  cat(path, '\t', batches[k], '\n',
      format(c('DyDaSL - N', 'DyDaSL - FT', 'DyDaSL - W'), width=13,
             justify='centre'),
      '\n', media[1:5], '\n', rep('-', 30), '\n', append=T,
      file='MEDIA-DESVIO-BATCHSIZE.txt')
}
