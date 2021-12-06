library(stringr)

args <- commandArgs(TRUE)

csv <- args[1]


if (is.na(args[2])) {
    path <- './'
} else {
    path <- args[2]
}
# csv <- 'mediasElectricity.csv'
data <- read.csv(csv)

# print(data)
col <- c("#8500bbaa", "#ba8b13AA", "#00bb7daa")

metric <- c('Acurácia', 'F-Score', 'Kappa')
batches <- c(100, 250, 500, 750, 1000, 2500, 5000)
div <- max(batches) / batches


names <- c()
index <- c()
right_drift <- c()

for (i in 1:length(batches)) {
#     aux <- seq(i, nrow(data), length(batches))
#     index <- c(index, aux)
    names <- c(names, rep(batches[i], 3))
    index <- ((i - 1) * 3 + 1):(i * 3)
    right_drift <- c(right_drift, data$Drifts[index] / div[i])
}

approaches = c('DyDaSL N', 'DyDaSL FT', 'DyDaSL W')

space <- c(0.2, rep(c(0.2, 0.2, 1), length(batches)))
space <- space[1:length(space)-1]



for (i in 1:3) {
    img_name <- paste(str_sub(strsplit(csv, ".", T)[[1]][1], 7), metric[i], ".png", sep="")

    png(paste(path, img_name, sep=""), 802, 802)

    aux <- barplot(data[, i+2], beside=T, col=col, ylim=c(0, 1.1), space=space,
                   xlab='Métodos', ylab=metric[i], names.arg=names)

    legend('top', approaches, fill=col, ncol=3, bty='n')

    text(x=aux, y=data[, i+2], pos=3, col='red', label=round(data[, i+2], 2))

    dev.off()
}

png(paste(path, str_sub(strsplit(csv, ".", T)[[1]][1], 7), "drifts", ".png", sep="")
    , 802, 802)

aux <- barplot(data[, 6], beside=T, col=col, space=space, xlab='Métodos',
               names.arg=names, ylab='Drifts',
               ylim=range(pretty(c(0, max(data[, 6]) + 1)))
              )

legend('top', approaches, fill=col, ncol = 3, bty='n')

text(x=aux, y=data[, 6], label=data[, 6], pos=3, col='red')

dev.off()


png(paste(path, str_sub(strsplit(csv, ".", T)[[1]][1], 7), "drifts-suavizado-", ".png", sep="")
    , 802, 802)

aux <- barplot(right_drift, beside=T,
               col=col, space=space, xlab='Métodos', names.arg=names,
               ylab='Drifts', ylim=range(pretty(c(0, max(right_drift) + 1)))
              )

legend('top', approaches, fill=col, ncol = 3, bty='n')

text(x=aux, y=right_drift, label=right_drift, pos=3, col='red')

dev.off()
