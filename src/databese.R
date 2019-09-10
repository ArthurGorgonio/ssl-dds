getBatch(database, temporal, index) {
  col <- match(temporal, colnames(database))
  newBatch <- which(database[, col] == unique(database[, col][index]))
}
