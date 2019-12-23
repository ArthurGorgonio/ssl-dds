saveCM <- function(ensemblePred, oraclePred, iter, dataName) {
  cm <- table(ensemblePred, oraclePred)
  msg <- paste("Iteration:", iter)
  archiveName <- paste(dataName, "txt", collapse = "", sep = ".")
  writeArchive(archiveName, "../results/", msg)
  writeArchive(archiveName, "../results/", paste(getAcc(cm),"%"))
}

ensembleChange <- function(dataName, detail) {
  archiveName <- paste(dataName, "txt", collapse = "", sep = ".")
  writeArchive(archiveName, "../results/", detail)
}

#' @description Write in the output file the content.
#'
#' @usage write_archive (title, content, append = TRUE)
#'
#' @param title The title of the file.
#' @param content The content of the file.
#' @param append The method to write in the archive.
#'
writeArchive <- function(title, prefix, content, append = T, row = F, col = F,
                         sep = " ") {
  write.table(content, paste(prefix, title, sep = "/"), append, sep = sep,
              row.names = row,  col.names = col)
}