saveCM <- function(ensemblePred, oraclePred, iter) {
  cm <- table(ensemblePred, oraclePred)
  msg <- paste("Iteration:", iter)
  write(msg, file = "text.txt", append = T)
  write.table(paste(getAcc(cm),"%"), file = "text.txt", append = T, row.names = F, col.names = F)
}

#' @description Write in the output file the content.
#'
#' @usage write_archive (title, content, append = TRUE)
#'
#' @param title The title of the file.
#' @param content The content of the file.
#' @param append The method to write in the archive.
#'
writeArchive <- function(title, prefix, content, append = T, row = F, col = F, sep = " ") {
  write.table(content, paste(prefix, title, sep = "/"), append, sep = sep,
              row.names = row,  col.names = col)
}