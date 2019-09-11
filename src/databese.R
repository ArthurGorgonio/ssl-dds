getBatch <- function(database, n) {
  return(database$get_points(n))
}

readData <- function(dbName) {
  data <- factorise(datastream_dataframe(dbName))
  return (data)
}
