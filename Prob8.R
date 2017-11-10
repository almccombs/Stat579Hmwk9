#Problem 8

  #Problem 8.a

prettyvec <- function(filename) {
  iris <- readLines(con = filename)
  iris <- iris[-c(1:2, length(iris))]  #remove first 2 lines and blank last line
  indices <- grep(pattern = "\\s", iris)

  iris <- iris[-c(indices-1)]   #remove blank lines
  indices <- grep(pattern = "\\s", iris)

  final <- as.list(rep(NA, length(indices)))
  for (i in 1:length(indices)) {
    grpsize <- strsplit(x = iris[indices[i]], split = "=")
    final[[i]] <- rep(i-1, grpsize[[1]][2])
  }

  finalvec <- unlist(final)
  iris <- iris[-c(indices)]

  finaldf <- as.data.frame(cbind(finalvec, iris))
  names(finaldf) <- c("group", "observation")
  return(finaldf)
}

  #Problem 8.b
prettyvec("Iris1.out")
prettyvec("Iris2.out")
