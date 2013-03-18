readData <- function (x) {
  
  x = read.table(x, sep="\n")
  y = apply(x,1, function(i)strsplit(i,"\\s")[[1]])
  return(y)
}