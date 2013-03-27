writeData <- function(L) {
   lapply(Fk_list,function(y) apply(y,1,function(x) itemVector[x]))
   for (k in 1:length(L)) {
      temp <-paste(k,"-itemset frequent pattern") 
      names(L)[k] = temp
   }
   write.list(L, filename ="frequentItemSet.txt", append = FALSE, closefile = TRUE, outfile) 
}
