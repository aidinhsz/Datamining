pruneInfreqCandidate2 <- function(C,F,k) {  
   nC = nrow(C)
   nF = nrow(F)

   pC <- matrix(data=NA,nrow = nC,ncol=k)
   ind = 0
   for (i in 1:nC) {
      temp = t(combn(C[i,],k-1))
      nS = nrow(temp)
      n = 0
      for (s in 1:nS) {
         for (j in 1:round(nF)) {
            if (all(temp[s,] %in% F[j,])) {
               n = n + 1
            break
            }
         }
      }
      if (n == nS) {
         ind = ind + 1
         pC[ind,] = C[i,]      
      }
   }
   return(na.exclude(pC))
}
  
