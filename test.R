# pruneInfreqCandidate2 <- function(C,F,k) {  
#    for (i in 1)
# }

Y = readData("ds_alphabetic.txt")

nT = length(Y)    # estimate number of transactions
w = maxLen <- max(sapply(Y, length))

sY <- lapply(Y, function(x) {c(x, rep(NA, maxLen - length(x)))})
T <- do.call(rbind, sY)
I <- sort(c(na.exclude(unique(c(T)))))
I = sequence(20)
k =3
C = t(combn(I,3))
F = t(combn(I,2))
F = F[-(1:4),]
inF = F[1:4,] 

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

