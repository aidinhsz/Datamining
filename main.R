# T: number of transaction
# N: number of items
# w: maximum length of items
# I: set of all items

message('reading data from text file...')
# Y = readData("ds_alphabetic.txt")
# Y = readData("test.txt")
Y = lapply(readData("course-text.txt"),sort)
nT = length(Y)    # estimate number of transactions
w = maxLen <- max(sapply(Y, length))

# generate symmetric data sets
message('sorting data set...')
sY <- lapply(Y, function(x) {c(x, rep(NA, maxLen - length(x)))})
T <- do.call(rbind, sY)
I <- sort(c(na.exclude(unique(c(T)))))

nI = length(I)    # estimate numer of all items
minSup = 0.3     # intiailize minimum support value
L = list()

k = 1
# estimate frequent items set for 1-itemset
cat(k,"-itemset:\n")
message('generating candidates...')
C = I
N = length(I)
SC = rep(0,N)
message('computing supports...')
for (j in 1:nT) {
  idC = sapply(C,function(i) all(i%in% T[j,])) 
  SC = SC + idC
}

S = SC/nT
F = as.matrix(C[S>minSup])
cat("nC:",length(C),',    ')
cat("nF:",length(F),'\n')

if (length(F)==0) {
   message('No frequenc itemset discoverd. Decrease minSup value!!')
   stop()
}


breakFlag = FALSE
while (!breakFlag) {
   L[[k]] = F
   k = k+1
   cat(k,"-itemset:\n")
   message('generating candidates...')
   C = generateCandidate(F,k,nI)       # apply candidate generation
   if (length(C) == k ) {
      L[k] = C
      message('no candidates generated. process terminated.')
      break
   } else if (length(C) == 0 ) {
      message('no candidates survived from pruning. process terminated.')
      break
   }
   message('pruning candidates...')
   C = pruleneInfreqCandidate2(C,F,k)     # apply candidate candidate pruning
   if (length(C)==0) {
      message('no candidates survived from pruning. process terminated.')
      break
   }
  
   message('computing supports...')
   SC = rep(0,nrow(C))    # initialize suportcount vector
   for (j in 1:nT) {
      # indentify all candidates that belong to transaction(j)
      idC = apply(C,1,function(i) all(i%in%T[j,]))
      # increment supportcount vector
      SC = SC + idC
   }

   # estimate support for candidates
   S = SC/nT

   # estimate frequent "k-itemset"
   F = (as.matrix(C[S>minSup,]))
   cat("nC:",nrow(C),',    ')
   cat("nF:",nrow(F),'\n')
   if (length(F) == k ) {
      breakFlag = TRUE
      L[k] = F
   }  
   else if (length(F) == 0 ) {
      breakFlag = TRUE
   }
}

writeData(L)






