pruneInfreqCandidate <- function(C,F,k) {  
  if(k==2) {
    ind = apply(C,1, function(j)all(apply(t(combn(j,k-1)),1,function(i) any(apply(F,1,function(x) i == x)) == k-1)))
  } else {
    ind = apply(C,1, function(j)all(apply(t(combn(j,k-1)),1,function(i) max(colSums(apply(F,1,function(x) i == x))) == k-1)))
  }
  if(any(ind)==FALSE){print("all candidated pruned!!!")}
  return(as.matrix(C[ind,]))
}