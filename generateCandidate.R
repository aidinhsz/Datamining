generateCandidate <- function(C,k,nI) {
  if(k==2) {
    F = t(combn(C,k))
    return(F)
  } else {
    N = nrow(C)
    # sort data
    C = t(apply(C,1,sort))
    C = C[do.call(order,as.list(as.data.frame(C))),]
    F = matrix(nrow=N*(nI-k+1)/k, ncol=k)
    ind = 0

    for (m in 1:(N-1)) {
      breakFlag = FALSE
      n = 0
      while(!breakFlag & (m+n<N)){
        n = n + 1
#         print("m")
#         print(m)
#         print(C[m,1:(k-2)])
#         print("m+n")
#         print(m+n)
#         print(C[m+n,1:(k-2)])
#         print("-----------------")
        if (all(C[m,1:(k-2)]==C[m+n,1:(k-2)])) {
          ind = ind + 1
          F[ind,] = union(C[m,1:(k-1)],C[m+n,1:(k-1)])
#           print("code generated...")
#           print(F[ind,])
        } else {
#           print("-------------break-------------")
          breakFlag = TRUE
        }
      }
    }
    return(na.exclude(F))
  }
}
