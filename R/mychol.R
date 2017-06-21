mychol <- function(A) {
  l <- matrix(0, nrow(A), nrow(A))
  l[1, 1] <- sqrt(A[1, 1])
  # Calculate elements
  for (i in 2:nrow(A)) {
    for (j in 1:i-1) {
      l[i, j] <- 1/l[j, j] * (A[i, j] - sum(l[i, 1:j-1] * l[j, 1:j-1]))
      l[i, i] <- sqrt(A[i, i] - sum(l[i, 1:i-1]**2))
    }
  }
  return(l)
}
