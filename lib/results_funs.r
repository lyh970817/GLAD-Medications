multi_adjust <- function(model) {
    map(model, function(m) {
       p_cols <- grep("p", colnames(m)) 
       m[p_cols] <- m[p_cols] * 3

       m
    })
}


only_sig <- function(model) {
  modify(model, function(m) {
    r_lgl <- apply(m, 1, function(row) {
      lgl <- any(as.numeric(row) <= 0.05)
      lgl[is.na(lgl)] <- FALSE
      return(lgl)
    })

    m[r_lgl, ]
  })
}

