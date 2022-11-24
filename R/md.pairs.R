
md.pairs <- function(data) {
    # calculates pairwise missing data statistics
    # rr:  response-response pairs
    # rm:  response-missing pairs
    # mr:  missing-response pairs
    # mm:  missing-missing pairs
    if (!(is.matrix(data) || is.data.frame(data))) 
        stop("Data should be a matrix or dataframe")
    if (ncol(data) < 2) 
        stop("Data should have at least two columns")
    
    r <- !is.na(data)
    rr <- t(r) %*% r
    mm <- t(!r) %*% (!r)
    mr <- t(!r) %*% r
    rm <- t(r) %*% (!r)
    return(list(rr = rr, rm = rm, mr = mr, mm = mm))
}
