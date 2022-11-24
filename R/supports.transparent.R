
supports.transparent <- function() {
    query <- dev.capabilities("semiTransparency")$semiTransparency
    if (is.na(query)) 
        query <- FALSE
    return(query)
}

