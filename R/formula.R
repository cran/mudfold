
make.formulas <- function(data, blocks = make.blocks(data), 
                          predictorMatrix = NULL) {
  data <- check.dataform(data)
  formulas <- as.list(rep("~ 0", length(blocks)))
  names(formulas) <- names(blocks)
  
  for (h in names(blocks)) {
    y <- blocks[[h]]
    if (is.null(predictorMatrix)) {
      predictors <- colnames(data)
    } else {
      type <- predictorMatrix[h, ]
      predictors <- names(type)[type != 0]
    }
    x <- setdiff(predictors, y)
    formulas[[h]] <- paste(paste(y, collapse = "+"), "~", 
                           paste(c("0", x), collapse = "+"))
  }
  
  formulas <- lapply(formulas, as.formula)
  formulas
}

name.formulas <- function (formulas, prefix = "F") 
{
  if (!is.list(formulas)) {
    stop("Argument `formulas` not a list", call. = FALSE)
  }
  if (!all(sapply(formulas, is.formula) | sapply(formulas, 
                                                 is.list))) {
    stop("Not all elements in `formulas` are a formula or a list")
  }
  if (is.null(names(formulas))) 
    names(formulas) <- rep("", length(formulas))
  inc <- 1
  for (i in seq_along(formulas)) {
    if (names(formulas)[i] != "") 
      next
    y <- lhs(formulas[[i]])
    if (length(y) == 1) {
      names(formulas)[i] <- y
    }
    else {
      names(formulas)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  formulas
}

check.formulas <- function (formulas, data) 
{
  formulas <- name.formulas(formulas)
  formulas <- handle.oldstyle.formulas(formulas, data)
  formulas <- lapply(formulas, expand.dots, data)
  if (any(sapply(formulas, is.list))) {
    return(formulas)
  }
  formulas <- lapply(formulas, as.formula)
  formulas
}

extend.formula <- function(formula = ~ 0,
                           predictors = NULL,
                           auxiliary = TRUE,
                           include.intercept = FALSE, ...) {
  if (!is.formula(formula)) formula <- ~ 0
  
  # handle dot in RHS
  if (hasdot(formula)) {
    if (length(predictors) > 1)
      fr <- as.formula(c("~", paste(predictors, collapse = "+")))
    else 
      fr <- ~ 0
  } else 
    fr <- reformulate(c(".", predictors))
  
  if (auxiliary) formula <- update(formula, fr, ...)
  if (include.intercept) formula <- update(formula, ~ . + 1, ...)
  formula
}



handle.oldstyle.formulas <- function(formulas, data) {
  # converts old-style character vector to formula list
  oldstyle <- length(formulas) == ncol(data) && is.vector(formulas) && 
    is.character(formulas)
  if (!oldstyle) return(formulas)
  formulas[formulas != ""] <- "~ 0"
  fl <- as.list(formulas)
  names(fl) <- names(formulas)
  fl
}


is.empty.model.data <- function (x, data) 
{
  tt <- terms(x, data = data)
  (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}

lhs <- function(x) all.vars(update(x, . ~ 1))

is.formula <- function(x){
  inherits(x, "formula")
}

hasdot <- function(f) {
  if(is.recursive(f)) {
    return(any(sapply(as.list(f), hasdot)))
  } else {
    f == as.symbol(".")}
}

expand.dots <- function(formula, data) {
  if (!is.formula(formula)) return(formula)
  if (!hasdot(formula)) return(formula)
  
  y <- lhs(formula)
  x <- setdiff(colnames(data), y)
  fs <- paste(paste(y, collapse = "+"), "~", paste(x, collapse = "+"))
  as.formula(fs)
}
