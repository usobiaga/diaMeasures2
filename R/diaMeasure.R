
METHODS <- c(
    osa     = 0L,
    lv      = 1L,
    dl      = 2L,
    hamming = 3L,
    lcs     = 4L,
    rdi     = 30L,
    ipd     = 31L
)

BINARYMETHODS  <- c(
    jac   = 1L,
    dice  = 2L,
    cover = 3L
)


#' @title Compute Dialectometrical measure
#'
#' @description \code{diaMeasure} computes the requested dialectometrical measure between variables of \code{data}.
#' 
#' @param data data frame, data.table or object that is coercible by data.table::as.data.table
#' containing all the variables that are referenced by the \code{formula}. The \code{data} must be in
#' long format meaning each response must be in a single column. \code{data(dsample)} is an example of the
#' data format.
#' @param formula formula indicating which variables represent identity variables and which measure
#' variables. See Examples.
#' @param value.var character vector of length one indicating which variable of data contains the linguistical
#' values. See Examples.
#' @param measure  method: Dissimilarity index, match to ‘"osa"’,
#' ‘"lv"’, ‘"dl"’, ‘"hamming"’, ‘"lcs"’, ‘"rdi"’, ‘"ipd"’.
#' @param binary.index binary index to be used with multiple response. For measures ‘"rdi"’ and ‘"ipd"’
#' "jac" (jaccard) and "dice" (dice) are available. For the rest of measures "cover" (cover set distance)
#' must be used.
#' @param weight pondreation for the distance metrics. For "ipd" weights must be numeric vector of length
#' one. The higher the weight for "ipd" the less impact very rare linguistical values have. See
#' Goebsl
#' @param q currently unused.
#' @param p currently unused.
#' @param bt currently unused.
#' @param useBytes Perform byte-wise comparison, see
#'   stringdist-encoding (from the stringdist package).
#' @param variable.dist logical of length one indicating if the dialectometrical distance needs to be
#' computed between linguistical identities (FALSE) or linguistical variables (TRUE).
#'
#' @return a vector with dialectometric distances (can be coerced into matrix). 
#'
#' @export
#'
#' @import data.table stringdist
#' 
#' @useDynLib diaMeasures2 dia_measure_C
#' 
#' @examples
#'
#' data(dsample)
#' 
#' ## The linguistical identities are "gender" and "location" meaning that each line
#' ## that belongs to the same location  and gender will be grouped. The distance between
#' ## each gender and each location will be computed
#' measure <- diaMeasure(dsample, gender + location ~ question, 'answer', 'rdi')
#' print(measure)
#'
#' ## if the linguistical identity is only the gender then all the reponses that each gender has
#' ## given belong to a single group. The distance between genders will be computed.
#' measure <- diaMeasure(dsample, gender ~ question, 'answer', 'rdi')
#' print(measure)
#'
#' ## locations defining the linguistical identities
#' measure <- diaMeasure(dsample, location ~ question, 'answer', 'rdi')
#' print(measure)
#'
#' ## measures between the questions instead of the locations
#' measure <- diaMeasure(dsample, location ~ question, 'answer', 'rdi', variable.dist = TRUE)
#' print(measure)
#'
diaMeasure <- function(data, formula, value.var,
                       measure = c('lv', 'rdi', 'ipd', 'osa', 'lv', 'dl', 'hamming', 'lcs'),
                       binary.index = c('jac', 'dice', 'cover'),
                       weight = c(d = 1, i = 1, s = 1, t = 1),
                       q = 1L, p = 0, bt = 0, useBytes = FALSE, variable.dist = FALSE){

    stopifnot(
        all(is.finite(weight)),
        all(weight > 0),
        all(weight <=1),
        q >= 0,
        p <= 0.25,
        p >= 0,
        is.logical(useBytes),
        is.logical(variable.dist),
        ifelse(measure %in% c('osa','dl'), length(weight) >= 4L, TRUE),
        ifelse(measure %in% c('lv', 'lv01', 'jw') , length(weight) >= 3L, TRUE),
        ifelse(measure %in% 'ipd', length(weight) >= 1L, TRUE)
    )
    
    ## coerce data to data.table
    if (!data.table::is.data.table(data)){
        data <- as.data.table(data)}
    
    data[[value.var]] <- as.character(data[[value.var]])
    
    if (!useBytes){
        data[[value.var]] <- enc2utf8(data[[value.var]])
    }

    mf <- data.table::dcast(data,
                            formula,
                            fun.aggregate = function(x) list(unique(x)),
                            value.var = value.var)
    ## attributes
    idnbr <- length(all.vars(formula[[2L]]))
    if (variable.dist){
        vars <- all.vars(formula[[3L]])
        labels <- names(mf)[-idnbr]
    } else {
        vars <- all.vars(formula[[2L]])
        labels <- do.call(paste, mf[, ..vars])
    }
    attrs <- list(Size = length(mf[[1]]), Labels = labels, class = c('diaMeasure', 'dist'))
    mf <- mf[, -(1:idnbr)]
    
    ## measures
    measure  <- match.arg(measure)

    ## jw is currently unused.
    if (measure == 'jw')
        weight <- weight[c(2, 1, 3)]

    if (measure == 'ipd'){
        ma <- any(sapply(unlist(mf, FALSE), length) > 1L)
        if (ma) stop ("IPD can't be applied to data with multiple answers per language identity")
    }
    measure <- METHODS[measure]

    if (missing(binary.index)) binary.index <- 'dice'
    binary.index <- match.arg(binary.index)
    binary.index <- BINARYMETHODS[binary.index]
    result <- .Call(dia_measure_C, 
                    ql = mf,
                    measure = measure,
                    binary_index = binary.index,
                    weight = weight,
                    q = q,
                    p = p,
                    bt = bt,
                    useBytes = useBytes,
                    nthread = 1L,
                    variablesDist = variable.dist)
    attributes(result) <- attrs
    return (result)
}


#' Coertion to matrix
#'
#' Coerce the diaMeasure object into a  matrix class. Uses the as.matrix method from the package dist.
#'
#' @param x object of class diaMeasure
#' @param ... unused
#' 
#' @export
as.matrix.diaMeasure <- function(x, ...){
    d <- sqrt(length(x) * 2 +  length(attr(x, 'Labels')))
    y <- matrix(0, d, d)
    y[upper.tri(y)] <- x
    y[lower.tri(y)] <- t(y)[lower.tri(y)]
    colnames(y) <- rownames(y) <- attr(x, 'Labels')
    return (y)
}

#' Coerce to diaMeasure
#'
#' Coertion to diaMeasure, see the corresponding method.
#'
#' @param x Object to be coerced
#' @param ... parameters passed to other methods
#' @export
as.diaMeasure <- function(x, ...) UseMethod('as.diaMeasure')


#' Coerce matrix to diaMeasure
#'
#' Coerce matrix to diaMeasure.
#'
#' @param x square matrix
#' @param idVars attributes
#' @param ... parameters passed to other methods
#' 
#' @export
as.diaMeasure.matrix <- function(x, idVars, ...){
    if (nrow(x) != ncol(x)) stop ('"x" should be square')
    r <- range(diag(x))
    if ((r[2] - r[1]) > 1e-07) stop ('"x" non equal values in diagonal')
    y <- x[upper.tri(x)]
    attr(y, 'diagv') <- x[1]
    attr(y, 'Size') <- ncol(x)
    if (is.null(colnames(x))) colnames(x) <- as.character(1:ncol(x))
    attr(y, 'Labels') <- colnames(x)
    attr(y, 'class') <- 'diaMeasure'
    if (missing(idVars)) idVars <- data.frame(Labels = colnames(x), stringAsFactors = FALSE)
    attr(y, 'idVars') <- idVars
    return (y)
}
