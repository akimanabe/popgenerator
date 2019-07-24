

#' Generate random ages
#'
#' @param n integer, number of age generated
#' @param mean positive numeric
#' @param sd positive numeric
#' @param roundage round age or allow dicimals. The function will return with ceiling.
#'
#' @return vector
#' @export
#'
#' @examples
#' age(n=100, mean=2, sd=1, roundage=TRUE)
#' age(n=10, mean=3, sd=1.5)

randomage <- function(n,mean,sd,roundage=FALSE){

  tempage <- abs(rnorm(n,mean,sd))
  tempage <- tempage[order(tempage)]

  if(roundage==TRUE){
    return(ceiling(tempage))
  }
  if(roundage==FALSE){
    return(tempage)
  }

}

