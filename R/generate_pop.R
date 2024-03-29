#growth functions

#' Generates an imaginary population
#'
#' @param n Number of samples to be generated
#' @param roundage Rounds the age to integer if 'TRUE'. Default is set as 'FALSE' which returns ages with dicimals.
#' @param p parameters for growth (p[1],p[2],p[3]) and mean_age p[4] and its sd p[5]
#'
#' @return data frame
#' @export
#' @details Fishtype can be selected from "sardine", "flounder", "mackerel", and "tuna". When type = "manual", then VBGF parameters need to be input manually as a vector of c(Linf, K, t0).
#' "sardine" consists of short lived organism with small body size. This species exhibit indeterminate growth with inflexion point of growth around 2 years old.
#' @importFrom magrittr "%>%"
#' @importFrom stats rgamma rnorm
#' @references Manabe, A., et al. (2018). A novel ontogenetic function incorporating energy allocation. PLoS ONE. e######.
#'
#' @examples


generate_pop <- function(n,p,roundage=FALSE){

  nage <- ceiling(sqrt(n))
  nlen <- ceiling(n/nage)

  # if(type=="sardine")  {par <- c(250,  0.340, -1.53, 3, 1)}  # Oshimo et al. (2009) Fish. Oceanogr. 18(5):346-358.
  # if(type=="sardine92"){par <- c(220,  0.649, -1.226,2, 1)} # Morimoto (2003) Fisheries Science. 69:745-754.
  # if(type=="flounder") {par <- c(358,  0.357, -0.15, 3, 2)}  # Manabe et al. (2018) PLoS ONE Fig 1 willowy flounder M
  # if(type=="mackerel") {par <- c(524,  0.19,  -1.61, 4, 2)}  # Lorenzo and Pajuelo 2010 South African J. Mar. Sci 17(1)
  # if(type=="tuna")     {par <- c(2570, 0.2,    0.83, 6, 4)}  # Secor et al. (2008) SCRS Growth of Atlantic bluefin tuna: direct age estimates

  tt <- age(n=nage,mean=p[4],sd=p[5],roundage) #roundage=TRUE gives integer age

  vb <- function(p,tt){
    p[1]*(1-exp(-p[2]*(tt-p[3])))
  }

  basedata <- data.frame(tt=tt,len=vb(p,tt))

  newdata<-data.frame(NULL,NULL)

  for(i in 1:length(basedata[,1])){
    tempdata <- data.frame(tt= rep(tt[i],nlen),len=rgamma(n=nlen,shape=basedata$len[i],rate=0.8))
     # tempdata <- data.frame(tt= rep(tt[i],nlen),len=rgamma(n=nlen,shape=basedata$len[i],rate=0.9))
      newdata <- rbind(newdata,tempdata)

  }

  popdata <- newdata[sample(nrow(newdata),n),]%>%
    dplyr::arrange(.,tt)

  return(popdata)

}



#' Title flounder data
#'
#' @param n sample size
#' @param seed seed, initially 41
#' @param roundage either return rounded age or not initally FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' flounder(1000)
#'

flounder <- function(n,seed=41,roundage=FALSE){
   set.seed(seed)
   pars <- c(358,  0.357, -0.15, 4, 2)
  floundata <- generate_pop(n,pars,roundage)
  return(floundata)
 }
