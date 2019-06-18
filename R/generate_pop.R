#growth functions

#' Generates an imaginary population
#'
#' @param n Number of samples to be generated
#' @param type Fish growth type to provide specific growth parameters for von Bertalanffy function. Type can be selected from "sardine", "sardine92", "flounder", "mackerel", "tuna", and "manual". If it is 'manual' parameter needs to be input. If it is 'NULL' type "sardine" is employed.
#' @param roundage Rounds the age to integer if 'TRUE'. Default is set as 'FALSE' which returns ages with dicimals.
#'
#' @return data frame
#' @export
#' @details Fishtype can be selected from "sardine", "flounder", "mackerel", and "tuna". When type = "manual", then VBGF parameters need to be input manually as a vector of c(Linf, K, t0).
#' "sardine" consists of short lived organism with small body size. This species exhibit indeterminate growth with inflexion point of growth around 2 years old.
#'
#' @references Manabe, A., et al. (2018). A novel ontogenetic function incorporating energy allocation. PLoS ONE. e######.
#'
#' @examples





generate_pop <- function(n,type="sardine",roundage=FALSE){

  # nから年齢×年齢別体長へ割り振り
  # 基本的には年齢と年齢別体長は同じくらいの量
  nage <- ceiling(sqrt(n))
  lage <- ceiling(n/nage)

  # Totalが足りなくなると困るからCeilingで余分にGenerateして、最後にTotnum関数でn分無作為抽出
  # totnum <- sample(nage*lage,n,replace=FALSE,prob=NULL) #adjust to n num

  # temporary parameters - for imaginary sardine "iwashi sample"



#  par <- c(300,0.6,0) #iwashi sample

  if(type=="sardine")  {par <- c(250,  0.340, -1.53, 3, 1)}  # Oshimo et al. (2009) Fish. Oceanogr. 18(5):346-358.
  if(type=="sardine92"){par <- c(220,  0.649, -1.226,2, 1)} # Morimoto (2003) Fisheries Science. 69:745-754.
  if(type=="flounder") {par <- c(358,  0.357, -0.15, 3, 2)}  # Manabe et al. (2018) PLoS ONE Fig 1 willowy flounder M
  if(type=="mackerel") {par <- c(524,  0.19,  -1.61, 4, 2)}  # Lorenzo and Pajuelo 2010 South African J. Mar. Sci 17(1)
  if(type=="tuna")     {par <- c(2570, 0.2,    0.83, 6, 4)}  # Secor et al. (2008) SCRS Growth of Atlantic bluefin tuna: direct age estimates

  tt <- age(n=nage,mean=par[4],sd=par[5],roundage) #roundage=TRUE gives integer age

  vb <- function(par,tt){
    par[1]*(1-exp(-par[2]*(tt-par[3])))
  }

  basedata <- data.frame(tt=tt,len=vb(par,tt))

  newdata<-data.frame(NULL,NULL)

  for(i in 1:length(basedata[,1])){

      #tempdata <- data.frame(tt= rep(tt[i],lage),len=rnorm(n=lage,mean=basedata$len[i],sd=basedata$tt[i]*5))
      tempdata <- data.frame(tt= rep(tt[i],lage),len=rgamma(n=lage,shape=basedata$len[i],rate=1))
      newdata <- rbind(newdata,tempdata)

  }

  popdata <- newdata[sample(nrow(newdata),n),]%>%
    dplyr::arrange(.,tt)

  return(popdata)
  grapher <-
  ggplot2::ggplot(popdata)+
    geom_point(aes(tt,len))+
    xlim(0,1.2*round(max(popdata$tt)))+
    ylim(0,1.2*round(max(popdata$len)))

 #return(list(popdata,grapher))
}


#data with limited proportion of "aged" population
set.seed(10)
popdata <- generate_pop(1000,type="flounder",roundage=TRUE)

aged <- dplyr::sample_n(popdata,200)
disaged <- dplyr::sample_n(popdata,800)

aged <- data.frame(tt = aged$tt,len = round(aged$len))
disaged <- data.frame(tt = rep(NA,length(disaged$tt)),len=round(disaged$len))

