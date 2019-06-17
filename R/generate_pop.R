#growth functions

#' Generates an imaginary population
#'
#' @param n integer
#'
#' @return data frame
#' @export
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

  tt <- age(n=nage,mean=3,sd=1,roundage) #iwashi sample #roundage gives integer age

  par <- c(300,0.6,0) #iwashi sample

  if(type=="sardine"){par <- c(250, 0.340,-1.53)} # Oshimo et al. (2009) Fish. Oceanogr. 18(5):346-358.
  if(type=="sardine92"){par<-c(220,0.649,-1.226)} # Morimoto (2003) Fisheries Science. 69:745-754.

  vb <- function(par,tt){

    par[1]*(1-exp(-par[2]*(tt-par[3])))

  }

  basedata <- data.frame(tt=tt,len=vb(par,tt))

  newdata<-data.frame(NULL,NULL)

  for(i in 1:length(basedata[,1])){

      tempdata <- data.frame(tt= rep(tt[i],lage),len=rnorm(n=lage,mean=basedata$len[i],sd=basedata$tt[i]*5))
      newdata <- rbind(newdata,tempdata)

  }

  popdata <- newdata[sample(nrow(newdata),n),]%>%
    dplyr::arrange(.,tt)

  return(popdata)


}

#age <- ceiling(sqrt(n))
#lage <- ceiling(n/nage)
#totnum <- sample(nage*lage,n,replace=FALSE,prob=NULL) #adjust to n num
#res <- generate_pop(1000)
#ggplot2::ggplot(res)+  geom_point(aes(tt, len))+
#  xlim(0,round(max(1.2*res$tt)))+
#  ylim(0,round(max(1.2*res$len)))

# feat expected
## growthtype = c("list of major species with given params from fishbase or something")
## curvetype = c("vb", "logistic", "qVB") this allows indeterminate growth

# min age et max age ? like salmonids

#

#if(type=="sardine"){par <- c(250, 0.340,-1.53)} # Oshimo et al. (2009) Fish. Oceanogr. 18(5):346-358.
#if(type=="sardine92"){par<-c(220,0.649,-1.226)} # Norimoto (2003) Fisheries Science. 69:745-754.
