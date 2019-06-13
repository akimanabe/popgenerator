#growth functions

generate_pop <- function(n){

  # nから年齢×年齢別体長へ割り振り
  # 基本的には年齢と年齢別体長は同じくらいの量
  nage <- ceiling(sqrt(n))
  lage <- ceiling(n/nage)

  # Totalが足りなくなると困るからCeilingで余分にGenerateして、最後にTotnum関数でn分無作為抽出
  # totnum <- sample(nage*lage,n,replace=FALSE,prob=NULL) #adjust to n num

  # temporary parameters - for imaginary sardine "iwashi sample"
  # In the future growthtype = c("sardine", "rockfish", "mackerel", "grouper", "anchovy", "manual")
  # where "manual" allows user to input pars
  tt <- age(n=nage,mean=3,sd=1,roundage=FALSE) #iwashi sample
  par <- c(300,0.6,0) #iwashi sample

  vb <- function(par,tt){

    par[1]*(1-exp(-par[2]*(tt-par[3])))

  }

  basedata <- data.frame(tt=tt,len=vb(par,tt))

  newdata<-data.frame(NULL,NULL)

  for(i in 1:length(basedata[,1])){

      tempdata <- data.frame(tt= rep(tt[i],lage),len=rnorm(n=lage,mean=basedata$len[i],sd=basedata$tt[i]*10))
      newdata <- rbind(newdata,tempdata)

  }

  #popdata <- newdata[sample(newdata[,1],n,replace=FALSE,prob=NULL),]
  popdata <- newdata[sample(nrow(newdata),n),]
  #i.pop <- tibble::as_tibble(newdata)
  #return(i.pop)

  return(popdata)
  #return(vb(par,tt))

  #basedata <- tibbe::tibble(tt,vb(c(100,0.6,0),tt)) %>%
  #dplyr::()


  #
}

#age <- ceiling(sqrt(n))
#lage <- ceiling(n/nage)
#totnum <- sample(nage*lage,n,replace=FALSE,prob=NULL) #adjust to n num
res <- generate_pop(1000)
