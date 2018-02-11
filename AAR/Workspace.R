#--------------------------------------------------------------------------
# getYFData - Function to get historical daily stock data from Yahoo Finance.
# 
# Author: Sameer Gupta
# 
# Usage: msft <- getYFData("MSFT", as.Date("2001-01-01"), as.Date("2001-01-31")
#        The output is a data frame with Date, Open, High, Low, Close, Volume
#        and Dividend Adj. Close columns. 
#
#----------------------------------------------------------------------------

library(zoo)

getYFData <- function(ticker, startDate, endDate) {
  startDate <- strsplit(as.character(startDate),"-")[[1]]
  endDate <- strsplit(as.character(endDate),"-")[[1]]
  urlString <- sprintf("http://ichart.finance.yahoo.com/table.csv?s=%s&d=%s&e=%s&f=%s&g=d&a=%s&b=%s&c=%s&ignore=.csv",
                       ticker,as.integer(endDate[2])-1,endDate[3],endDate[1],as.integer(startDate[2])-1,startDate[3],startDate[1])
  data <- read.csv(url(urlString),header=T, stringsAsFactors=F)
  data[,1] <- as.Date(data[,1])
  data <- data[order(data[,1]),]
  return(data)
}


returns <- function(dataInitial, dataFinal){
  difference <- dataFinal - dataInitial 
  return(difference)
}



  
moving <- function(X, Y=NULL,
                     fun=mean,
                     period=min(length(X), 20),
                     simplify=TRUE,
                     ...)
  {
    n <- length(X)
    
    from  <-  1:(n - period)
    to    <-  period:(n-1)
    
    run.elements  <- apply(cbind(from, to), 1, function(x) seq(x[1], x[2]) )
    
    if(is.matrix(run.elements))
      run.elements <- as.data.frame(run.elements) # ensure its a list!
    
    names(run.elements) <- paste(from,to,sep=':')
    
    if(is.null(Y))  # univariate 
    {
      funct <- function(which,what,fun,...) fun(what[which],...)
      
      if(simplify)
        Xvar <- sapply(run.elements, funct, what=X, fun=fun, ...)
      else
        Xvar <- lapply(run.elements, funct, what=X, fun=fun, ...)        
    }
    else # bivariate
    {
      funct <- function(which,XX,YY,fun,...) fun(XX[which],YY[which], ...)
      
      if(simplify)
        Xvar <- sapply(run.elements, funct, XX=X, YY=Y, fun=fun, ...)
      else
        Xvar <- lapply(run.elements, funct, XX=X, YY=Y, fun=fun, ...)
    }
    
    if(is.null(dim(Xvar)))
    {
      outVec <- rep(NA,n)
      names(outVec) <- c(paste(1,1:period,sep=":"),names(Xvar))
      outVec[(period+1):n] <- Xvar
      Xvar <- outVec
    }
    else if(is.list(Xvar))
    {
      outList <- rep(NA,n)
      names(outList) <- c(paste(1,1:period,sep=":"),names(Xvar))
      outList[(period+1):n] <- Xvar
      Xvar <- outVec		
    }
    else
    {
      if(nrow(Xvar) != n-period) Xvar <- t(Xvar)
      outMat <- matrix(NA,nrow=n,ncol=ncol(Xvar))
      dimnames(outMat)<- list(c(paste(1,1:period,sep=":"),dimnames(Xvar)[[1]]),dimnames(Xvar)[[2]])
      outMat[(period+1):n,] <- Xvar
      Xvar <- outMat		
    }
    
    return(Xvar)
  }


