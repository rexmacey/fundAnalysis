library(xts, quietly = TRUE)
library(readr, quietly = TRUE)
library(quantmod, quietly=TRUE)
library(MASS, quietly = TRUE)
library(FactorAnalytics, quietly = TRUE)
library(rvest, quietly = TRUE)
library(RQuantLib, quietly = TRUE)

#' Converts prices to returns    
#'
#' @param prices xts of prices such as returned by getPrices function
#' @param freq M for monthly, or D for daily
#'
#' @return If ncol(prices)==1, an xts object, else a list with xts object of returns
#' @export
#'
#' @examples
#' convertPricesToReturns(p)

convertPricesToReturns <- function(prices, freq="D"){
    if(toupper(freq)=="D") {
        rets <- lapply(1:ncol(prices),function(x) periodReturn(prices[,x],period="daily", leading=TRUE))
        
    } else {
        rets <- lapply(1:ncol(prices),function(x) periodReturn(prices[,x],period="monthly", leading=TRUE))
        rets <- lapply(rets, function(x) makeCompleteMonths(x))
    }
    rets <- lapply(rets,function(x) x[-1]) # remove first row which is 0
    names(rets) <- colnames(prices)
    for(i in 1:length(rets)){
        colnames(rets[[i]]) <- names(rets)[i]
    }
    if(length(rets)==1) rets <- rets[[1]]
    return(rets)
}

#' Filter (subset) an xts object
#' Subsets an xts object from start (s) to end (e).  If either is omitted, the earliest or latest observation is used
#' If n is not null it will return n observations from the last date (e).
#'
#' @param xtsData xts object to be filtered (subsetted) 
#' @param s Start date
#' @param e End date
#' @param n Number of observations
#'
#' @return xts object
#' @export
#'
#' @examples
#' dateFilter(xtsOjbect,s="2012-12-31",e="2017-12-31")
#' dateFilter(xtsOjbect,e="2017-12-31", n=60)

dateFilter <- function(xtsData,s=NULL,e=NULL,n=NULL){
    if(is.null(s)) s<- start(xtsData[1])
    if(is.null(e)) e<- end(xtsData)
    data <- xtsData[paste0(s,"/",e)]
    if(!is.null(n)){
        data<-last(data,n)
    }
    return(data)
}

#' Define a benchmark    
#' A benchmark is used to compare the performance of a fund against. A benchmark may be a single symbol with a weight of 1,
#' or a blend of multiple symbols (a vector) 
#'
#' @param shortName A short name used much the way a symbol (ticker) might be used as a column head.
#' @param description A longer description of the benchmark (e.g., 50% S&P 500 / 50% Barclay's Agg)
#' @param symbol A symbol (ticker) or character vector of symbols 
#' @param weights Weights of the symbol(s). Length must match the symbol
#' @param startDate Start date to get the prices for the benchmarks
#' @param freq Frequency of returns for the benchmark ("M" or "D")
#'
#' @return List with information about the benchmark
#' @export
#'
#' @examples
#' defineBenchmark("SP500","S&P 500 ETF", "SPY", 1)
#' defineBenchmark("Bench","50% S&P 500 / 50% Barclay's Agg", c("SPY","AGG"), c(0.5,0.5))

defineBenchmark <- function(shortName="Bench", description="Benchmark", symbol=NULL, weights=1,
                            startDate="1970-01-01", freq="M"){
    if(length(symbol)!=length(weights)) stop("Error in defineBenchmark: length of symbol is not equal to length of weights.")
    out <- list()
    out$shortName <- shortName
    out$description <- description
    out$symbol <- symbol
    out$weights <- weights/sum(weights)
    p <- getPrices(symbol, startDate, "D")
    r.daily <- convertPricesToReturns(p, "D")
    r.monthly <- convertPricesToReturns(p, "M")
    r.daily <- faCommonDate(r.daily, "D")
    r.monthly <- faCommonDate(r.monthly, "M")
    
    r.matrix.daily <- matrix(unlist(r.daily),ncol=length(r.daily))
    r.xts.daily <- xts(r.matrix.daily,order.by = index(r.daily[[1]]))
    colnames(r.xts.daily) <- names(r.daily)
    if(length(symbol)==1){
        out$returns.daily <- r.xts.daily
    } else {
        b.ret <- t(apply(r.xts.daily,1,function(x) x*weights))
        out$returns.daily <- xts(apply(b.ret,1,sum), order.by = index(r.xts.daily))
        colnames(out$returns.daily)<-shortName
    }
    r.matrix.monthly <- matrix(unlist(r.monthly),ncol=length(r.monthly))
    r.xts.monthly <- xts(r.matrix.monthly,order.by = index(r.monthly[[1]]))
    colnames(r.xts.monthly) <- names(r.monthly)
    if(length(symbol)==1){
        out$returns.monthly <- r.xts.monthly
    } else {
        b.ret <- t(apply(r.xts.monthly,1,function(x) x*weights))
        out$returns.monthly <- xts(apply(b.ret,1,sum), order.by = index(r.xts.monthly))
        colnames(out$returns.monthly)<-shortName
    }
    return(out)
}

#' Download Fama-French 5 factor monthly     
#' source: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip
#' 
#' @return xts object with monthly data for Fama-French model
#'
#' @examples
#' download_FF_5_factor_monthly()

download_FF_5_factor_monthly <- function(){
    ff_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
    temp <- tempfile()
    download.file(ff_url,temp)
    ff_data <- read.csv(unz(temp,"F-F_Research_Data_5_Factors_2x3.CSV"), skip=3, check.names=TRUE)
    unlink(temp)
    idx <- which(ff_data[,1]== " Annual Factors: January-December ")
    ff_data<-ff_data[1:(idx-1),]
    ff_data <- data.frame(apply(ff_data,2,as.numeric))
    ff_data<-as.xts(ff_data, order.by=as.yearmon(as.character(ff_data$X),"%Y%m"))
    return(ff_data[,-1])
}

#' Download Fama-French 5 factor daily data    
#' source: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip
#' 
#' @return xts object with daily data for Fama-French model
#'
#' @examples
#' download_FF_5_factor_daily()

download_FF_5_factor_daily <- function(){
    ff_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip"
    temp <- tempfile()
    download.file(ff_url,temp)
    ff_data <- read.csv(unz(temp,"F-F_Research_Data_5_Factors_2x3_daily.CSV"), skip=3, check.names=TRUE)
    unlink(temp)
    ff_data <- xts(ff_data, order.by=as.Date(as.character(ff_data$X),"%Y%m%d"))
    ff_data <- ff_data[,-1]
    return(ff_data)
}

#' Download Fama-French 5 factor model data from Ken French website    
#'
#' @param freq Either "M" for monthly data or "D" for daily data
#'
#' @return xts object with the daily or monthly data
#' @export
#'
#' @examples
#' download_FF_5_factor("M")
#' download_FF_5_factor("D")

download_FF_5_factor <- function(freq="M"){
    if (toupper(substr(freq,1,1))=="M") {
        out <- download_FF_5_factor_monthly()
    } else {
        out <- download_FF_5_factor_daily()
    }
    return(out)
}


#' Generates individual lm model for a fund using Fama-French data    
#' Not public.  
#'
#' @param y Returns of fund
#' @param ff_data Fama-French data
#' @param s Start date
#' @param e End date
#' @param n Number of periods
#'
#' @return lm model
#'
#' @examples
#' ffModelLM_sub(y,ff_data)

ffModelLM_sub <- function(y,ff_data,s=NULL,e=NULL,n=NULL){
    data <- ffMergeXTS(y,ff_data,s,e,n)
    mdl <- lm(y~Mkt.RF+SMB+HML+RMW+CMA+RF,data=data)
    return(mdl)
}

#' Generates lm models for one or more funds using Fama-French data as independent variables    
#'
#' @param rets List of returns such as generated by convertPricesToReturns()
#' @param ff_data Fama-French data in same frequency as Y
#' @param s Start date
#' @param e End date
#' @param n Number of observations
#'
#' @return List of lm models
#' @export
#'
#' @examples
#' ffModelLM(rets, ff_data)

ffModelLM <- function(rets,ff_data,s=NULL,e=NULL,n=NULL){
    out <- lapply(rets, function(x) ffModelLM_sub(y=x,ff_data=ff_data,s=s,e=e,n=n))
    names(out) <- names(rets)
    return(out)
}

#' Generates an lm model using stepwise regression
#'
#' @param y Fund returns
#' @param ff_data Fama-French data
#' @param s Start date
#' @param e End date
#' @param n Number of observations
#'
#' @return lm model
#'
#' @examples
#' ffModelStepLM_sub(y,ff_data)

ffModelStepLM_sub <- function(y,ff_data, s=NULL, e=NULL, n=NULL){
    data <- ffMergeXTS(y,ff_data,s,e,n)
    mdl <- lm(y~Mkt.RF+SMB+HML+RMW+CMA+RF,data=data)
    return(stepAIC(mdl, direction="both",trace=0))
}

#' Generates list of lm models using step-wise regression
#'
#' @param y Fund returns
#' @param ff_data Fama-French data
#' @param s Start date
#' @param e End date
#' @param n Number of periods
#'
#' @return List of lm models
#' @export
#'
#' @examples
#' ffModelStepLM(y,ff_data)

ffModelStepLM <- function(rets,ff_data, s=NULL, e=NULL, n=NULL){
    out <- lapply(rets, function(x) ffModelStepLM_sub(y=x,ff_data=ff_data,s=s,e=e,n=n))
    names(out) <- names(rets)
    return(out)
}

#' Find common period between xts objects    
#' Given a list of xts objects, it returns a list of those objects with the longest common period (starting at most recent start,
#' and ending at earliest end)
#'
#' @param rets List of xts objects
#' @param freq "M" for monthly, "D" for daily
#'
#' @return List of xts objects with common dates
#' @export
#'
#' @examples
#' faCommonDate(rets)

faCommonDate <- function(rets, freq="M"){
    s<-max(sapply(rets,start))
    e<-min(sapply(rets,end))
    out<-lapply(rets, function(x) x[paste0(as.Date(s, origin="1970-01-01"),"/",as.Date(e, origin="1970-01-01"))])
    return(out)
}


#' Merge return series with Fama-French data
#' Merges a xts object of returns with the ff data xts object.
#'
#' @param y Fund return
#' @param ff_data Fama-French data
#' @param s Start date
#' @param e End date
#' @param n Number of observations
#'
#' @return xts object
#' @export
#'
#' @examples
#' ffMergeXTS(y,ff_data)

ffMergeXTS <- function(y,ff_data,s=NULL,e=NULL,n=NULL){
    data <- merge.xts(ff_data,y,join = "inner")
    data <-dateFilter(data,s,e,n)
    colnames(data)[ncol(data)] <- "y"
    return(data[complete.cases(data),])
}


#' Align date indices of two xts objects
#'
#' @param xts1 First xts object
#' @param xts2 Second xts object
#' @param s Start date
#' @param e End date
#' @param n Number of periods
#'
#' @return List with two xts objects with the same indices
#' @export
#'
#' @examples
#' faAlignXTS(xts1, xts2)
faAlignXTS <- function(xts1,
                       xts2,
                       s = NULL,
                       e = NULL,
                       n = NULL) {
    ncol1 <- ncol(xts1)
    ncol2 <- ncol(xts2)
    out <- merge.xts(xts1, xts2, join = "inner")
    out <- dateFilter(out, s, e, n)
    out <- out[complete.cases(out), ]
    out <- list(out[, 1:ncol1], out[, (ncol1 + 1):(ncol1 + ncol2)])
    return(out)
}

#' Coefficients from lm models
#'
#' @param lst List of lm models
#'
#' @return Table with coefficients for each model
#' @export
#'
#' @examples
#' coefficients.lm(lst)

coefficients_lm <- function(lst){
    temp <- t(sapply(lst,coefficients))
    row.names(temp)<-names(lst)
    return(temp)
}

#' Coefficients from stepwise models
#'
#' @param lst List of stepwise models
#'
#' @return Table with coefficients for each model
#' @export
#'
#' @examples
#' coefficients.step(lst)

coefficients_step <- function(lst){
    # create a matrix to hold all results
    mdl_coef <- matrix(0,nrow = length(lst), ncol=7)
    row.names(mdl_coef)<-names(lst)
    colnames(mdl_coef)<-c("(Intercept)","Mkt.RF","SMB","HML","RMW","CMA","RF")
    for(i in 1:length(lst)){
        mdl_coef[i,names(coefficients(lst[[i]]))]<-coefficients(lst[[i]])    
    }
    return(mdl_coef)
}

#' Returns based style analysis    
#'
#' @param r.fund Fund returns (xts)
#' @param r.style Style returns (xts)
#' @param s Start date
#' @param e End date
#' @param n Number of observations
#' @param method Method from Factor Analyticss package's style.fit function
#' @param leverage Leverage from Factor Analyticss package's style.fit function
#' @param selection Selection from Factor Analytics package's style.fit function
#'
#' @return List of 3: weights, R.squared, and adj.R.squared
#' @export
#'
#' @examples
#' RBSA(r.fund, r.style)

RBSA <- function(r.fund, r.style, s=NULL, e=NULL, n=NULL, method="constrained", leverage=FALSE, selection="AIC"){
    data <- faAlignXTS(r.fund, r.style, s, e, n)
    out <- style.fit(data[[1]], data[[2]], method=method, leverage=leverage, selection=selection)
    return(out)
}

#' Returns based style analysis    
#'
#' @param r.fund fund returns (xts)
#' @param r.style style returns (xts)
#' @param s start date
#' @param e end date
#' @param n number of observations
#' @param method method from Factor Analyticss package's style.fit function
#' @param leverage leverage from Factor Analyticss package's style.fit function
#' @param selection selection from Factor Analytics package's style.fit function
#' @param scale number of periods in a year
#'
#' @return List of 3: weights, R.squared, and adj.R.squared
#' @export
#'
#' @examples
#' RBSA(r.fund, r.style) 
#' 
rbsa <- function(r.fund, r.style, s=NULL, e=NULL, n=NULL, method="constrained", leverage=FALSE, selection="AIC", scale=12){
    data <- faAlignXTS(r.fund, r.style, s, e, n)
    y <- data[[1]]
    x <- data[[2]]
    out <- rbsa_calc(y,x, method, leverage, selection, scale)
    return(out)
}

rbsa_calc <- function(y,x, method="constrained", leverage=FALSE, selection="AIC", scale=12){
    out <- list()
    fit <- style.fit(y,x, method=method, leverage=leverage, selection=selection)
    out$weights <- unlist(fit$weights)
    names(out$weights) <- colnames(x)
    yhat <- Return.portfolio(x,out$weights,geometric = FALSE)
    out$regStats <- regressStats(yhat, y, scale)
    if(length(y)<=scale){
        out$fundReturn <- prod(1+y) - 1
        out$benchReturn <- prod(1+yhat) -1
    } else {
        out$fundReturn <- prod(1+y)^(scale/length(y)) - 1
        out$benchReturn  <- prod(1+yhat)^(scale/length(yhat)) -1
    }
    out$excessReturn <- out$fundReturn - out$benchReturn
    return(out)
}

regressStats <- function(pred,y, scale=12){
    err <- pred - y
    rsquared <- cor(pred,y)^2
    te <- sd(err)*sqrt(scale)
    mae <- mean(abs(err))
    rmse <- sqrt(mean(err^2))
    return(c(RSquared=rsquared, TE=te,MAE=mae,RMSE=rmse))
}

#' RBSA over a rolling window
#'
#' @param r.fund Fund returns (xts)
#' @param r.style  Style returns (xts)
#' @param s Start date
#' @param e End date
#' @param n Number of Observations
#' @param method Method from Factor Analyticss package's style.fit function
#' @param leverage Leverage from Factor Analyticss package's style.fit function
#' @param width Number of observations in a window
#'
#' @return xts object with one row per moving window containing the weights
#' @export
#'
#' @examples
#' RBSA_rolling(r.fund, r.style)

#RBSA_rolling <- function(r.fund, r.style, s=NULL, e=NULL, n=NULL, method="constrained", leverage=FALSE, width=30, selection="AIC"){
#    data <- faAlignXTS(r.fund, r.style, s, e, n)
#    out <- table.RollingStyle(data[[1]],data[[2]], method=method, leverage=leverage,width = width, selection=selection)
#    return(out)
#}

#' Returns-based style analysis (RBSA) over a rolling window
#'
#' @param r.fund Fund returns (xts)
#' @param r.style  Style returns (xts)
#' @param s Start date
#' @param e End date
#' @param n Number of Observations
#' @param method Method from Factor Analyticss package's style.fit function
#' @param leverage Leverage from Factor Analyticss package's style.fit function
#' @param width Number of observations in a window
#' @param selection Selection from Factor Analytics package's style.fit function
#'
#' @return List containing: weights - xts object with one row per moving window containing the weights; 
#' meanSDofWeights - mean of the standard deviation of the columns of the weights.  Lower values represent 
#' more consistency of the weightings of the styles; regressStats - xts of the regression stats for each window 
#' including the rsquared (R2), tracking error (TE), mean absolute error (MAE), and root mean square error (RMSE);
#' fundReturn are the returns of the fund over each window; benchReturn are the returns of a benchmark defined by the 
#' style weight of the window (returns are annualized for periods exceeding one year); excessReturn is the fund 
#' return less the benchmark return.
#
#' @export
#'
#' @examples
#' RBSA_rolling(r.fund, r.style)
#' 
rbsa_rolling <- function(r.fund, r.style, s=NULL, e=NULL, n=NULL, method="constrained", leverage=FALSE, width=30, selection="AIC", scale=12){
    data <- faAlignXTS(r.fund, r.style, s, e, n)
    out<-list()
    i <- seq(width,nrow(data[[1]])) # ending index positions
    idx <- index(data[[1]][i]) # index values used for xts conversion
    temp<-lapply(i, function(x) rbsa_calc(data[[1]][(x-width+1):x,], 
                                          data[[2]][(x-width+1):x,],
                                          method, leverage, selection))
    out$weights <- as.xts(t(sapply(1:length(i), function(x) temp[[x]]$weights)),order.by = idx)
    out$meanSDofWeights <- mean(apply(out$weights,2,sd))
    out$regressStats <- as.xts(t(sapply(1:length(i), function(x) temp[[x]]$regStats)), order.by = idx)
    out$fundReturn <- as.xts(sapply(1:length(i), function(x) temp[[x]]$fundReturn),order.by = idx)
    out$benchReturn <- as.xts(sapply(1:length(i), function(x) temp[[x]]$benchReturn),order.by = idx)
    out$excessReturn <- as.xts(sapply(1:length(i), function(x) temp[[x]]$excessReturn),order.by = idx)
    return(out)
}

rbsa_bootstrap <- function(r.fund, r.style, n=120L, method="constrained", leverage=FALSE, width=30, selection="AIC", scale=12){
    data <- faAlignXTS(r.fund, r.style)
    nperiods <- nrow(data[[1]])
    if(width > nperiods) stop("Width greater than number of periods in the data")
    z <- t(replicate(n,sample(seq(1,nperiods),width, replace = FALSE)))
    temp<-lapply(1:n, function(x) {rbsa_calc(data[[1]][z[x,]], 
                                             data[[2]][z[x,]],
                                             method, leverage, selection)})
    out<-list()
    out$weights <- t(sapply(1:n, function(x) temp[[x]]$weights))
    colnames(out$weights) <- colnames(data[[2]])
    out$meanSDofWeights <- mean(apply(out$weights,2,sd))
    out$regressStats <- t(sapply(1:n, function(x) temp[[x]]$regStats))
    out$fundReturn <- sapply(1:n, function(x) temp[[x]]$fundReturn)
    out$benchReturn <- sapply(1:n, function(x) temp[[x]]$benchReturn)
    out$excessReturn <- sapply(1:n, function(x) temp[[x]]$excessReturn)
    return(out)
}
    

#' Scrape quote summary from Yahoo Finance
#'
#' @param symbol Ticker of fund
#'
#' @return List with data from quote summary
#' @export
#'
#' @examples
#' scrapeQuoteSummary("SPY")

scrapeQuoteSummary <- function(symbol){
    url <- paste0("https://finance.yahoo.com/quote/",symbol,"?p=",symbol)
    webpage <- read_html(url)
    result <- html_nodes(webpage, "#quote-summary")
    result <- html_nodes(result, "table") %>% html_table()
    fundName <- html_nodes(webpage,"h1") %>% html_text()
    startPos <- regexpr(" - ",fundName)
    fundName <- substr(fundName,startPos+3,nchar(fundName))
    out <-  c(symbol,fundName, result[[1]]$X2,result[[2]]$X2)
    names(out)<-c("Symbol","Fund Name",result[[1]]$X1,result[[2]]$X1)
    return(out)
}


#' Get prices for one or more symbols    
#'
#' @param symbols Tickers of the mutual funds and ETFs
#' @param startDate default is 1970-01-01
#' @param freq M for monthly or D for daily
#' @param endDate default is today's date (Sys.Date)
#'
#' @return xts object with prices. Index will be yearmon for monthly data
#' @export
#'
#' @examples
#' getPrices("IVV")
#' getPrices("IVV", startDate="2015-12-31", freq="D")

getPrices <- function(symbols, startDate="1970-01-01", freq="M", endDate=Sys.Date()){
    Sys.setenv(TZ="UTC")
    getSymbols(symbols,warnings=FALSE, from=startDate, to=endDate)
    data<-xts(frequency = "Date")
    for (symbol in symbols){
        #cat(symbol," ... ")
        data<-merge(data,Ad(get(symbol)))
    }
    index(data) <- as.Date(index(data))
    data <- na.omit(data)
    #colnames(data) <- sapply(colnames(data),function(x) substr(x,1,regexpr("\\.",x)-1))
    colnames(data) <- symbols
    if(toupper(freq) != "D"){
        mon_idx <- endpoints(data, on="months")
        data <- data[mon_idx,]
        index(data)<-as.yearmon(index(data),"%Y-%m-%d")
    }
    return(data)
}

#' Get prices and returns for a set of symbols
#'
#' @param symbols List of symbols
#' @param startDate Start date, default is 1970-01-01
#' @param endDate End date, default is today's date Sys.Date()
#'
#' @return List of 3 items. Prices is an xts object with one column per symbol. returns.daily 
#' is a list with one item per symbol containing daily returns.  returns.monthly contains the monthly returns.
#' @export
#'
#' @examples
#' getPricesAndReturns(c("FNDB","IVV","SPY"))

getPricesAndReturns <- function(symbols, startDate="1970-01-01", endDate=Sys.Date()){
    out<-list()
    out$prices <- getPrices(symbols, startDate, freq = "D", endDate)
    out$returns.daily <- convertPricesToReturns(out$prices, freq="D")
    out$returns.monthly <- convertPricesToReturns(out$prices, freq="M")
    return(out)
}

#' Makes sure the last month in an xts object is a full trading month    
#' This is used when converting to monthly returns. If the last price was not at the end 
#' of a month (e.g. 2/13/2018) a return will still be produced for that month (Feb 2018). 
#' We may not want to use a partial month for some calculations such as when calculating
#' the return for the last x months, so we eliminate those types of months.  
#'
#' @param x xts object
#'
#' @return xts object
#'
#' @examples
#' makeCompleteMonths(x)

makeCompleteMonths <- function(x){
    if(! isEndOfMonth("UnitedStates/NYSE",end(x))) x<-x[1:(length(x)-1)] # from RQuantLib package
    return(x)
}


#' Get price and returns for 13 Week Treasury Bills    
#'
#' @return List with 4 xts objects: yield, prices, returns.daily, and returns.monthly
#' @export
#'
#' @examples
#' getRiskFree()
getRiskFree <- function(){
    out <- list()
    out$yield <- getSymbols("^IRX",freq="M") # retrieve 13 Week TBill yields annualized
    out$yield <- Ad(IRX) # keep adjusted close
    out$yield <- na.omit(out$yield)/100 # remove NAs and convert to decimal 
    colnames(out$yield) <- "TBILL"
    rf.yld <- (1+out$yield)^(1/252) # convert to daily yield +1
    out$prices <- cumprod(rf.yld)
    out$returns.daily<-convertPricesToReturns(out$prices,"D")
    out$returns.monthly<-convertPricesToReturns(out$prices,"M")
    return(out)
}

