library(quantmod)
library(xts)
library(tseries)
library(forecast) #auto.arima

closing_data = read.csv("large_cap_close.csv",header=F)
tics = read.csv("large_cap_tics_final.csv",header=F,strip.white=TRUE)
colnames(closing_data)<-tics$V1
size = dim(closing_data)
closing_data = closing_data[size[1]:1,]
## -----------------doing weekly instead of daily --------------##
index = seq(1,size[1],10)
closing_data = closing_data[index,]
size = dim(closing_data)

closing_prices = closing_data
dates = read.csv("large_cap_dates.csv",header=F)
dates = data.frame(dates[dim(dates):1,])
dates = data.frame(dates[index,])
#write.csv(dates, file="dates2.csv")
### we don't have any concern for dates right now

###########VERY IMPORTANT COMMENT###############
#Earlier when I just did a read of csv the date column was also included in the data
#hence all the code below was getting screwed up and the is.numeric was never coming out to be true
#then I just took the numeric values from 2nd column on and it works fine

##look for companies that had a stock split or something
temp = zoo(closing_prices)
closing_lagged = lag(temp,1)        #Instead of lag I could have also simply chosen my indices
temp = data.frame(closing_lagged)
temp2 = temp/closing_prices[1:(size[1]-1),]    #temp2 is the return vector
split = apply(temp2,2,function(x) sum(any(x>=1.8|x<=0.5)))
closing_prices = closing_prices[,which(split==0)]
returns = temp2[,which(split==0)]

### Computing time series and returns by fitting to a time series ##########

### check for stationarity
pval = rep(0,length(returns))
for (i in 1:length(returns))
{p = adf.test(returns[,i])
 pval[i] = p$p.value}

###### Setting the values of parameters
no_of_hist = 10
horizon = 2
current = 11
invest_prct = 0.9
dates = data.frame(dates[current+seq(0,time-current,horizon),])

### create a portfolio based on ARMA model prediction.
pred_ret = NULL
port = NULL
bench = NULL
alloc=NULL
port[1] = 1
bench[1] = 1
n = length(returns)
time = dim(returns)[1]
j=2

while(current+horizon<time)
{
  pred_ret = NULL
  for (i in 1:n)
  {
   factors = auto.arima(returns[(current-no_of_hist):(current-1),i],max.p=5,max.q=5,ic="bic",allowdrift=FALSE)
   forecast = predict(factors,horizon)
   pred_ret[i] = prod((forecast$pred+mean(returns[(current-no_of_hist):(current-1),i]))/forecast$se/100)
  }
   p = data.frame(pred_ret)
   p2 = p[1:(n-1),]
   ret = data.frame(p2)
   limit = quantile(ret$p2,prob=invest_prct)
   invest = returns[current:(current+horizon-1),1:(n-1)]
   invest = data.frame(apply(invest,2,prod))
   invest = invest[which(ret$p2>limit),]  
   bench[j] = prod(returns[current:(current+horizon-1),n])
   port[j] = mean(invest)
   alloc[j-1]=data.frame(ret$p2>limit)
  
   current = current+horizon
   j = j+1
}
alloc = t(data.frame(alloc))
colnames(alloc)<-colnames(returns)[1:(n-1)]
rownames(alloc)<-1:dim(alloc)[1]

prod(port[!is.na(port)])/sd(port[!is.na(port)])
prod(bench[1:j-1])/sd(bench)

dates = dates[current+seq(0,time,horizon)]

#### Comparing with investing in the high return stocks of today
horizon = 2
no_of_hist=10
current = 11
invest_prct = 0.9
naive_port=NULL
prev_ret=NULL
naive_port[1]=1
i=2

while((current+horizon)<time)
{
  prev_ret = apply(returns[(current-no_of_hist):(current-1),],2,prod)
  prev_ret = data.frame(prev_ret[1:(n-1)])
  limit = quantile(prev_ret$prev_ret,prob=invest_prct)
  invest = returns[current:(current+horizon-1),1:(n-1)]
  invest = data.frame(apply(invest,2,prod))
  invest = invest[which(prev_ret$prev_ret>limit),]  
  naive_port[i]=mean(invest)
  
  current = current+horizon
  i=i+1
}

prod(naive_port[!is.na(naive_port)])/sd(naive_port[!is.na(naive_port)])

#### Based on allocation how many winners were predicted?
horizon = 2
current = 11
threshold = 0.75
winners=NULL
prev_ret=NULL
i=1

while((current+horizon)<time)
{
  next_ret = data.frame(ret=apply(returns[current:(current+horizon-1),1:n-1],2,prod))
  next_ret = next_ret>1     #quantile(next_ret$ret,threshold)
  winners[i] = sum(alloc[i,]*next_ret)
  
  i = i+1
  current = current+horizon
}


### comparing against investing in high momentum stocks
current = 601
horizon = 5
no_of_hist = 200
no_of_mom = 15
invst_prct = 0.8
long_momntm = 0.99^(0:(no_of_hist-1))
short_momntm = 0.99^(0:(no_of_mom-1))

pred = NULL
port_mom = NULL
bench_mom = NULL
alloc=NULL
stdev_port = NULL
port_mom[1]=1
bench_mom[1]=1
n = length(returns)
time = dim(returns)[1]
long_momntm = matrix(long_momntm,n-1,nrow=no_of_hist)
short_momntm = matrix(short_momntm,n-1,nrow=no_of_mom)
i=2
prev_ind = rep(1,n-1)

while(current+horizon<time)
{
  MA_long = closing_prices[(current-no_of_hist):(current-1),1:(n-1)]*long_momntm
  MA_short = closing_prices[(current-no_of_mom):(current-1),1:(n-1)]*short_momntm
  MA_long = apply(MA_long,2,sum)/apply(long_momntm,2,sum)
  MA_short = apply(MA_short,2,sum)/apply(short_momntm,2,sum)
  #past_ret = data.frame(apply(returns[(current-no_of_mom):(current-1),1:(n-1)],2,prod))
  #colnames(past_ret) = 'past'
  #limit = quantile(past_ret$past,invst_prct)
  ind = prev_ind*(MA_short>MA_long)
  ret = as.data.frame(returns[current:(current+horizon-1),which(ind==1)])
  ret = apply(ret,2,prod)
  ret = data.frame(ret)
  bench_mom[i] = prod(returns$SNP[current:current+horizon])
  port_mom[i] = mean(ret)

#### to see how frequently is it switched
  temp1 = MA_short>MA_long
  temp2 = 1
  alloc[i-1]=data.frame(temp1*temp2)
  
###to look at another measure of risk
  stdev = apply(returns[current:(current+horizon-1),],2,sd)
  stdev = stdev[1:(n-1)]*data.frame(alloc[(i-1)])
  stdev_port[i-1] = sqrt(sum(stdev^2))/sum(data.frame(alloc[(i-1)]))
  
  current = current+horizon
  i=i+1
  prev_ind=1-ind
}

alloc = t(data.frame(alloc))
colnames(alloc)<-colnames(returns)[1:(n-1)]
rownames(alloc)<-1:dim(alloc)[1]

#apply(alloc,2,sum)

prod(bench_mom)/sd(bench_mom)
prod(port_mom[!is.na(port_mom)])/sd(port_mom[!is.na(port_mom)])

mean(port_mom[2:(i-1)]/stdev_port)
