library(Quandl)
library(ggplot2)
library(PerformanceAnalytics)

time_update <- function(x, C , A , P , Q ) {
  x = A%*%x + C
  P = A%*%P%*%t(A) + Q
  return (list(state = x, mat = P))
  
}
measurement_update <- function(x , z , H , P , R) {
  K = P%*%t(H)%*%solve(H%*%P%*%t(H) + R)
  x = x + K%*%(z - H%*%x)
  P = (diag(nrow(P)) - K%*%H)%*%P
  return(list(state = x , mat = P))
}




run_kf <- function ( x , P ,  A , Q ,C ,  H , R , price) {
  pred = c()
  for ( i in 1:length(price)) {
    v = time_update(x , C[i] , A , P , Q)
    x = v$state
    P = v$mat
    pred = c(pred , H%*%x)
    v = measurement_update(x , price[i] , H , P , R )
    x = v$state
    P = v$mat
    
  }
  return(pred)
}

sse <- function(x , P ,  A , Q ,C ,  H , R , price) { 
  pred = run_kf(x , P ,  A , Q ,C ,  H , R , price)
  delt = pred - price
  se = sum(delt*delt)
  return(se)
}
helper <- function(list , d , func) {
  ret_list = c()
  for ( i in 1:length(list)) {
    start_ix = max(1,i - d)
    ret_list = c(ret_list , func(list[start_ix:i]))
  }
  return(ret_list)
} 
calculate_k <- function(df , d) {
  curr_close = array(df$Last)
  lowst_low = array(helper(df$Low , d , min))
  highst_high = array(helper(df$High , d , max))
  K = (curr_close - lowst_low)*100/(highst_high - lowst_low)
  return(K)
}

opti_wrapper <- function(dataset , par) {
  a11 = par[1]
  a12 = par[2]
  a22 = par[3]
  h11 = par[4]
  h12 = par[5]
  r11 = par[6]
  q11 = par[7]
  q12 = par[8]
  q21 = par[9]
  q22 = par[10]
  p11 = par[11]
  p22 = par[12]
  c11 = par[13]
  c12 = par[14]
  c21 = par[15]
  c22 = par[16]  
  data = dataset$data
  K = dataset$K
  A = matrix(c(a11,a12,0,a22) , nrow = 2 , ncol = 2)
  H = matrix(c(h11 ,h12 ) , nrow = 1 , ncol = 2)
  R = matrix(c(r11) , nrow = 1)
  Q = matrix(c(q11,q12,q21,q22) , nrow = 2)
  P  = matrix(c(p11,0 , 0 , p22) , nrow = 2)
  U1 = c11 - c12*K
  U2 = c21 - c22*K
  C = vector(length = length(data))
  for ( i in 1:length(K)) {
    C[i] = matrix(c(U1[i] , U2[i]) , nrow = 2)
  }
  x0 = matrix(c(1,1) , nrow = 2)
  e = sse(x0 , P , A , Q , C , H , R, data)
  return (e)
}

runner_wrapper <- function(dataset, par) {
  a11 = par[1]
  a12 = par[2]
  a22 = par[3]
  h11 = par[4]
  h12 = par[5]
  r11 = par[6]
  q11 = par[7]
  q12 = par[8]
  q21 = par[9]
  q22 = par[10]
  p11 = par[11]
  p22 = par[12]
  c11 = par[13]
  c12 = par[14]
  c21 = par[15]
  c22 = par[16]  
  data = dataset$data
  K = dataset$K
  A = matrix(c(a11,a12,0,a22) , nrow = 2 , ncol = 2)
  H = matrix(c(h11 ,h12 ) , nrow = 1 , ncol = 2)
  R = matrix(c(r11) , nrow = 1)
  Q = matrix(c(q11,q12,q21,q22) , nrow = 2)
  P  = matrix(c(p11,0 , 0 , p22) , nrow = 2)
  U1 = c11 - c12*K
  U2 = c21 - c22*K
  C = vector(length = length(data))
  for ( i in 1:length(K)) {
    C[i] = matrix(c(U1[i] , U2[i]) , nrow = 2)
  }
  x0 = matrix(c(1,1) , nrow = 2)
  pred = run_kf(x0 , P ,  A , Q ,C ,  H , R , data)
  print(pred)
  return(pred)
}



sp500<-Quandl("CHRIS/CME_ES1",type="xts")
df = get("sp500")
starting_date="2015-01-01"
df_subset<-df["2015-01-01/"]
price = array(df$Last) 
K = calculate_k(df , 15)
# optim(par = c( 1 ,1 , 1, 1 , 1 , 1 , 1 ,1  , 1 ,1 ,1 , 1 ,1 , 1 , 1 , 1 ) ,opti_wrapper , data = list(data = price , K = K) )

# Params for entire data
pred = runner_wrapper( list(data = price , K = K) , c(0.99096666,0.69073784,0.78268025,1.04369883,1.33940717,0.83915650,1.25269400,0.91041264,1.13400494,0.90719438,1.23719213
,1.21665465,1.20411538,-0.03344232,1.23766700,1.14800654))

# Params for last one year data
# pred = runner_wrapper( list(data = price , K = K) , c(0.60568353,1.14895134,0.99616054,0.95086742,0.56622011,1.14341085,0.95898062,0.96325286,1.22158019,
# 1.20046515,1.25998248,1.13982826,1.48270772,-0.02545405,0.78480878,1.26122980))

#
df_price<-data.frame(value=price,date=index(df))
df_pred<-data.frame(value=pred,date=index(df))

p <- ggplot() +
  geom_line(data = df_price, aes(x = date, y = value, color = "Price")) +
  geom_line(data = df_pred, aes(x = date, y = value, color = "Pred_KF")) +
  xlab('Date') +
  ylab('Value')

p + ggtitle("E-mini S&P 500 Future")+theme(plot.title = element_text(hjust = 0.5))

print (p)

# Trading Strategy and Performance Metrics
df_price_xts<-xts(x= df_price$value, order.by = as.Date(df_price$date))
df_pred_xts<-xts(x= df_pred$value, order.by = as.Date(df_pred$date))
dfReturns <- lag(diff(log(df_price_xts)),-1)

posn <- vector(mode="character", length=length(df_pred_xts))
# Threshold in Percentage
tld=5
for (i in (1:(length(df_pred_xts)-1))){
  if(coredata(df_pred_xts[i + 1])>((coredata(df_price_xts[i])*(1+thld/100)))){
    if ( i== 1 || posn[i-1] < 1 ) {
      posn[i]=paste(index(df_pred_xts[i]),1,sep=",")
    }
    else {
      posn[i]=paste(index(df_pred_xts[i]),0,sep=",")
    }
    # print (posn[i])
  }else if (coredata(df_pred_xts[i + 1])<((coredata(df_price_xts[i])*(1-thld/100)))){
  
  # print (posn[i])
  if ( i== 1 || posn[i-1] > -1 ) {
    posn[i]=paste(index(df_pred_xts[i]),-1,sep=",")
  }
  else {
    posn[i]=paste(index(df_pred_xts[i]),0,sep=",")
  }
  }
}

write.table(posn, file=paste("posn.csv",sep=""), sep = ",",
row.names = F, col.names = F, quote = F)
sp500_kf = as.xts(read.zoo(file = paste("posn.csv",sep=""), format = "%Y-%m-%d", header = F, sep = ","))
spIntersect = merge(sp500_kf, dfReturns, all=F )
spReturns = spIntersect[,1] * spIntersect[,2]
# spReturns <- dfReturns*Lag(sp500_kf,1)
# spReturns[1] <- 0
names(dfReturns) <- 'ES_mini_Futures'
names(spReturns) <- 'KF_strategy'

charts.PerformanceSummary(cbind(dfReturns,spReturns))
Performance <- function(x) {
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
  return(Perf)
}
cbind(KF_strategy=Performance(spReturns),ES_mini_Futures=Performance(head(dfReturns,-1)))

# spCurve = log(cumprod(1 + spReturns))
#
# spBuyHoldCurve = log(cumprod(1 + spIntersect[,2]))
# # create matrix with both Model and SP500 returns
# spCombinedCurve = merge(spCurve,spBuyHoldCurve, all=F)
#
# # plot results
# returns_plot <- xyplot(
#   spCombinedCurve,
#   superpose=T,
#   col=c("darkred", "darkblue"),
#   lwd=2,
#   key=list(
#     text=list(
#       c(paste(SymbolName,"KF4"), "Buy & Hold")
#     ),
#     lines=list(
#       lwd=2, col=c("darkred", "darkblue")
#     )
#   )
# )
# # save device and save as png file
# trellis.device(device = "png", filename = paste("Output_plot.png",sep=""))
# print(returns_plot)
# dev.off()



