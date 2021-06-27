library(shiny)
library(quantmod)
library(VGAM)
library(plotly)
library(DT)
library(TTR)
library(PerformanceAnalytics)
library(dplyr)
library(shinyWidgets)

options(scipen = 999)

AAPL<-readRDS("C:/Users/owner/Downloads/6511225-73cf2fa1eaf1d0d556599ef94c969888b8cb69a0/AAPL.rds")

Full_analysis <- function(x, strt_date = '2019-01-01', commission = 0){
  # Getting stock prices of AAPL, TSLA and NFLX
  AAPL<-x #getSymbols(x, src = 'yahoo', from = strt_date, auto.assign = FALSE)
  col_nms<-c("AAPL.Open",     "AAPL.High",     "AAPL.Low",      "AAPL.Close",   "AAPL.Volume",   "AAPL.Adjusted")
  names(AAPL)<-col_nms
  
  # Creating Leading and Lagging Technical Indicators
  
  # a. Simple Moving Average (SMA)
  sma20_aapl <- SMA(AAPL$AAPL.Close, n = 20)
  sma50_aapl <- SMA(AAPL$AAPL.Close, n = 50)
  
  # b.  Parabolic Stop And Reverse (SAR)
  sar_aapl <- SAR(cbind(Hi(AAPL),Lo(AAPL)), accel = c(0.02, 0.2))
  
  # c. Commodity Channel Index (CCI)
  cci_aapl <- CCI(HLC(AAPL), n = 20, c = 0.015)
  
  # d. Rate of Change (ROC)
  roc_aapl <- ROC(AAPL$AAPL.Close, n = 25)
  
  # e. Stochastic Momentum Index (SMI)
  smi_aapl <- SMI(HLC(AAPL),
                  n = 13, nFast = 2, nSlow = 25, nSig = 9)
  
  # f. Williams %R
  wpr_aapl <- WPR(HLC(AAPL), n = 14)
  colnames(wpr_aapl) <- 'wpr'
  
  # Creating Trading signal with Indicators
  
  # SMA 
  # SMA 20 Crossover Signal 
  sma20_aapl_ts <- Lag(
    ifelse(Lag(Cl(AAPL)) < Lag(sma20_aapl) & Cl(AAPL) > sma20_aapl,1,
           ifelse(Lag(Cl(AAPL)) > Lag(sma20_aapl) & Cl(AAPL) < sma20_aapl,-1,0)))
  sma20_aapl_ts[is.na(sma20_aapl_ts)] <- 0
  # SMA 50 Crossover Signal
  sma50_aapl_ts <- Lag(
    ifelse(Lag(Cl(AAPL)) < Lag(sma50_aapl) & Cl(AAPL) > sma50_aapl,1,
           ifelse(Lag(Cl(AAPL)) > Lag(sma50_aapl) & Cl(AAPL) < sma50_aapl,-1,0)))
  sma50_aapl_ts[is.na(sma50_aapl_ts)] <- 0
  # SMA 20 and SMA 50 Crossover Signal
  sma_aapl_ts <- Lag(
    ifelse(Lag(sma20_aapl) < Lag(sma50_aapl) & sma20_aapl > sma50_aapl,1,
           ifelse(Lag(sma20_aapl) > Lag(sma50_aapl) & sma20_aapl < sma50_aapl,-1,0)))
  sma_aapl_ts[is.na(sma_aapl_ts)] <- 0
  
  
  # 2. Parabolic Stop And Reverse (SAR) 
  sar_aapl_ts <- Lag(
    ifelse(Lag(Cl(AAPL)) < Lag(sar_aapl) & Cl(AAPL) > sar_aapl,1,
           ifelse(Lag(Cl(AAPL)) > Lag(sar_aapl) & Cl(AAPL) < sar_aapl,-1,0)))
  sar_aapl_ts[is.na(sar_aapl_ts)] <- 0
  
  # 3. Commodity Channel Index  (CCI)
  cci_aapl_ts <- Lag(
    ifelse(Lag(cci_aapl) < (-100) & cci_aapl > (-100),1,
           ifelse(Lag(cci_aapl) < (100) & cci_aapl > (100),-1,0)))
  cci_aapl_ts[is.na(cci_aapl_ts)] <- 0
  
  
  # 4. Rate of Change (ROC)
  roc_aapl_ts <- Lag(
    ifelse(Lag(roc_aapl) < (-0.05) & roc_aapl > (-0.05),1,
           ifelse(Lag(roc_aapl) < (0.05) & roc_aapl > (0.05),-1,0)))
  roc_aapl_ts[is.na(roc_aapl_ts)] <- 0
  
  
  # 5. Stochastic Momentum Index (SMI)
  smi_aapl_ts <- Lag(
    ifelse(Lag(smi_aapl[,1]) < Lag(smi_aapl[,2]) & smi_aapl[,1] > smi_aapl[,2],1, 
           ifelse(Lag(smi_aapl[,1]) > Lag(smi_aapl[,2]) & smi_aapl[,1] < smi_aapl[,2],-1,0)))
  smi_aapl_ts[is.na(smi_aapl_ts)] <- 0
  
  # 6. williams %R
  wpr_aapl_ts <- Lag(
    ifelse(Lag(wpr_aapl) > 0.8 & wpr_aapl < 0.8,1,
           ifelse(Lag(wpr_aapl) > 0.2 & wpr_aapl < 0.2,-1,0)))
  wpr_aapl_ts[is.na(wpr_aapl_ts)] <- 0
  
  # Creating Trading Strategies using Signals
  # 1. SMA 20 and SMA 50 Crossover Strategy
  sma_aapl_strat <- ifelse(sma_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    sma_aapl_strat[i] <- ifelse(sma_aapl_ts[i] == 1,1,ifelse(sma_aapl_ts[i] == -1,0,sma_aapl_strat[i-1]))
  }
  sma_aapl_strat[is.na(sma_aapl_strat)] <- 1
  sma_aapl_stratcomp <- cbind(sma20_aapl, sma50_aapl, sma_aapl_ts, sma_aapl_strat)
  colnames(sma_aapl_stratcomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')
  
  # Parabolic SAR Strategy 
  sar_aapl_strat <- ifelse(sar_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    sar_aapl_strat[i] <- ifelse(sar_aapl_ts[i] == 1,1,ifelse(sar_aapl_ts[i] == -1,0,sar_aapl_strat[i-1]))
  }
  sar_aapl_strat[is.na(sar_aapl_strat)] <- 1
  sar_aapl_stratcomp <- cbind(Cl(AAPL), sar_aapl, sar_aapl_ts, sar_aapl_strat)
  colnames(sar_aapl_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')
  
  # CCI
  cci_aapl_strat <- ifelse(cci_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    cci_aapl_strat[i] <- ifelse(cci_aapl_ts[i] == 1,1,ifelse(cci_aapl_ts[i] == -1,0,cci_aapl_strat[i-1]))
  }
  cci_aapl_strat[is.na(cci_aapl_strat)] <- 1
  cci_aapl_stratcomp <- cbind(cci_aapl, cci_aapl_ts, cci_aapl_strat)
  colnames(cci_aapl_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')
  
  # ROC
  roc_aapl_strat <- ifelse(roc_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    roc_aapl_strat[i] <- ifelse(roc_aapl_ts[i] == 1,1,ifelse(roc_aapl_ts[i] == -1,0,roc_aapl_strat[i-1]))
  }
  roc_aapl_strat[is.na(roc_aapl_strat)] <- 1
  roc_aapl_stratcomp <- cbind(roc_aapl, roc_aapl_ts, roc_aapl_strat)
  colnames(roc_aapl_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')
  
  # SMI
  smi_aapl_strat <- ifelse(smi_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    smi_aapl_strat[i] <- ifelse(smi_aapl_ts[i] == 1,1,ifelse(smi_aapl_ts[i] == -1,0,smi_aapl_strat[i-1]))
  }
  smi_aapl_strat[is.na(smi_aapl_strat)] <- 1
  smi_aapl_stratcomp <- cbind(smi_aapl[,1],smi_aapl[,2],smi_aapl_ts,smi_aapl_strat)
  colnames(smi_aapl_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')
  
  # WPR
  wpr_aapl_strat <- ifelse(wpr_aapl_ts > 1,0,1)
  for (i in 1 : length(Cl(AAPL))) {
    wpr_aapl_strat[i] <- ifelse(wpr_aapl_ts[i] == 1,1,ifelse(wpr_aapl_ts[i] == -1,0,wpr_aapl_strat[i-1]))
  }
  wpr_aapl_strat[is.na(wpr_aapl_strat)] <- 1
  wpr_aapl_stratcomp <- cbind(wpr_aapl, wpr_aapl_ts, wpr_aapl_strat)
  colnames(wpr_aapl_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')
  
  # Trading Strategy Performance 
  # Calculating Returns & setting Benchmark for companies
  ret_aapl <- (AAPL$AAPL.Close-stats::lag(AAPL$AAPL.Close))/stats::lag(AAPL$AAPL.Close)
  benchmark_aapl <- ret_aapl
  #commission<-0
  # SMA 
  sma_aapl_ret <- ifelse((sma_aapl_ts == 1|sma_aapl_ts == -1) & sma_aapl_strat != Lag(sma_aapl_ts), (ret_aapl-commission)*sma_aapl_strat, ret_aapl*sma_aapl_strat)
  
  # Parabolic SAR 
  sar_aapl_ret <- ifelse((sar_aapl_ts == 1|sar_aapl_ts == -1) & sar_aapl_strat != Lag(sar_aapl_ts), (ret_aapl-commission)*sar_aapl_strat, ret_aapl*sar_aapl_strat)
  
  # CCI  
  cci_aapl_ret <- ifelse((cci_aapl_ts == 1|cci_aapl_ts == -1) & cci_aapl_strat != Lag(cci_aapl_ts), (ret_aapl-commission)*cci_aapl_strat, ret_aapl*cci_aapl_strat)
  
  # ROC  
  roc_aapl_ret <- ifelse((roc_aapl_ts == 1|roc_aapl_ts == -1) & roc_aapl_strat != Lag(roc_aapl_ts), (ret_aapl-commission)*roc_aapl_strat, ret_aapl*roc_aapl_strat)
  
  # SMI 
  smi_aapl_ret <-  ifelse((smi_aapl_ts == 1|smi_aapl_ts == -1) & smi_aapl_strat != Lag(smi_aapl_ts), (ret_aapl-commission)*smi_aapl_strat, ret_aapl*smi_aapl_strat)
  
  # WPR  
  wpr_aapl_ret <- ifelse((wpr_aapl_ts == 1|wpr_aapl_ts == -1) & wpr_aapl_strat != Lag(wpr_aapl_ts), (ret_aapl-commission)*wpr_aapl_strat, ret_aapl*wpr_aapl_strat)
  
  a<-data.frame("Symbol" = c("Stock")
                ,"CR_BENCH" = Return.cumulative(ret_aapl)[1,1]
                ,"CR_SMA" = Return.cumulative(sma_aapl_ret)[1,1]
                ,"CR_PSAR" = Return.cumulative(sar_aapl_ret)[1,1]
                , "CR_CCI" = Return.cumulative(cci_aapl_ret)[1,1]
                , "CR_ROC" = Return.cumulative(roc_aapl_ret)[1,1]
                , "CR_SMI" = Return.cumulative(smi_aapl_ret)[1,1]
                , "CR_WPR" = Return.cumulative(wpr_aapl_ret)[1,1]
                ,"AR_BENCH" = Return.annualized(ret_aapl)[1,1]
                ,"AR_SMA" = Return.annualized(sma_aapl_ret)[1,1]
                ,"AR_PSAR" = Return.annualized(sar_aapl_ret)[1,1]
                , "AR_CCI" = Return.annualized(cci_aapl_ret)[1,1]
                , "AR_ROC" = Return.annualized(roc_aapl_ret)[1,1]
                , "AR_SMI" = Return.annualized(smi_aapl_ret)[1,1]
                , "AR_WPR" = Return.annualized(wpr_aapl_ret)[1,1])
  
  b<-data.frame("Symbol" = c("Stock")
                ,"CR_Bench" = Return.cumulative(ret_aapl)[1,1]
                ,"CR_Max_signal" =do.call(pmax,a[2:8])
                ,"Name_Max_signal" = names(a[match(do.call(pmax,a[2:8]),a)])
                ,"AR_Bench" = Return.annualized(ret_aapl)[1,1]
                ,"AR_Max_signal" =do.call(pmax,a[9:15])
                ,"Name_Max_signal" = names(a[match(do.call(pmax,a[9:15]),a)]))
  
sma_aapl_stratcomp$`SMA SIGNAL` <- ifelse(sma_aapl_stratcomp$`SMA SIGNAL` == 1, "buy", ifelse(sma_aapl_stratcomp$`SMA SIGNAL` ==-1, "sell",""))
sar_aapl_stratcomp$`SAR SIGNAL` <- ifelse(sar_aapl_stratcomp$`SAR SIGNAL` == 1, "buy", ifelse(sar_aapl_stratcomp$`SAR SIGNAL` ==-1, "sell",""))
cci_aapl_stratcomp$`CCI SIGNAL` <- ifelse(cci_aapl_stratcomp$`CCI SIGNAL` == 1, "buy", ifelse(cci_aapl_stratcomp$`CCI SIGNAL` ==-1, "sell",""))
roc_aapl_stratcomp$`ROC SIGNAL` <- ifelse(roc_aapl_stratcomp$`ROC SIGNAL` == 1, "buy", ifelse(roc_aapl_stratcomp$`ROC SIGNAL` ==-1, "sell",""))
smi_aapl_stratcomp$`SMI SIGNAL` <- ifelse(smi_aapl_stratcomp$`SMI SIGNAL` == 1, "buy", ifelse(smi_aapl_stratcomp$`SMI SIGNAL` ==-1, "sell",""))
wpr_aapl_stratcomp$`WPR SIGNAL` <- ifelse(wpr_aapl_stratcomp$`WPR SIGNAL` == 1, "buy", ifelse(wpr_aapl_stratcomp$`WPR SIGNAL` ==-1, "sell",""))
  

sma_aapl_stratcomp$`SMA POSITION` <- ifelse(sma_aapl_stratcomp$`SMA POSITION` == 1, "own", ifelse(sma_aapl_stratcomp$`SMA POSITION` ==0, "sold",""))
sar_aapl_stratcomp$`SAR POSITION` <- ifelse(sar_aapl_stratcomp$`SAR POSITION` == 1, "own", ifelse(sar_aapl_stratcomp$`SAR POSITION` ==0, "sold",""))
cci_aapl_stratcomp$`CCI POSITION` <- ifelse(cci_aapl_stratcomp$`CCI POSITION` == 1, "own", ifelse(cci_aapl_stratcomp$`CCI POSITION` ==0, "sold",""))
roc_aapl_stratcomp$`ROC POSITION` <- ifelse(roc_aapl_stratcomp$`ROC POSITION` == 1, "own", ifelse(roc_aapl_stratcomp$`ROC POSITION` ==0, "sold",""))
smi_aapl_stratcomp$`SMI POSITION` <- ifelse(smi_aapl_stratcomp$`SMI POSITION` == 1, "own", ifelse(smi_aapl_stratcomp$`SMI POSITION` ==0, "sold",""))
wpr_aapl_stratcomp$`WPR POSITION` <- ifelse(wpr_aapl_stratcomp$`WPR POSITION` == 1, "own", ifelse(wpr_aapl_stratcomp$`WPR POSITION` ==0, "sold",""))


  d<-data.frame("Date" = tail(index(sma_aapl_stratcomp),30)
                ,"sma_signal" = tail(sma_aapl_stratcomp$`SMA SIGNAL`,30)
                ,"sma_position" = tail(sma_aapl_stratcomp$`SMA POSITION`,30)
                ,"sar_signal" = tail(sar_aapl_stratcomp$`SAR SIGNAL`,30)
                ,"sar_position" = tail(sar_aapl_stratcomp$`SAR POSITION`,30)
                ,"cci_signal" = tail(cci_aapl_stratcomp$`CCI SIGNAL`,30)
                ,"cci_position" = tail(cci_aapl_stratcomp$`CCI POSITION`,30)
                ,"roc_signal" = tail(roc_aapl_stratcomp$`ROC SIGNAL`,30)
                ,"roc_position" = tail(roc_aapl_stratcomp$`ROC POSITION`,30)
                ,"smi_signal" = tail(smi_aapl_stratcomp$`SMI SIGNAL`,30)
                ,"smi_position" = tail(smi_aapl_stratcomp$`SMI POSITION`,30)
                ,"wpr_signal" = tail(wpr_aapl_stratcomp$`WPR SIGNAL`,30)
                ,"wpr_position" = tail(wpr_aapl_stratcomp$`WPR POSITION`,30)
                )
  
  final<-list(Symbol = x
              ,Returns_Calc = a
              ,Sum_best_returns = b
              ,sma_strat=sma_aapl_stratcomp
              ,sar_strat=sar_aapl_stratcomp
              ,cci_strat=cci_aapl_stratcomp
              ,roc_strat=roc_aapl_stratcomp
              ,smi_strat=smi_aapl_stratcomp
              ,wpr_strat=wpr_aapl_stratcomp
              ,signals = d)
  return(final)
  
}
