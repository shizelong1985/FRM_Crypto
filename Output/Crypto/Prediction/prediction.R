rm(list = ls(all = TRUE))

wdir = "/Users/annshchekina/Desktop/Kod/FRM_Crypto"

outliers = c(301,302,651)

channel = "Crypto"
s = 63

setwd(wdir)

input_path = paste0("Output/", channel, "/Lambda")
output_path = paste0("Output/", channel, "/Prediction") 
output_path_quant = paste0("Output/", channel, "/Sensitivity/Plots") 

library(dplyr)
library(bootstrap)

#### Calculating time series ####

FRM_index = read.csv(file = paste0(input_path, "/FRM_Crypto_index.csv"), header = TRUE)
crix = read.csv(file = paste0(output_path, "/crix.csv"), header = TRUE) 

FRM_crix = merge(FRM_index, crix, by = "date")
colnames(FRM_crix)[3] = "crix"
FRM_crix = FRM_crix[-outliers, ]
FRM_crix = FRM_crix[1:which(FRM_crix$date=="2021-02-01"), ]

ln_rn_day = (FRM_crix$crix[-1]/FRM_crix$crix[-nrow(FRM_crix)]) %>% log()
FRM_crix = cbind(FRM_crix[-1,], ln_rn_day)
colnames(FRM_crix)[4] = "log_rn"
N = nrow(FRM_crix)

var = sapply(s:N, function(i) var(FRM_crix$log_rn[(i-s+1):i])) 
var = c(rep(NA, s-1), var)
FRM_crix = FRM_crix %>% cbind(var)

cor.test(FRM_crix$var[-c(1:(s-1))], FRM_crix$frm[-c(1:(s-1))])

#### end ####

cond_quant_btc = read.csv(file = paste0(input_path, "/cond_quant_wide.csv"), header = TRUE)[, "BTC"] 
btc_price = read.csv(file = paste0("Input/", channel, "/20141128-20210709/Crypto_Price_20210709.csv"), header = TRUE)[, c("date", "BTC")]
btc_price = btc_price[which(btc_price$date=="2015-01-31"):nrow(btc_price),]
btc_rn_ln = (btc_price$BTC[-1]/btc_price$BTC[-nrow(btc_price)]) %>% log()

png(paste0(output_path_quant, "/Plots/cond_quant_btc.png"), width = 900, height = 600, bg = "transparent")

plot(btc_rn_ln, type = "l", col = "blue", xlab = "", ylab = "", xaxt = "n", lwd = 2, ylim = c(-0.1, 0.1))
lines(cond_quant_btc, type = "l", col = "black",axes = FALSE, xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(FRM_crix$log_rn)))
div = floor(N/5)
ll = c(1, div, 2*div, 3*div, 4*div, N)
axis(1, at = ll, labels = FRM_crix$date[ll])
mtext("CRIX log return", side = 4, line = 3)

dev.off()

#### Plotting ####

png(paste0(output_path, "/Plots/return_day.png"), width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_crix$frm, type = "l", col = "blue", xlab = "", ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(FRM_crix$log_rn, type = "l", col = "black",axes = FALSE, xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(FRM_crix$log_rn)))
div = floor(N/5)
ll = c(1, div, 2*div, 3*div, 4*div, N)
axis(1, at = ll, labels = FRM_crix$date[ll])
mtext("CRIX log return", side = 4, line = 3)

dev.off()


png(paste0(output_path, "/Plots/variance.png"), width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_crix$frm[s:N], type = "l", col = "blue", xlab = "", ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(FRM_crix$var[s:N], type = "l", col = "black",axes = FALSE, xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(FRM_crix$var[s:N])))
div = floor((N-s+1)/5)
ll = c(1, div, 2*div, 3*div, 4*div, N-s+1) + s - 1
axis(1, at = ll, labels = FRM_crix$date[ll])
mtext("CRIX log return rolling variance", side = 4, line = 3)

dev.off()


#Calculate the scale

y = FRM_crix$var[s:N]
x = FRM_crix$frm[s:N]
c = sum(x*y)/sum(x*x)

kernel = abs(y-c*x)

plot(kernel, type = "l", col = "red", xlab = "", xaxt = "n", ylab = "", ylim = range(y), lwd = 2)
lines(y, type = "l", col = "black")
ll = c(1, div, 2*div, 3*div, 4*div, N-s+1) + s - 1
axis(1, at = ll, labels = FRM_crix$date[ll])


#### end ####


#### Running lagged regressions ####

#Cross-validated R square
k_fold_rsq <- function(lmfit, ngroup=30) {
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

#Rolling window out-of-sample R square
oos_rsq <- function(dat, wnd=60) {
  T_dat = nrow(dat) 
  form = as.formula(paste(colnames(dat)[1], "~.", sep = ""))
  predict = sapply(1:(T_dat-wnd), function(i) {
    dat_i = dat[i:(i+wnd-1),]
    lm_i = lm(form, dat_i)
    predict(lm_i, dat[i+wnd, -1])
  })
  y = dat[-(1:wnd), 1]
  rss = (y - predict)^2 %>% sum()
  tss = (y - mean(y))^2 %>% sum()
  rsq = 1 - rss/tss
  rsq
}

#Regression: daily obs, lagged daily, weekly, monthly

day_lags = 10
week_lags = 5
month_lags = 5

data_lagged = FRM_crix[c("date", "log_rn", "var", "frm")]

for (i in 1:10) {
  FRMD = c(rep(NA, i), data_lagged$frm[1:(N-i)])
  data_lagged = data_lagged %>% cbind(FRMD)
  data_lagged = data_lagged %>% rename_with(~gsub("FRMD", paste0("FRM_D", i), .x))
}

for (i in 1:5) {
  FRMW = c(rep(NA, 5*i), data_lagged$frm[1:(N-5*i)])
  data_lagged = data_lagged %>% cbind(FRMW)
  data_lagged = data_lagged %>% rename_with(~gsub("FRMW", paste0("FRM_W", i), .x))
}

#Note: months can alternatively be first available day that month
for (i in 1:5) {
  FRMM = c(rep(NA, 20*i), data_lagged$frm[1:(N-20*i)])
  data_lagged = data_lagged %>% cbind(FRMM)
  data_lagged = data_lagged %>% rename_with(~gsub("FRMM", paste0("FRM_M", i), .x))
}

#Variance as dependent variable

data_day_var = data_lagged[, c("var", "FRM_D1", "FRM_D2", "FRM_D3", "FRM_D4", "FRM_D5", 
                               "FRM_D6", "FRM_D7", "FRM_D8", "FRM_D9", "FRM_D10")] %>% na.omit()

lmfit_day <- lm(var ~ ., data_day_var)
k_fold_rsq(lmfit_day)
oos_rsq(data_day_var)


data_week_var = data_lagged[, c("var", "FRM_W1", "FRM_W2", "FRM_W3", "FRM_W4", "FRM_W5")] %>% na.omit()

lmfit_week <- lm(var ~ ., data_week_var)
k_fold_rsq(lmfit_week)
oos_rsq(data_week_var)


data_month_var = data_lagged[, c("var", "FRM_M1", "FRM_M2", "FRM_M3", "FRM_M4", "FRM_M5")] %>% na.omit()

lmfit_month <- lm(var ~ ., data_month_var)
k_fold_rsq(lmfit_month)
oos_rsq(data_month_var)

#Return as dependent variable

data_day_rn = data_lagged[, c("log_rn", "FRM_D1", "FRM_D2", "FRM_D3", "FRM_D4", "FRM_D5", 
                               "FRM_D6", "FRM_D7", "FRM_D8", "FRM_D9", "FRM_D10")] %>% na.omit()

lmfit_day <- lm(log_rn ~ ., data_day_rn)
k_fold_rsq(lmfit_day)
oos_rsq(data_day_rn)


data_week_rn = data_lagged[, c("log_rn", "FRM_W1", "FRM_W2", "FRM_W3", "FRM_W4", "FRM_W5")] %>% na.omit()

lmfit_week <- lm(log_rn ~ ., data_week_rn)
k_fold_rsq(lmfit_week)
oos_rsq(data_week_rn)


data_month_rn = data_lagged[, c("log_rn", "FRM_M1", "FRM_M2", "FRM_M3", "FRM_M4", "FRM_M5")] %>% na.omit()

lmfit_month <- lm(log_rn ~ ., data_month_rn)
k_fold_rsq(lmfit_month)
oos_rsq(data_month_rn)


#Does variance predict itself just as well?

var_lagged = FRM_crix["var"]

for (i in 1:10) {
  varD = c(rep(NA, i), var_lagged$var[1:(N-i)])
  var_lagged = var_lagged %>% cbind(varD)
  var_lagged = var_lagged %>% rename_with(~gsub("varD", paste0("var_D", i), .x))
}

for (i in 1:5) {
  varW = c(rep(NA, 5*i), var_lagged$var[1:(N-5*i)])
  var_lagged = var_lagged %>% cbind(varW)
  var_lagged = var_lagged %>% rename_with(~gsub("varW", paste0("var_W", i), .x))
}

#Note: months can alternatively be first available day that month
for (i in 1:5) {
  varM = c(rep(NA, 20*i), var_lagged$var[1:(N-20*i)])
  var_lagged = var_lagged %>% cbind(varM)
  var_lagged = var_lagged %>% rename_with(~gsub("varM", paste0("var_M", i), .x))
}

data_day_var = var_lagged[, c("var", "var_D1", "var_D2", "var_D3", "var_D4", "var_D5", 
                               "var_D6", "var_D7", "var_D8", "var_D9", "var_D10")] %>% na.omit()

lmfit_day <- lm(var ~ ., data_day_var)
k_fold_rsq(lmfit_day)
oos_rsq(data_day_var)


data_week_var = var_lagged[, c("var", "var_W1", "var_W2", "var_W3", "var_W4", "var_W5")] %>% na.omit()

lmfit_week <- lm(var ~ ., data_week_var)
k_fold_rsq(lmfit_week)
oos_rsq(data_week_var)


data_month_var = var_lagged[, c("var", "var_M1", "var_M2", "var_M3", "var_M4", "var_M5")] %>% na.omit()

lmfit_month <- lm(var ~ ., data_month_var)
k_fold_rsq(lmfit_month)
oos_rsq(data_month_var)

#### end ####



