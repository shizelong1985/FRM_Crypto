#Note 1: Export the macro file to csv and delete first rows
#Note 2: Rename files and folders with the new last date

rm(list = ls(all = TRUE))

library(zoo)
library(dplyr)

wdir = "/Users/annshchekina/Desktop/Kod/FRM_All"
setwd(wdir)

date_end_old_data = 20210525
date_end_old_data_str = "2021-05-25"

date_end_new_data = 20210709

input_path_new = paste0("Input/Crypto/", date_end_new_data)
input_path = paste0("Input/Crypto/20141128-", date_end_new_data)

Mktcap_prev = read.csv(file = paste0(input_path, "/Crypto_Mktcap_", date_end_old_data, ".csv"), header = TRUE)
Prices_prev = read.csv(file = paste0(input_path, "/Crypto_Price_", date_end_old_data, ".csv"), header = TRUE)
Macro_prev = read.csv(file = paste0(input_path, "/Crypto_Macro_", date_end_old_data, ".csv"), header = TRUE)

Mktcap_new = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Market_", date_end_new_data, ".csv"), header = TRUE)
colnames(Mktcap_new) = colnames(Mktcap_new) %>% toupper()
colnames(Mktcap_new)[1] = "date"
N1_m = which(Mktcap_new$date==date_end_old_data_str)
if (length(N1_m)==0) N1_m = 0
Mktcap_new = Mktcap_new[(N1_m+1):nrow(Mktcap_new), -which(names(Mktcap_new) %in% c("USDT", "USDC", "BUSD"))]

Prices_new = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Price_", date_end_new_data, ".csv"), header = TRUE)
colnames(Prices_new) = colnames(Prices_new) %>% toupper()
colnames(Prices_new)[1] = "date"
N1_s = which(Prices_new$date==date_end_old_data_str)
if (length(N1_s)==0) N1_s = 0
Prices_new = Prices_new[(N1_s+1):nrow(Prices_new), -which(names(Prices_new) %in% c("USDT", "USDC", "BUSD"))]

Macro = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Macro.csv"), header = TRUE, sep = ";", dec = ",")
colnames(Macro)[1] = "date"
Macro$date = as.character(as.Date(Macro$date, "%d/%m/%Y"))

#Macro = rbind(Macro_prev, Macro_new)
Mktcap = dplyr::bind_rows(Mktcap_prev, Mktcap_new)
Prices = dplyr::bind_rows(Prices_prev, Prices_new)

write.csv(Macro, paste0(input_path, "/Crypto_Macro_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
write.csv(Mktcap, paste0(input_path, "/Crypto_Mktcap_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
write.csv(Prices, paste0(input_path, "/Crypto_Price_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
