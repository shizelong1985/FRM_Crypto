rm(list = ls(all = TRUE))

wdir = "/Users/annshchekina/Desktop/Kod/FRM_Crypto"

channel = "Crypto"
tau = 0.05
s = 63

#Plot parameter defined based on the outliers
lambda_cutoff = 0.1359
M_macro = 5

setwd(wdir)

if (tau == 0.05) input_path = paste0("Output/", channel) else 
  input_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)
output_path = input_path 

#Check if package is installed, if not: install, either way: load 
if (!require(igraph)) install.packages("igraph"); library(igraph)
if (!require(qgraph)) install.packages("graph"); library(qgraph)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(gganimate)) install.packages("gganimate"); library(gganimate)
if (!require(av)) install.packages('av'); library(av)
if (!require(gifski)) install.packages("gifski"); library(gifski)
if (!require(strex)) install.packages("strex"); library(strex)
if (!require(magick)) install.packages("magick"); library(magick)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(matrixStats)) install.packages("matrixStats"); library(matrixStats)
if (!require(tools)) install.packages("tools"); library(tools)

#Set ggplot2 theme
theme_set(theme_classic())

#List of centrality types and their numbers
centralitylist = list("OutDegree" = 1, "InDegree" = 2, "Closeness" = 3, 
                      "Betweenness" = 4, "InInfluence" = 5, "OutInfluence" = 6)

#Read historical FRM index
FRM_index = read.csv(paste0(input_path, "/Lambda/FRM_", channel, "_index.csv"))

N = nrow(FRM_index)
div = floor(N/5)
plot_labels = FRM_index$date[c(1, div, 2*div, 3*div, 4*div, N)]


## Calculate centralities

#Create a list of files in the folder
file_list = list.files(path = paste0(input_path, "/Adj_Matrices"))
file_list = file_list[file_list!="Fixed"]

#dates = as.character(str_first_number(file_list), format = "%Y%m%d")
#dates = as.Date(dates, format = "%Y%m%d")
#N = length(file_list)

#Create a list of all network graphs
allgraphs = lapply(1:N, function(i) {
  data = read.csv(paste0(input_path, "/Adj_Matrices/", file_list[i]), row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
                                   vsize = c(5,15), DoNotPlot = TRUE)
  return(q)
})

allcentralities = centrality(allgraphs)
eigencentrality = lapply(1:N, function(i) eigen_centrality(as.igraph(allgraphs[[i]]))$vector)

#Calculate averages
outdegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$OutDegree))
indegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$InDegree))
closeness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Closeness))
betweenness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Betweenness))
eigenvector_avg = sapply(1:N, function(i) mean(eigencentrality[[i]]))

len = sapply(1:N, function(i) length(allcentralities[[i]]$OutDegree))

#Restructure list into individual node centralities
indegree_btc = sapply(1:N, function(i) allcentralities[[i]]$InDegree["BTC"]) / len
lambda_btc = read.csv(paste0(input_path, "/Lambda/lambdas_wide.csv"))[, c("date", "BTC")]
lambda_btc = merge(lambda_btc, FRM_index, by = "date")[, 2]
b_btc = read.csv(paste0(input_path, "/Lambda/constraint_wide.csv"))[, c("date", "BTC")]
b_btc = merge(b_btc, FRM_index, by = "date")[, 2]
indegree_eth = sapply(1:N, function(i) allcentralities[[i]]$InDegree["ETH"]) / len
lambda_eth = read.csv(paste0(input_path, "/Lambda/lambdas_wide.csv"))[, "ETH"]
b_eth = read.csv(paste0(input_path, "/Lambda/constraint_wide.csv"))[, "ETH"]


#Manually calculate

btc_id = matrix(0, N, 1)
eth_id = matrix(0, N, 1)
tot_d = matrix(0, N, 1)
btc_wid = matrix(0, N, 1)
eth_wid = matrix(0, N, 1)
tot_wd = matrix(0, N, 1)

for(i in 1:N) {
  data = read.csv(paste0(output_path, "/Adj_Matrices/adj_matix_", 
                         gsub("-", "", FRM_index$date[i]), ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  btc_id[i, 1] = sum(adj_matrix["BTC",] != 0) 
  tot_d[i, 1] = sum(adj_matrix != 0) 
  btc_wid[i, 1] = sum(abs(adj_matrix["BTC",])) 
  tot_wd[i, 1] = sum(abs(adj_matrix)) 
}

eth_N0 = which(is.na(indegree_eth)) %>% length() + 10

for(i in eth_N0:N) {
  data = read.csv(paste0(output_path, "/Adj_Matrices/adj_matix_", 
                         gsub("-", "", FRM_index$date[i]), ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  eth_id[i, 1] = sum(adj_matrix["ETH",] != 0) 
  eth_wid[i, 1] = sum(abs(adj_matrix["ETH",])) 
}


png(paste0(output_path, "/Centrality/BTC_id.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(lambda_btc, type = "l", col = "blue", xlab = "", 
     ylab = "BTC lambda", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(btc_id, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(btc_id)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("In-degree centrality", side = 4, line = 3)

dev.off()

png(paste0(output_path, "/Centrality/BTC_id_b.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(b_btc, type = "l", col = "green", xlab = "", 
     ylab = "Optimal constraint level", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(btc_id, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(btc_id)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("In-degree centrality", side = 4, line = 3)

dev.off()


png(paste0(output_path, "/Centrality/BTC_wid.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(lambda_btc, type = "l", col = "blue", xlab = "", 
     ylab = "BTC lambda", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(btc_wid, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(btc_wid)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("Weighted abs in-degree centrality", side = 4, line = 3)

dev.off()

png(paste0(output_path, "/Centrality/BTC_wid_b.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(b_btc, type = "l", col = "green", xlab = "", 
     ylab = "Optimal constraint level", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(btc_wid, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(btc_wid)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("Weighted abs in-degree centrality", side = 4, line = 3)

dev.off()

png(paste0(output_path, "/Centrality/tot_d.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$frm, type = "l", col = "blue", xlab = "", 
     ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(tot_d, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(tot_d)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("Total degree centrality", side = 4, line = 3)

dev.off()


png(paste0(output_path, "/Centrality/tot_wd.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$frm, type = "l", col = "blue", xlab = "", 
     ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(tot_wd, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(tot_wd)))
ll = which(FRM_index$date %in% plot_labels)
axis(1, at = ll, labels = plot_labels)
mtext("Total weighted abs degree centrality", side = 4, line = 3)

dev.off()


png(paste0(output_path, "/Centrality/ETH_indegree.png"), 
    width = 900, height = 600, bg = "transparent")

eth_N0 = which(eth_id==0) %>% length() + 10

par(mar = c(5, 4, 4, 4) + 0.3)
plot(lambda_eth[eth_N0:N], type = "l", col = "blue", xlab = "", 
     ylab = "ETH lambda", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(eth_id[eth_N0:N], type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(indegree_eth)))
plot_labels_eth = FRM_index$date[eth_N0:N]
N_eth = length(plot_labels_eth)
div = floor(N_eth/5)
plot_labels_eth = plot_labels_eth[c(1, div, 2*div, 3*div, 4*div, N_eth)]
ll = which(FRM_index$date %in% plot_labels_eth)
axis(1, at = ll, labels = plot_labels_eth)
mtext("In-degree centrality", side = 4, line = 3)

dev.off()

png(paste0(output_path, "/Centrality/ETH_indegree_b.png"), 
    width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(b_eth[eth_N0:eth_N1], type = "l", col = "green", xlab = "", 
     ylab = "Optimal constraint level", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(eth_wid[eth_N0:eth_N1], type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(indegree_eth)))
ll = which(FRM_index$date %in% plot_labels_eth)
axis(1, at = ll, labels = plot_labels_eth)
mtext("Weighted abs in-degree centrality", side = 4, line = 3)

dev.off()

## Plot FRM index vs all centralities

cent_plot = function(cent_type, lambda) {
  cent_string = deparse(substitute(cent_type))
  png(paste0(output_path, "/Centrality/FRM_", cent_string,".png"), 
      width = 900, height = 600, bg = "transparent")
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(lambda[-outliers], type = "l", col = "blue", xlab = "", 
       ylab = "FRM index", xaxt = "n", lwd = 2)
  par(new = TRUE)
  plot(cent_type[-outliers], type = "l", col = "red", axes = FALSE, 
       xlab = "", ylab = "", xaxt = "n")
  axis(side = 4, at = pretty(range(cent_type[-outliers])))
  ll = which(FRM_index$date[-outliers] %in% plot_labels)
  axis(1, at = ll, labels = plot_labels)
  mtext(paste0(gsub("_.*", "", cent_string) %>% toTitleCase, " centrality"), 
        side = 4, line = 3)
  dev.off()
}

cent_plot(outdegree_avg, FRM_index$frm)
cent_plot(indegree_avg, FRM_index$frm)
cent_plot(betweenness_avg, FRM_index$frm)
cent_plot(closeness_avg, FRM_index$frm)
cent_plot(eigenvector_avg, FRM_index$frm)



