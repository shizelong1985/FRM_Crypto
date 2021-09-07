rm(list = ls(all = TRUE))

library(RColorBrewer)
library(tidyr)

channel = "Crypto"
wdir = "/Users/annshchekina/Desktop/Kod/FRM_All"
setwd(wdir)

outliers = c(301, 302, 369, 651, 843)

output_path = "Output/Crypto/Sensitivity"

t5s63 = read.csv(file = "Output/Crypto/Lambda/FRM_Crypto_index.csv", header = TRUE)
colnames(t5s63)[2] = "t5"
t10s63 = read.csv(file = paste0(output_path, "/tau=10/s=63/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t10s63)[2] = "t10"
t25s63 = read.csv(file = paste0(output_path, "/tau=25/s=63/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t25s63)[2] = "t25"
t50s63 = read.csv(file = paste0(output_path, "/tau=50/s=63/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t50s63)[2] = "t50"
s63 = merge(t5s63, t10s63)
s63 = merge(s63, t25s63)
s63 = merge(s63, t50s63)
s63 = s63[-outliers,]

t5s21 = read.csv(file = paste0(output_path, "/tau=5/s=21/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t5s21)[2] = "t5"
t10s21 = read.csv(file = paste0(output_path, "/tau=10/s=21/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t10s21)[2] = "t10"
t25s21 = read.csv(file = paste0(output_path, "/tau=25/s=21/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t25s21)[2] = "t25"
t50s21 = read.csv(file = paste0(output_path, "/tau=50/s=21/Lambda/FRM_Crypto_index.csv"), header = TRUE)
colnames(t50s21)[2] = "t50"
s21 = merge(t5s21, t10s21)
s21 = merge(s21, t25s21)
s21 = merge(s21, t50s21)
s21 = s21[-outliers,]

N = nrow(s63)
div = floor(N/4)
plot_labels = s63$date[c(div, 2*div, 3*div, N)]

png(paste0(output_path, "/Plots/s63.png"), width = 900, height = 900, bg = "transparent")
ggplot(s63, aes(x = date, group = 1)) +
  geom_line(aes(y = t5, colour = "0.05"), size = 1.5) +
  geom_line(aes(y = t10, colour = "0.10"), size = 1.5) +
  geom_line(aes(y = t25, colour = "0.25"), size = 1.5) +
  geom_line(aes(y = t50, colour = "0.50"), size = 1.5) +
  scale_x_discrete(breaks = plot_labels) +
  scale_colour_brewer(palette="Set1") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(size = 1.5, colour = "black"),
        axis.text.x = element_text(hjust = 1, size = 20),
        axis.text.y = element_text(size = 20))
dev.off()

png(paste0(output_path, "/Plots/s21.png"), width = 900, height = 900, bg = "transparent")
ggplot(s21, aes(x = date, group = 1)) +
  geom_line(aes(y = t5, colour = "0.05"), size = 1.5) +
  geom_line(aes(y = t10, colour = "0.10"), size = 1.5) +
  geom_line(aes(y = t25, colour = "0.25"), size = 1.5) +
  geom_line(aes(y = t50, colour = "0.50"), size = 1.5) +
  scale_x_discrete(breaks = plot_labels) +
  scale_colour_brewer(palette="Set1") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(size = 1.5, colour = "black"),
        axis.text.x = element_text(hjust = 1, size = 20),
        axis.text.y = element_text(size = 20))
dev.off()


png(paste0(output_path, "/Boxplot/q75_", date_start, "_", date_end, "_", 
           channel, ".png"), width = 900, height = 600, bg = "transparent")

plot(FRM_q75$q75, type = "l", col = "darkgreen", xaxt = "n", ylab = "", xlab = "")
lines(FRM_index$FRM, col = "blue")
axis(1, at = ll, labels = boxplot_labels)

dev.off()


#VCRIX Comparison

vcrix = read.csv(file = "Input/Crypto/VCRIX/vcrix.txt", header = TRUE)
vcrix = vcrix$vcrix
FRM_index = read.csv(file = "Output/Crypto/Lambda/FRM_Crypto_index.csv", header = TRUE)

png(file = "Input/Crypto/VCRIX/vcrix.png", width = 900, height = 600, bg = "transparent")

par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$FRM[-c(301,302,651)], type = "l", col = "blue", xlab = "", ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(vcrix[-c(301,302,651)], type = "l", col = "red",axes = FALSE, xlab = "", ylab = "", xaxt = "n", lwd = 2)
axis(side = 4, at = pretty(range(vcrix[-c(301,302,651)])))
axis(1, at = ll, labels = boxplot_labels)
mtext("VCRIX", side = 4, line = 3)

dev.off()


#Line alternative to boxplot

for (tau in c(5, 10, 25, 50))
  for (s in c(21, 63)) {
    if (tau == 5 & s == 63) input_boxplot = paste0("Output/", channel, "/Lambda") else 
      input_boxplot = paste0("Output/", channel, "/Sensitivity/tau=", tau, "/s=", s, "/Lambda")
    
    FRM_index = read.csv(file = paste0(input_boxplot, "/FRM_Crypto_index.csv"), header = TRUE)[-outliers,]
    FRM_max = read.csv(file = paste0(input_boxplot, "/max_lambda.csv"), header = TRUE)[-outliers,]
    FRM_min = read.csv(file = paste0(input_boxplot, "/min_lambda.csv"), header = TRUE)[-outliers,]
    FRM_25 = read.csv(file = paste0(input_boxplot, "/Quantiles/q25_lambda.csv"), header = TRUE)[-outliers,]
    FRM_75 = read.csv(file = paste0(input_boxplot, "/Quantiles/q75_lambda.csv"), header = TRUE)[-outliers,]
    
    png(file = paste0("Output/", channel, "/Sensitivity/Plots/tau", tau, "s", s, ".png"), 
        width = 900, height = 600, bg = "transparent")
    
    plot(FRM_index$frm, type = "l", col = "blue", xlab = "", ylab = "", xaxt = "n", 
         lwd = 2, ylim = range(FRM_max$lambda)) #range(FRM_max$lambda)
    lines(FRM_max$lambda, col = "red", lwd = 0.5)
    lines(FRM_min$lambda, col = "red", lwd = 0.5)
    lines(FRM_25$quantile, col = "green", lwd = 0.5)
    lines(FRM_75$quantile, col = "green", lwd = 0.5)
    
    N = nrow(FRM_index)
    div = floor(N/5)
    plot_labels_boxplot = FRM_index$date[c(1, div, 2*div, 3*div, 4*div, N)]
    
    ll = c(1, div, 2*div, 3*div, 4*div, N)
    axis(1, at = ll, labels = FRM_index$date[ll])
    
    dev.off()
  }


# VIX influence for different setups

vix_inf = read.csv(file = "Output/Crypto/Macro/macro_influence.csv", header = TRUE)[-outliers, 1, drop = FALSE]

for (tau in c(5, 10, 25, 50))
  for (s in c(21, 63)) {
    if (tau == 5 & s == 63) input_macro = paste0("Output/", channel, "/Macro") else 
      input_macro = paste0("Output/", channel, "/Sensitivity/tau=", tau, "/s=", s, "/Macro")
    
    vix_inf_ind = read.csv(file = paste0(input_macro, "/macro_influence.csv"), header = TRUE)[-outliers, 6, drop = FALSE]
    colnames(vix_inf_ind) = paste0(tau, "_", s)
    
    vix_inf = cbind(vix_inf, vix_inf_ind)
  }

vix_inf_smooth = vix_inf

for (k in 2:9) {
  ss = smooth.spline(gsub("-", "", vix_inf[,"date"]), vix_inf[, k])$y
  vix_inf_smooth[, k] = ifelse(ss > 0, ss, 0)
}

vix_inf_long = pivot_longer(vix_inf_smooth, cols = 2:9, names_to = "setup")
s = sub(".*_", "", vix_inf_long$setup)
tau = sub("_.*", "", vix_inf_long$setup)
vix_inf_long = cbind(vix_inf_long, s, tau)
vix_inf_long = vix_inf_long[order(vix_inf_long$s, decreasing = TRUE),]

N = nrow(vix_inf_long)
div = floor(N/4)
plot_labels = vix_inf_long$date[c(div, 2*div, 3*div, N)]

group.colors <- c("5" = "#E41A1C", "10" = "#377EB8", "25" = "#4DAF4A", "50" = "#984EA3")
group.linetype <- c("21" = 2, "63" = 1)

png(file = paste0("Output/", channel, "/Sensitivity/Plots/vix.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(vix_inf_long, aes(x = date, value, group = setup)) + 
  geom_line(aes(color = tau, linetype = s), size=0.7) +
  scale_color_manual(values = group.colors) +
  scale_linetype_manual(values = group.linetype) +
  scale_x_discrete(breaks = plot_labels) +
  ylab("normalised # of non-zero betas") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.border = element_blank(),
        legend.key=element_blank(),
        panel.grid=element_blank())

dev.off()
