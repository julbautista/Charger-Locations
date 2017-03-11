source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
library(corrplot)
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Charger Locations")

#read data
supercharger <- read.csv("supercharger.csv", header = TRUE)
rem <- supercharger$Remoteness
use <- supercharger$Usage
df <- data.frame(rem,use)

#initial look with OLS
ggplot(df,aes(rem, use))  +  public + ggtitle("OLS Regression") +
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_point(size = 1.9, alpha = 0.85) +
  geom_smooth(method= 'lm', alpha = 0.2)
ggsave("ols.pdf", width = 17, height = 8.5)

#initial look with loess
ggplot(df,aes(rem, use)) + public + ggtitle("LOESS Regression") +
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_point(size = 1.9, alpha = 0.85) +
  geom_smooth(method ='loess', alpha = 0.2)
ggsave("loess.pdf", width = 17, height = 8.5)

#initial look with convex hull to create shape around the instance space
ggplot(df,aes(rem, use)) + public + ggtitle("Shrinking Variance on LOESS") +
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_polygon(data = df[chull(df$rem,df$use),], fill = "light blue", alpha = 0.2) + 
  geom_point(size = 1.9, alpha = 0.85) +
  geom_smooth(method= 'loess', alpha = 0)
ggsave("hull.pdf", width = 17, height = 8.5)

#fit stan model
stanc("Charger.stan")$status
fit <- stan("Charger.stan",
            data = c("rem","use"),
            iter = 2000, chains = 4)
beep(1)

#create new dataframe
use_est<-colMeans(extract(fit)$use_est)
df <- data.frame(use,use_est)

#plot of the prediction vs the real data
ggplot(df) + 
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_point(aes(rem,use), size = 1) +
  geom_line(aes(rem,use), colour = jbpal$green) +
  geom_line(aes(rem,use_est), size = 1)

#add upper and lower bound to df
lowbound <- apply(extract(fit)$use_est, 2, quantile, prob = 0.025)
upbound <- apply(extract(fit)$use_est, 2, quantile, prob = 0.975)
df <- data.frame(rem,use,use_est,lowbound,upbound)

#plot of 95% interval of usage
ggplot(df) + public + ggtitle("Beta Distribution Model") +
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_ribbon(aes(x = rem, ymin = lowbound, ymax = upbound), alpha = 0.14) +
  geom_line(aes(x = rem, y = use_est), size = 0.85) + 
  geom_point(aes(rem,use), size = 1.9, alpha = 0.7)
ggsave("betamodel.pdf", width = 17, height = 8.5)

#pinpointing greatest distance where 5% usage is near certain
ggplot(df) + ggtitle("Find Minimum Distance to Achieve 5% Usage Rate") + public +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  xlab("Distance from Nearest Major City") + ylab("Usage Rate by Owners in Nearest City") +
  geom_ribbon(aes(x = rem, ymin = lowbound, ymax = upbound), alpha = 0.14) + 
  geom_line(aes(x = rem, y = use_est), size = 0.75, alpha = 0.25) + 
  geom_point(aes(rem,use), size = 1.9, alpha = 0.85) +
  geom_line(aes(x = rem, y = lowbound), size = 0.75) +
  geom_hline(yintercept = 0.05) +
  geom_vline(xintercept = 56) +
  geom_point(aes(x = 56, y = 0.05), colour = jbpal$red, size = 2.4)
ggsave("fivepercent.pdf", width = 17, height = 8.5)
