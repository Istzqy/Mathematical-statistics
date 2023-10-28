####
#date:2023-10-15~201018
#function:对不同基线数据下患者出院状态进行后验分布估计
#在贝叶斯的框架下，请给出患者出院状态时的死亡概率θ的（1）后验分布
####

#导入相关库
library(openxlsx)
library(ggplot2)
library(patchwork)
library(tidyverse)

#读取目录下mydata_mean.csv表格中的数据,存储在mydata中
mydata_mean=read.xlsx( "mydata_mean.xlsx")


# 步骤1：设定先验分布
prior_alpha <- 35  # 先验分布的参数alpha
prior_beta <- 65   # 先验分布的参数beta
# 绘制概率密度函数图像
x <- seq(0, 1, length.out = 100)
# 计算PDF和CDF
prior_pdf <- dbeta(x, prior_alpha, prior_beta)
cdf <- pbeta(x, prior_alpha, prior_beta)
ggplot(data.frame(x,prior_pdf,cdf), aes(x = x,y=prior_pdf)) +
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Beta Distribution Probability Density')+
  xlim(0, 1) + ylim(0, 9) +
  scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
  scale_y_continuous(limits=c(0,9),breaks = seq(0,9,1))
ggsave("Beta先验分布PDF.tiff",dpi=600, compression = 'lzw') 
#  ggtitle("Beta Distribution PDF")

# 绘制累积分布函数图像
ggplot(data.frame(x,prior_pdf,cdf), aes(x = x,y=cdf)) +
  geom_line(colour="blue",size=1)+
  xlim(0, 1) + ylim(0, 1) +
  xlab("Mortality θ") + ylab("Beta Distribution Cumulative Distribution") 
#  ggtitle("Beta Distribution CDF")
ggsave("Beta分布CDF3565.tiff",dpi=600, compression = 'lzw') 


# 步骤2：计算似然估计函数
Expire_flag <- mydata_mean$hospital_expire_flag   # 数据集
# 计算存活出现的频数
Expire_flag_0 <- sum(Expire_flag == 0)
# 计算死亡出现的频数
Expire_flag_1 <- length(Expire_flag) - Expire_flag_0
# 构建二项分布的似然估计函数(由于样本数量太大会导致幂次计算出来为0，因此缩小1000倍)
likelihood_function <- function(p) {
  likelihood <- (p^(Expire_flag_1/1000)) * ((1-p)^(Expire_flag_0/1000))
  return(likelihood)
}
# 生成一系列的概率值并计算似然估计
probs <- seq(0, 1, length.out = 100)
likelihoods <- sapply(probs, likelihood_function)
# 绘制似然估计的概率密度函数
#plot(probs, likelihoods, type = "l", xlab = "θ", ylab = "Likelihood Estimation", main = "Likelihood Estimation - Coin Toss",color="blue",size=1)
ggplot(data.frame(probs,likelihoods),aes(probs,likelihoods))+
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Likelihood Estimation')+
  scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
  scale_y_continuous(limits=c(0,0.0205),breaks = seq(0,0.02,0.0025))
ggsave("似然估计概率密度分布.tiff",dpi=600, compression = 'lzw') 
  


# 步骤3：计算证据（归一化常数）
evidence <- integrate(function(theta)  dbeta(theta, prior_alpha, prior_beta)* likelihood_function(theta), 0, 1)$value

# 步骤4：计算后验分布
# 后验分布的概率密度函数(利用贝叶斯公式法，一步步推：先验分布*似然估计/证据)
posterior_density <- prior_pdf*likelihoods/evidence  
#后验分布参数计算（直接利用伯努利分布与Beta分布的特点，得到后验分布仍然为Beta分布）
posterior_alpha <- prior_alpha + sum(Expire_flag)
posterior_beta <- prior_beta + length(Expire_flag) - sum(Expire_flag)
posterior_density2 <- dbeta(probs, posterior_alpha, posterior_beta)  # 后验分布的概率密度函数

# 绘制后验分布的概率密度图
#plot(theta_values, posterior_density, type = "l", xlab = "Theta (Probability of Heads)", ylab = "Posterior Density")
ggplot(data.frame(probs,posterior_density),aes(probs,posterior_density))+
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Mortality Posterior Density')
  # scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
  # scale_y_continuous(limits=c(0,0.0205),breaks = seq(0,0.02,0.0025))
#ggsave("后验概率概率密度分布.tiff",dpi=600, compression = 'lzw') 
ggplot(data.frame(probs,posterior_density2),aes(probs,posterior_density2))+
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Mortality Posterior Density')
#ggsave("后验概率概率密度分布_新.tiff",dpi=600, compression = 'lzw') 
# scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
# scale_y_continuous(limits=c(0,0.0205),breaks = seq(0,0.02,0.0025))

ggplot(data.frame(probs,cumsum(posterior_density)),aes(probs,cumsum(posterior_density)/cumsum(posterior_density)[length(posterior_density)]))+
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Cumulative Distribution')
# scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
# scale_y_continuous(limits=c(0,0.0205),breaks = seq(0,0.02,0.0025))
#ggsave("后验概率累积分布.tiff",dpi=600, compression = 'lzw') 
ggplot(data.frame(probs,cumsum(posterior_density2)),aes(probs,cumsum(posterior_density2)/cumsum(posterior_density2)[length(posterior_density2)]))+
  geom_line(colour="blue",size=1)+
  labs(x='Mortality θ ',y='Cumulative Distribution')
# scale_x_continuous(limits = c(-0.02,1),breaks = seq(0,1,0.2))+
# scale_y_continuous(limits=c(0,0.0205),breaks = seq(0,0.02,0.0025))
ggsave("后验概率累积分布_新.tiff",dpi=600, compression = 'lzw')

# 输出后验分布的统计指标（均值、中位数、95%置信区间）
posterior_mean <- posterior_alpha / (posterior_alpha + posterior_beta)
posterior_median <- qbeta(0.5, posterior_alpha, posterior_beta)
posterior_interval <- qbeta(c(0.025, 0.975), posterior_alpha, posterior_beta)
cat("Posterior Mean:", posterior_mean, "\n")
cat("Posterior Median:", posterior_median, "\n")
cat("95% Credible Interval:", posterior_interval, "\n")




# # 输出后验分布的统计指标（均值、中位数、95%置信区间）
# # 计算均值
# posterior_mean <- sum(posterior_density * probs) / sum(posterior_density)
# # 计算中位数
# cumulative_density <- cumsum(posterior_density) / sum(posterior_density)
# posterior_median <- x[which.max(cumulative_density >= 0.5)]
# # 计算置信区间
# lower_quantile <- quantile(posterior_density,0.025)
# upper_quantile <- quantile(posterior_density,0.975)
# 
# # 打印结果
# cat("Mean:", mean_value, "\n")
# cat("Median:", posterior_median, "\n")
# cat("95% Confidence Interval:", lower_quantile, "-", upper_quantile, "\n")
# 
# 
# 
# # 安装并加载`MASS`包
# library(MASS)
# 
# # # 拟合beta分布
# params <- list(shape1 = 4200, shape2 = 2000)
# fitdistr(x=posterior_density,densfun = "beta",start=params)
# 
# # 从拟合对象中提取参数估计值与标准误差
# shape1_est <- fit$estimate["shape1"]
# shape2_est <- fit$estimate["shape2"]
# se_shape1 <- fit$sd["shape1"]
# se_shape2 <- fit$sd["shape2"]












