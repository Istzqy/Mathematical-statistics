####
#date:2020-10-18
#function:（2）画出θ的点估计值随时间变动的曲线图，（3）并添加相应的贝叶斯可信区间
####

#导入相关库
library(openxlsx)
library(ggplot2)
library(patchwork)
library(tidyverse)

#读取目录下mydata_mean.csv表格中的数据,存储在mydata中
mydata_mean=read.xlsx( "mydata_mean.xlsx")

#选取前100个Y的观测数据作为先验
# 步骤1：设定先验分布
prior_alpha <- sum(mydata_mean$hospital_expire_flag[1:100])  # 先验分布的参数alpha
prior_beta <- 100-prior_alpha   # 先验分布的参数beta

#步骤二更新后验分布
posterior_mean=rep(1.0,length(mydata_mean$hospital_expire_flag)-99)
posterior_mean[1]=prior_alpha / (prior_alpha + prior_beta)
posterior_alpha =rep(1.0,length(mydata_mean$hospital_expire_flag)-100)
posterior_beta  =rep(1.0,length(mydata_mean$hospital_expire_flag)-100)
posterior_interval  =matrix(1.0,length(mydata_mean$hospital_expire_flag)-99,2)
posterior_interval[1,]=qbeta(c(0.025, 0.975), prior_alpha, prior_beta)
for (i in 101:length(mydata_mean$hospital_expire_flag))
{
  posterior_alpha[i-100]=sum(mydata_mean$hospital_expire_flag[1:i])
  posterior_beta[i-100]=i-sum(mydata_mean$hospital_expire_flag[1:i])
  posterior_mean[i-100+1]=  posterior_alpha[i-100]/(posterior_alpha[i-100]+ posterior_beta[i-100]) 
  posterior_interval[i-100+1,] <- qbeta(c(0.025, 0.975), posterior_alpha[i-100],  posterior_beta[i-100])
}

observation=100:length(mydata_mean$hospital_expire_flag)
ggplot(data.frame(posterior_mean,observation),aes(x=observation,y=posterior_mean))+
  geom_line(colour="blue",size=0.5)+
  geom_ribbon(aes(ymin = posterior_interval[,1], ymax = posterior_interval[,2]), alpha = 0.3)+  # 添加置信区间
  theme_bw()+
  theme_test(base_size = 15)+
  labs(x=' Observation ',y='Mortality θ') +
  scale_x_continuous(limits = c(-100,6500),breaks = seq(0,6000,2000))+
  scale_y_continuous(limits=c(0.25,0.47),breaks = seq(0.25,0.45,0.05))+
  geom_hline(yintercept = posterior_mean[6174], linetype = "dashed", color = "red")+
  geom_text(x = 4500, y = 0.26, label = "θ = 0.3204", color = "black",family = "sans",size=5)+
  theme(text = element_text(family ="sans"))
ggsave("概率随观察量变换分布.tiff",dpi=600, compression = 'lzw')





