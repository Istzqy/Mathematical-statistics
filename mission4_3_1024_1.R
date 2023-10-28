#根据已有参数绘制ROC曲线

#导入相关库
library(openxlsx)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(mixtools)
library(MASS)



#读取目录下mydata.csv表格中的数据,存储在mydata中
mydata_mean=read.xlsx( "mydata_mean.xlsx")

#取出患者出院状态这一列
Var_ex_flag= mydata_mean$hospital_expire_flag

#估计参数
mu0 <- -0.052
sigma0 <- 1.438
mu1 <- 4.12
sigma1 <- 1.401
alpha=0.689
# mu0 <- 4
# sigma0 <- 3
# mu1 <- 7
# sigma1 <- 2
# alpha=0.679
#原参数
# mu0 <- 0
# sigma0 <- 2
# mu1 <- 4
# sigma1 <- 2
# alpha=0.679


# 生成对应状态的符合混合高斯分布的随机数 x
# 生成对应状态的符合混合高斯分布的随机数 x
x <- ifelse(Var_ex_flag == 0, mvrnorm(sum(Var_ex_flag == 0), mu = 0, Sigma = 2), 
            mvrnorm(sum(Var_ex_flag == 1), mu = 4, Sigma = 2))
f0 <- dnorm(x, mean = mu0, sd = sigma0)
f1 <- dnorm(x, mean = mu1, sd = sigma1)
# 计算估计y的概率密度
#y_est <- alpha4 * dnorm(x3, mean = mu3, sd = sigma3) + (1 - alpha4) * dnorm(x3, mean = mu4, sd = sigma4)

# y_es_plot=ggplot(data.frame(x3,y_est), aes(x = x3)) +
#   geom_line(aes(x3,y_est),color="darkblue",linewidth =1)+
#   annotate("text", x = -4, y = 0.15,
#            label = "(b)", vjust = 1.5, size = 4.5)+
#   xlab("x") +
#   ylab("y_est")
# cowplot::plot_grid(y_es_plot,nrow=1)

f <- f1 / (f0 + f1)
# 计算来源于1的相对概率f与f1_es
threshold_f = seq(0, 1, by = 0.01)
# 初始化存储各指标的向量
fpr_f <- rep(0, length(threshold_f))
tpr_f <- rep(0, length(threshold_f))
for (i in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(f >= threshold_f[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f <- table(predicted, Var_ex_flag)
  if(dim(confusion_matrix_f)[1]==2)
  {
    # 计算真阳性率、假阳性率和F1值
    tp <- confusion_matrix_f[2, 2]
    tn <- confusion_matrix_f[1, 1]
    fp <- confusion_matrix_f[2, 1]
    fn <- confusion_matrix_f[1, 2]
    
    fpr_f[i] <- fp / (fp + tn)
    tpr_f[i] <- tp / (tp + fn)
  }
  else
  {
    if(predicted[i]==1)
    {
      fpr_f[i] <- 1
      tpr_f[i] <- 1 
    }
    else
    {
      fpr_f[i] <- 0
      tpr_f[i] <- 0 
    }
  }
  
}
library(MLmetrics)
# 
# # 使用roc函数创建ROC曲线对象
# # 计算AUC值
# auc_value_es <- AUC(f_es, Var_ex_flag)


ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#7773C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Auc = 0.9768 ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)
ggsave("EM算法估计分布ROC曲线.tiff",dpi=600, compression = 'lzw') 


#0.9774 0.9773 0.9763 0.9785 0.9756 0.9782 0.9717 0.9806 0.9778 0.978 /10 0.9771

#0.9743 0.9763 0.9795 0.9733 0.9796 0.9735 0.9796 0.9787 0.9775 0.9766 /10 0.9768
