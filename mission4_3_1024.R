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

# 设置混合高斯分布的参数
mu0 <- 0
sigma0 <- 2
mu1 <- 4
sigma1 <- 2
alpha=0.680


# 生成对应状态的符合混合高斯分布的随机数 x
x <- ifelse(Var_ex_flag == 0, mvrnorm(sum(Var_ex_flag == 0), mu = mu0, Sigma = sigma0), 
            mvrnorm(sum(Var_ex_flag == 1), mu = mu1, Sigma = sigma1))

# 初始化估计参数
initial_params <- list(
  "normal" = list(mean = c(-0.5, 4.5), var = c(1.5, 1.5), p = c(0.5, 0.5)),
  "mix" = list(p = c(0.5, 0.5))
)

  # # 使用EM算法估计参数
  # fit <- normalmixEM(x, lambda = initial_params$mix$p, mu = initial_params$normal$mean, sigma = sqrt(initial_params$normal$var))
  # 
  # # 输出估计得到的参数值
  # estimated_params <- cbind(fit$lambda, fit$mu, sqrt(fit$sigma^2))
  # rownames(estimated_params) <- c("Component 1", "Component 2")
  # colnames(estimated_params) <- c("Alpha", "Mean", "Standard Deviation")
  # print(estimated_params)
  # mu0_es=estimated_params[1,2]
  # mu1_es=estimated_params[2,2]
  # sigma0_es=estimated_params[1,3]
  # sigma1_es=estimated_params[2,3]
  # alpha_est=estimated_params[1,1]
 
  #三次运算取平均后的结果 
 mu0_es_mean=0.052
 mu1_es_mean=4.12
 sigma0_es_mean=1.438
 sigma1_es_mean=1.401
 alpha_est_mean=0.689


# 输出真实参数值
true_params <- cbind(c(0.679, 0.320), c(0, 4), c(2, 2))
rownames(true_params) <- c("Component 1", "Component 2")
colnames(true_params) <- c("Alpha", "Mean", "Standard Deviation")
print(true_params)

library(cowplot)
# 计算每个观测值来源于N(μ0, σ0)和N(μ1, σ1)的概率密度
f0 <- dnorm(x, mean = mu0, sd = sigma0)
f1 <- dnorm(x, mean = mu1, sd = sigma1)
f0_es=dnorm(x, mean = mu0_es_mean,sd=sigma0_es_mean)
f1_es=dnorm(x, mean = mu1_es_mean,sd=sigma1_es_mean)
# 计算估计y的概率密度
y_est <- alpha_est_mean * dnorm(x, mean = mu0_es_mean, sd = sigma0_es_mean) + (1 - alpha_est_mean) * dnorm(x, mean = mu1_es_mean, sd = sigma1_es_mean)
#真实y概率密度
y_true <- alpha * dnorm(x, mean = mu0, sd = sigma0) + (1 - alpha) * dnorm(x, mean = mu1, sd = sigma1)

y_es_plot=ggplot(data.frame(x,y_est), aes(x = x)) +
  geom_line(aes(x,y_est),color="darkblue",linewidth =1)+
  annotate("text", x = -4, y = 0.15,
           label = "(b)", vjust = 1.5, size = 4.5)+
  xlab("x") +
  ylab("y_est")
y_tr_plot=ggplot(data.frame(x,y_true), aes(x = x)) +
  geom_line(aes(x,y_true),color="darkblue",linewidth =1)+
  annotate("text", x = -4, y = 0.125,
           label = "(a)", vjust = 1.5, size = 4.5)+
  xlab("x") +
  ylab("y_true")
cowplot::plot_grid(y_tr_plot,y_es_plot,nrow=2)
ggsave("EM估计y概率密度与真实对比.tiff",dpi=600, compression = 'lzw') 

# 计算来源于1的相对概率f与f1_es
f <- f1 / (f0 + f1)
f_es=f1_es / (f0_es + f1_es)

library(pROC)
library(ROCR)
# 将相对概率f进行排序作为阈值
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
# 初始化存储各指标的向量
fpr_f_es <- rep(0, length(threshold_f))
tpr_f_es <- rep(0, length(threshold_f))
for (j in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted_es <- ifelse(f_es >= threshold_f[j], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f_es <- table(predicted_es, Var_ex_flag)
  if(dim(confusion_matrix_f_es)[1]==2)
  {
    # 计算真阳性率、假阳性率和F1值
    tp <- confusion_matrix_f_es[2, 2]
    tn <- confusion_matrix_f_es[1, 1]
    fp <- confusion_matrix_f_es[2, 1]
    fn <- confusion_matrix_f_es[1, 2]
    
    fpr_f_es[j] <- fp / (fp + tn)
    tpr_f_es[j] <- tp / (tp + fn)
  }
  else
  {
    if(predicted_es[j]==1)
    {
      fpr_f_es[j] <- 1
      tpr_f_es[j] <- 1 
    }
    else
    {
      fpr_f_es[j] <- 0
      tpr_f_es[j] <- 0 
    }
  }
  
}

# 使用roc函数创建ROC曲线对象
roc_obj_es <- roc(Var_ex_flag, f_es)
# 计算AUC值
auc_value_es=auc(roc_obj_es)
# 创建一个prediction对象
pred <- prediction( f,Var_ex_flag)
# 计算AUC值
auc_pre <- performance(pred, "auc")@y.values[[1]]
print(paste("auc_pre:", auc_pre))
print(paste("AUC_ES:", auc_value_es))

ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#0073C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Auc = 0.9762 ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)

ggplot(data.frame(fpr_f_es,tpr_f_es))+
  geom_line(aes(fpr_f_es,tpr_f_es),color="#7773C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Auc = 0.9762 ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f_es,y=tpr_f_es,xmin=0,xmax=1,ymin = 0, ymax = tpr_f_es), fill = "lightblue", alpha = 0.3)


roc_obj <- roc(Var_ex_flag, f)
# 计算AUC值
auc_value=auc(roc_obj)
print(paste("AUC:", auc_value))













