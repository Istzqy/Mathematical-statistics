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
#alpha <- 0.4



# 生成对应状态的符合混合高斯分布的随机数 x
x <- ifelse(Var_ex_flag == 0, mvrnorm(sum(Var_ex_flag == 0), mu = mu0, Sigma = sigma0), 
            mvrnorm(sum(Var_ex_flag == 1), mu = mu1, Sigma = sigma1))


# 将 x 转换为数据框
data_x <- data.frame(x)

# 任务4—1_1 绘制x的概率密度图
ggplot(data_x, aes(x = x)) +
  geom_density( color = "darkblue", alpha = 0.7,linewidth=1) +
#  labs(title = "Density Plot of x") +
  xlab("x") +
  ylab("density")
ggsave("随机数x概率密度分布.tiff",dpi=600, compression = 'lzw') 


# 任务4—1_2 将y和x转换为数据框
data_Var_ex_flag_X <- data.frame(Var_ex_flag, x)
flag=as.factor(Var_ex_flag)
# 绘制 x 在给定 y 的情况下各类别的概率密度图
ggplot(data_Var_ex_flag_X, aes(x = x, fill = flag)) +
  geom_density(color = "darkblue", alpha = 0.6) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
#  labs(title = "Density Plot of x by y", fill = "y") +
  xlab("x") +
  ylab("density") +
  theme(
        legend.text = element_text(),
        legend.position = c(0.85,0.85),
        legend.background =element_blank() )
  

ggsave("随机数x概率密度分布_分类.tiff",dpi=600, compression = 'lzw') 

#分别取出出院状态为0与1的数据
flag1_data <- data_Var_ex_flag_X[data_Var_ex_flag_X$Var_ex_flag == 1, ]
flag0_data <- data_Var_ex_flag_X[data_Var_ex_flag_X$Var_ex_flag == 0, ]

# 绘制flag=1 x 的概率密度图
flag1_data_x=data.frame(flag1_data$x)
colnames(flag1_data_x)=c("x")
ggplot(flag1_data_x, aes(x = x)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Density Plot of x") +
  xlab("x") +
  ylab("Density")

# 绘制flag=0 x 的概率密度图
flag0_data_x=data.frame(flag0_data$x)
colnames(flag0_data_x)=c("x")
ggplot(flag0_data_x, aes(x = x)) +
  geom_density(fill = "lightgreen", color = "darkblue", alpha = 0.7) +
  labs(title = "Density Plot of x") +
  xlab("x") +
  ylab("Density")


library(cowplot)
# 任务4_2
# 计算每个观测值来源于N(μ0, σ0)和N(μ1, σ1)的概率密度
f0 <- dnorm(x, mean = mu0, sd = sigma0)
f1 <- dnorm(x, mean = mu1, sd = sigma1)
# 计算来源于1的相对概率f
f <- f1 / (f0 + f1)
p1=ggplot(data.frame(x,f0,f1,f))+
  geom_line(aes(x,f0),color="darkblue",linewidth =1)+
  annotate("text", x = -3.5, y = Inf-0.05,
           label = "(a)", vjust = 1.5, size = 4.5)
p2=ggplot(data.frame(x,f0,f1,f))+
  geom_line(aes(x,f1),color="#0073C2FF",linewidth =1)+
  annotate("text", x = -3.5, y = Inf-0.05,
           label = "(b)", vjust = 1.5, size = 4.5)
p3=ggplot(data.frame(x,f0,f1,f))+
  geom_line(aes(x,f),color="darkgreen",linewidth =1)+
  annotate("text", x = -3.5, y = Inf-0.05,
           label = "(c)", vjust = 1.5, size = 4.5)
cowplot::plot_grid(p1,p2,p3,nrow=3)
ggsave("观测值x不同来源概率密度分布3合1.tiff",dpi=600, compression = 'lzw') 

library(pROC)

#以阈值为0.5为界
thresholds <- 0.5

# 初始化存储各指标的向量
fpr <- rep(0,1)
tpr <- rep(0,1)
f1_score <- rep(0,1)

# 计算混淆矩阵和各指标
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(f > thresholds, 1, 0)
  # 计算混淆矩阵
  confusion_matrix <- table(predicted, Var_ex_flag)
  
  # 计算真阳性率、假阳性率和F1值
  tp <- confusion_matrix[2, 2]
  tn <- confusion_matrix[1, 1]
  fp <- confusion_matrix[2, 1]
  fn <- confusion_matrix[1, 2]
  
  fpr <- fp / (fp + tn)
  tpr_sen_recall <- tp / (tp + fn)
  precision = tp / (tp + fp)
  specificity = tn / (fp+tn)
  accuracy=(tp+tn)/(tp+fp+tn+fn)
  f1_score  = 2 * precision*tpr_sen_recall / (precision + tpr_sen_recall)
  youden_index = tpr_sen_recall + specificity -1
  
  
  
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


# 使用roc函数创建ROC曲线对象
roc_obj <- roc(Var_ex_flag, f)

# 计算AUC值
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))
#绘制ROC曲线
ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#0073C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Auc = 0.9769 ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)
ggsave("ROC曲线.tiff",dpi=600, compression = 'lzw') 
  







