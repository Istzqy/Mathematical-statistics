
###
#23-10-25
#使用 x 与 y 的值，在将数据分为训练样本集和测试样本集的前提下，通过二次判别
#对训练样本集进行模型拟合，后用于测试样本进行预测，并
#给出训练样本集、测试样本集的 ROC 曲线
###导入库
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

# 生成对应状态的符合混合高斯分布的随机数 x
x <- ifelse(Var_ex_flag == 0, mvrnorm(sum(Var_ex_flag == 0), mu = mu0, Sigma = sigma0), 
            mvrnorm(sum(Var_ex_flag == 1), mu = mu1, Sigma = sigma1))
#创建数据框
data_new= tibble(Var_ex_flag, x)

# 分割数据集
train_prop <- 0.7 # 训练样本集比例
train_indices <- sample(1:nrow(data_new), round(train_prop * nrow(data_new)))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

# 线性判别分析
qda_model <- qda(Var_ex_flag ~ x, data = train_data)

# 显示模型结果
qda_model
# 预测
train_pred <- predict(qda_model, newdata = train_data)
train_prob <- train_pred$posterior[, 2] # 获取属于类别1的概率作为预测概率
test_pred <- predict(qda_model, newdata = test_data)
test_prob <- test_pred$posterior[, 2] # 获取属于类别1的概率作为预测概率

#test:  0.9792 0.9800 0.9785 0.9755 0.9749 0.9809 0.9766 0.9818 0.9752 0.9737 /10 = 0.9776
#train: 0.9796 0.9765 0.9788 0.9773 0.9776 0.9736 0.9769 0.9807 0.9792 0.9762 /10 = 0.9776

# 将相对概率f进行排序作为阈值
threshold_f = seq(0, 1, by = 0.01)
# 初始化存储各指标的向量
fpr_f <- rep(0, length(threshold_f))
tpr_f <- rep(0, length(threshold_f))
for (i in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(test_prob >= threshold_f[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f <- table(predicted, test_data$Var_ex_flag)
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

p1=ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#0043C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Test Auc = 0.9776 ", vjust = 1.5, size = 5)+
  annotate("text", x = 0.75, y = 0.25,
           label = "(b) ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)

for (i in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(train_prob >= threshold_f[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f <- table(predicted, train_data$Var_ex_flag)
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
p2=ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#00C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Train Auc = 0.9776 ", vjust = 1.5, size = 5)+
  annotate("text", x = 0.75, y = 0.25,
           label = "(a) ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)

cowplot::plot_grid(p2,p1,nrow=2)
ggsave("QDA算法估计分布ROC曲线.tiff",dpi=600, compression = 'lzw')


















