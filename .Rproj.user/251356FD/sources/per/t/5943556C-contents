###
#23-10-27
#使用 x 与 y 的值，在将数据分为训练样本集和测试样本集的前提下，通过线性判别
#对训练样本集进行模型拟合，后用于测试样本进行预测，并
#给出训练样本集、测试样本集的 ROC 曲线
###
#导入库

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
#以70%样本作为训练集求解
train_prop <- 0.7 # 训练样本集比例
train_indices <- sample(1:nrow(data_new), round(train_prop * nrow(data_new)))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

#线性判别函数参数求解
pi_1=sum(train_data$Var_ex_flag==1)/length(train_data$Var_ex_flag)
pi_0=sum(train_data$Var_ex_flag==0)/length(train_data$Var_ex_flag)

#假设各总体方差相同
#求muk_1
x1=0
for (i in 1:length(train_data$Var_ex_flag)){
  if(train_data$Var_ex_flag[i]==1){
    x1=x1+train_data$x[i]
  }
}
muk_1=x1 / sum(train_data$Var_ex_flag==1)

#求muk_0
x0=0
for (i in 1:length(train_data$Var_ex_flag)){
  if(train_data$Var_ex_flag[i]==0){
    x0=x0+train_data$x[i]
  }
}
muk_0=x0 / sum(train_data$Var_ex_flag==1)

#求sigma_es
i=0
index_0=which(train_data$Var_ex_flag==0)
index_1=which(train_data$Var_ex_flag==1)
sum0=0
sum1=0
for(i in 0:1 ){
  if(i==0)
  {
    for(j in 1:length(index_0)){
      sum0 = sum0+(train_data$x[index_0[j]]-muk_0)*(train_data$x[index_0[j]]-muk_0)
    }
  }
  else{
    for(j in 1:length(index_1)){
      sum1 = sum1+(train_data$x[index_1[j]]-muk_1)*(train_data$x[index_1[j]]-muk_1)
    }
  }
}
sigma_es=(sum0+sum1)/(length(train_data$Var_ex_flag)-2)

#线性判别函数
#log(pi)
train_flag=train_data$Var_ex_flag
tr_desicision0 = rep(0,length(train_data$Var_ex_flag))
tr_desicision1 = rep(0,length(train_data$Var_ex_flag))
#训练集中状态为1的相对概率
f_train = rep(0,length(train_data$Var_ex_flag))
train_result=rep(0,length(train_data$Var_ex_flag))
for(i in 1:length(train_data$Var_ex_flag)){
  tr_desicision0[i]=log(pi_0)+train_data$x[i]*muk_0/(sigma_es*sigma_es)-muk_0*muk_0/(2*sigma_es*sigma_es)
  tr_desicision1[i]=log(pi_1)+train_data$x[i]*muk_1/(sigma_es*sigma_es)-muk_1*muk_1/(2*sigma_es*sigma_es)
  f_train[i] = exp(tr_desicision1[i])/(exp(tr_desicision1[i])+exp(tr_desicision0[i]))
  if(tr_desicision0[i]>tr_desicision1[i]){
    train_result[i] = 0
  }
  else{
    train_result[i] = 1
  }
}
test_flag=test_data$Var_ex_flag
te_desicision0 = rep(0,length(test_data$Var_ex_flag))
te_desicision1 = rep(0,length(test_data$Var_ex_flag))
test_result=rep(0,length(test_data$Var_ex_flag))
#测试集中状态为1的相对概率
f_test=rep(0,length(test_data$Var_ex_flag))
for(i in 1:length(test_data$Var_ex_flag)){
  te_desicision0[i]=log(pi_0)+test_data$x[i]*muk_0/(sigma_es*sigma_es)-muk_0*muk_0/(2*sigma_es*sigma_es)
  te_desicision1[i]=log(pi_1)+test_data$x[i]*muk_1/(sigma_es*sigma_es)-muk_1*muk_1/(2*sigma_es*sigma_es)
  f_test[i] = exp(te_desicision1[i])/(exp(te_desicision1[i])+exp(te_desicision0[i]))
  if(te_desicision0[i]>te_desicision1[i]){
    test_result[i] = 0
  }
  else{
    test_result[i] = 1
  }
}

# 概率阈值
threshold_f = seq(0, 1, by = 0.01)
# 初始化存储各指标的向量
fpr_f <- rep(0, length(threshold_f))
tpr_f <- rep(0, length(threshold_f))
for (i in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(f_test >= threshold_f[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f <- table(predicted, test_flag)
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
library(pROC)
#Auc值计算
#auc_value <- auc(roc(train_flag,f_train))
##测试集
#0.9788 0.9776 0.9729 0.9742 0.9721 0.979 0.9776 0.9804 0.9805 0.9773 /10 =0.977
#测试集ROC曲线
p1=ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#0043C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Test Auc = 0.977 ", vjust = 1.5, size = 5)+
  annotate("text", x = 0.75, y = 0.25,
           label = "(b) ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)

for (i in 1:length(threshold_f)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(f_train >= threshold_f[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix_f <- table(predicted, train_flag)
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
#0.9716 0.9769 0.977 0.9743 0.9773 0.9812 0.979 0.9748 0.9831 0.9782 /10 = 0.9773
#训练集ROC曲线
p2=ggplot(data.frame(fpr_f,tpr_f))+
  geom_line(aes(fpr_f,tpr_f),color="#00C2FF",linewidth =1)+
  xlab("False Positive Rate(FPR)") +
  ylab("True Positive Rate(TPR)")+
  annotate("text", x = 0.25, y = 0.75,
           label = "Train Auc = 0.9773 ", vjust = 1.5, size = 5)+
  annotate("text", x = 0.75, y = 0.25,
           label = "(a) ", vjust = 1.5, size = 5)+
  # 添加水平参考线
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "darkblue",linewidth=0.8)+
  geom_ribbon(aes(x=fpr_f,y=tpr_f,xmin=0,xmax=1,ymin = 0, ymax = tpr_f), fill = "lightblue", alpha = 0.3)


cowplot::plot_grid(p2,p1,nrow=2)
ggsave("LDA估计分布ROC曲线.tiff",dpi=600, compression = 'lzw')

















