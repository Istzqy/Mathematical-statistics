library(openxlsx)
library(mixtools)
library(ggplot2)

# 创建一个新的excel表格
wb <- createWorkbook()
sheet <- addWorksheet(wb, "data")

# 生成患者出院状态数据 y (假设有100个数据)
y <- sample(c(0, 1), 100, replace = TRUE)

# 设置混合高斯分布的参数
mu0 <- 0
sigma0 <- 1
mu1 <- 3
sigma1 <- 2
alpha <- 0.7

# 生成对应状态的符合混合高斯分布的随机数 x
x <- mixtools::rnormmix(n = length(y), lambda = c(1 - alpha, alpha),
                        mu = c(mu0, mu1), sigma = c(sigma0, sigma1))

# 将 y 和 x 写入excel表格中
writeData(wb, sheet, y,  startRow = 1, startCol = 1)
writeData(wb, sheet, x,  startRow = 1, startCol = 2)

# 保存excel表格
saveWorkbook(wb, "patient_data.xlsx")


# 将 x 转换为数据框
data_x <- data.frame(x)

# 绘制 x 的概率密度图
density_plot <- ggplot(data_x, aes(x = x)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Density Plot of x") +
  xlab("x") +
  ylab("Density")

# 显示密度图
print(density_plot)


# 将 y 和 x 转换为数据框
data_y_X <- data.frame(y, x)

# 绘制 x 在给定 y 的情况下各类别的概率密度图
density_plot_by_y <- ggplot(data_y_X, aes(x = x, fill = as.factor(y))) +
  geom_density(color = "darkblue", alpha = 0.7) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  labs(title = "Density Plot of x by y", fill = "y") +
  xlab("x") +
  ylab("Density") +
  theme(legend.title = element_blank())

# 显示密度图
print(density_plot_by_y)



library(pROC)
# 计算阈值
thresholds <- seq(0.05, 0.95, by = 0.05)

# 初始化存储各指标的向量
fpr <- rep(0, length(thresholds))
tpr <- rep(0, length(thresholds))
f1_score <- rep(0, length(thresholds))

# 计算混淆矩阵和各指标
for (i in 1:length(thresholds)) {
  # 根据选择的阈值对样本进行分类
  predicted <- ifelse(f >= thresholds[i], 1, 0)
  
  # 计算混淆矩阵
  confusion_matrix <- table(predicted, y)
  
  # 计算真阳性率、假阳性率和F1值
  tp <- confusion_matrix[2, 2]
  tn <- confusion_matrix[1, 1]
  fp <- confusion_matrix[2, 1]
  fn <- confusion_matrix[1, 2]
  
  fpr[i] <- fp / (fp + tn)
  tpr[i] <- tp / (tp + fn)
  f1_score[i] <- 2 * tp / (2 * tp + fp + fn)
}

# 计算AUC值
auc <- auc(fpr, tpr)

# 绘制ROC曲线
roc_curve <- roc(y, f)
plot(roc_curve, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc = TRUE)

# 输出结果
print(paste("AUC:", auc))
print(paste("F1:", max(f1_score)))














