
###抛硬币问题如何计算似然函数及其概率密度函数
# 创建实验数据集（抛硬币结果）
set.seed(123)
data <- c("H", "H", "T", "T", "H", "H", "H", "T", "H", "T")

# 计算正面出现的频数
head_counts <- sum(data == "H")
# 计算反面出现的频数
tail_counts <- length(data) - head_counts

# 构建二项分布的似然估计函数
likelihood_function <- function(p) {
  likelihood <- (p^4000) * ((1-p)^2000)
  return(likelihood)
}

# 生成一系列的概率值并计算似然估计
probs <- seq(0, 1, by = 0.01)
likelihoods <- sapply(probs, likelihood_function)

# 绘制似然估计的概率密度函数
plot(probs, likelihoods, type = "l", xlab = "Probability", ylab = "Density", main = "Likelihood Estimation - Coin Toss")








