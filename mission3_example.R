####
#抛硬币问题后验分布计算
####


# 步骤1：设定先验分布
prior_alpha <- 1  # 先验分布的参数alpha
prior_beta <- 1   # 先验分布的参数beta

# 步骤2：计算似然度
data <- c(rep(1, 8), rep(0, 2))  # 数据集
likelihood <- prod(data)  # 计算似然度（二项分布的概率质量函数）

# 步骤3：计算证据（归一化常数）
evidence <- integrate(function(theta) dbeta(theta, prior_alpha, prior_beta) * likelihood, 0, 1)$value

# 步骤4：计算后验分布
posterior_alpha <- prior_alpha + sum(data)
posterior_beta <- prior_beta + length(data) - sum(data)

# 步骤5：可视化和分析后验分布
theta_values <- seq(0, 1, 0.01)  # 参数theta的取值范围
posterior_density <- dbeta(theta_values, posterior_alpha, posterior_beta)  # 后验分布的概率密度函数

# 绘制后验分布的概率密度图
plot(theta_values, posterior_density, type = "l", xlab = "Theta (Probability of Heads)", ylab = "Posterior Density")

# 添加先验分布的概率密度曲线
prior_density <- dbeta(theta_values, prior_alpha, prior_beta)
lines(theta_values, prior_density, col = "blue")

# 添加真实值Theta=0.5的竖线（用于参考）
abline(v = 0.5, col = "red", lty = 2)

# 输出后验分布的统计指标（均值、中位数、95%置信区间）
posterior_mean <- posterior_alpha / (posterior_alpha + posterior_beta)
posterior_median <- qbeta(0.5, posterior_alpha, posterior_beta)
posterior_interval <- qbeta(c(0.025, 0.975), posterior_alpha, posterior_beta)
cat("Posterior Mean:", posterior_mean, "\n")
cat("Posterior Median:", posterior_median, "\n")
cat("95% Credible Interval:", posterior_interval, "\n")


