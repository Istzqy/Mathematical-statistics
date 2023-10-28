
# 载入mixtools包
library(mixtools)

# 生成符合二元混合高斯分布的随机数
set.seed(123)
x <- c(rnorm(1000, mean = -2, sd = 0.5), rnorm(1000, mean = 2, sd = 0.5))
data_x <- data.frame(x)
# 绘制 x 的概率密度图
ggplot(data_x, aes(x = x)) +
  geom_density( color = "darkblue", alpha = 0.7,linewidth=1) +
  #  labs(title = "Density Plot of x") +
  xlab("x") +
  ylab("density")

# 初始化参数
initial_params <- list(
  "normal" = list(mean = c(-1, 1), var = c(1, 1), p = c(0.5, 0.5)),
  "mix" = list(p = c(0.5, 0.5))
)

# 使用EM算法估计参数
fit <- normalmixEM(x, lambda = initial_params$mix$p, mu = initial_params$normal$mean, sigma = sqrt(initial_params$normal$var))

# 输出估计得到的参数值
estimated_params <- cbind(fit$lambda, fit$mu, sqrt(fit$sigma^2))
rownames(estimated_params) <- c("Component 1", "Component 2")
colnames(estimated_params) <- c("Alpha", "Mean", "Standard Deviation")
print(estimated_params)

# 设置真实参数值
true_params <- cbind(c(0.5, 0.5), c(-2, 2), c(0.5, 0.5))
rownames(true_params) <- c("Component 1", "Component 2")
colnames(true_params) <- c("Alpha", "Mean", "Standard Deviation")
print(true_params)
