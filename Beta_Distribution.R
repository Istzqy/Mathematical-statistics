#利用ggplot2和ggdist库绘制了Alpha参数为2，Beta参数为5的Beta分布的概率密度函数和累积分布函数的图像。
#分别使用dbeta()函数和pbeta()函数计算概率密度函数和累积分布函数的值，并使用ggplot()函数创建图像。
#对于概率密度函数（PDF）图像，横轴表示随机变量取值x，纵轴表示对应的概率密度值。PDF图像在Alpha较小、
#Beta较大的情况下，质量主要集中在0附近。随着Alpha的增加和Beta的减少，分布向右移动，质量逐渐集中在1附近。
#对于累积分布函数（CDF）图像，横轴表示随机变量取值x，纵轴表示对应的累积概率值。
#CDF图像反映了随机变量取值小于等于x的概率。在Beta分布的CDF图像中，初始概率较小，随着x的增加逐渐增大，最终趋近于1。
#以上图像是基于特定的Alpha和Beta参数生成的，你可以根据具体需求修改参数值，观察对应Beta分布的概率密度函数和累积分布函数的变化

# 导入所需库
library(ggplot2)
library(ggdist)

# 设置Alpha和Beta参数
alpha <- 2
beta <- 5

# 创建x轴取值范围
x <- seq(0, 1, length.out = 100)

# 计算PDF和CDF
pdf <- dbeta(x, alpha, beta)
cdf <- pbeta(x, alpha, beta)

# 绘制概率密度函数图像
ggplot(data.frame(x), aes(x = x)) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                color = "blue", size = 1) +
  xlim(0, 1) + ylim(0, 3) +
  xlab("θ") + ylab("Probability Density") 
ggsave("Beta分布PDF.tiff",dpi=600, compression = 'lzw') 
#  ggtitle("Beta Distribution PDF")

# 绘制累积分布函数图像
ggplot(data.frame(x), aes(x = x)) +
  stat_function(fun = pbeta, args = list(shape1 = alpha, shape2 = beta),
                color = "blue", size = 1) +
  xlim(0, 1) + ylim(0, 1) +
  xlab("θ") + ylab("Cumulative Distribution") 
#  ggtitle("Beta Distribution CDF")
ggsave("Beta分布CDF.tiff",dpi=600, compression = 'lzw') 

