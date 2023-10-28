library(openxlsx)
#读取目录下mydata.csv表格中的数据,存储在mydata中
mydata=read.xlsx( "mydata_mean.xlsx")

#取出列名为gender的数据赋给Var_Gender,类型为字符数组.同理其他
Var_Gender=mydata$gender
Var_ex_flag= mydata$hospital_expire_flag
#table()函数,统计所有元素出现的频次
Fre_Gender=table(Var_Gender,exclude = NULL)
Fre_Exflag=table(Var_ex_flag,exclude = NULL)
#with函数，将table()作用在mydata中的gender与hospital_expire_flag
Fre_Gender_Exflag = with(mydata,table(gender,hospital_expire_flag))


######作图
#相关链接https://zhuanlan.zhihu.com/p/370223674
#柱状图绘制参考：https://www.jianshu.com/p/2606339a05e7
#导入做图库
library(ggplot2)
library(patchwork)


p1=ggplot(mydata,aes(x=hospital_expire_flag,fill=gender)) +
  geom_bar(position = "stack")+labs(title = 'position="stack"')

p2=ggplot(mydata,aes(x=gender,fill=hospital_expire_flag)) +
  geom_bar(position = "dodge")+labs(title = 'position="dodge"')

p3=ggplot(mydata,aes(x=gender,fill=hospital_expire_flag)) +
  geom_bar(position = "fill")+labs(title = 'position="fill"')
p1+p2+p3+plot_layout(guides = 'collect')


library(tidyverse)
library(reshape2)

data1=mydata %>% group_by(gender) %>%
  dplyr::summarise(n=n()) %>% as.data.frame()
data1
#创建一个数据框，包含性别、出院状态、各组人数
data2=mydata %>% group_by(gender,hospital_expire_flag) %>%
  dplyr::summarise(n=n()) %>% as.data.frame()
data2
#计算不同性别的死亡比例
ratio = c(data2$n[2]/(data2$n[1]+data2$n[2]),data2$n[2]/(data2$n[1]+data2$n[2]),data2$n[4]/(data2$n[3]+data2$n[4]),data2$n[4]/(data2$n[3]+data2$n[4]))
#保留4位小数，四舍五入
ratio = round(ratio,4) 
#将data2与ratio合并为数据框
data3 = data.frame(data2,ratio)

#纵坐标为频数
#注：position=position_stack()可以将各自的频数与显示堆叠在各自的块上
#https://zhuanlan.zhihu.com/p/27293754
#factor()很重要将原int型变量转换成因子型变量,用于绘图。否则会出现报错在修改颜色时 

#绘制堆叠型柱状图
p4=ggplot(data2,aes(x=gender,y=n,fill=factor(hospital_expire_flag)))+
  geom_bar(position = "stack",stat = "identity",color="white",alpha=0.8)+
  geom_text(aes(label=n),position=position_stack(),vjust=-0.3,hjust=0.5)+
  scale_y_continuous(limits=c(0,4000),
                     breaks = seq(0,4000,500),
                     expand=c(0,0))+
  labs(x='gender',y='Number of people counted')+
  theme_test(base_size = 20)+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'))+
  geom_rect(aes(xmin=0.8,xmax=1,ymin=3500,ymax=3600),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=0.8,xmax=1,ymin=3700,ymax=3800),
            fill='#fec79e',color='#fec79e')+
  annotate(geom='text',x=1.1,y=3570,label='flag=1',size=4.5)+
  annotate(geom='text',x=1.1,y=3770,label='flag=0',size=4.5)+
  #theme_bw() + #白色背景+深灰色网格线
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))  #'#F27369','#00BABF'可修改指定颜色
p4
#将纵坐标频数按比例显示
p5=ggplot(data2,aes(x=gender,y=n,fill=factor(hospital_expire_flag)))+
  geom_bar(position = "fill",stat = "identity")+labs(title = 'position="fill"')+
  guides(fill=guide_legend(reverse=TRUE)) #颠倒图例顺序
p5
#并调用position_dodge2是并排显示,间距可调不会太紧凑
p6=ggplot(data2,aes(x=gender,y=n,fill=factor(hospital_expire_flag)))+
  geom_bar(position = position_dodge2(),stat = "identity")+labs(title = 'position="dodge2"')+
  geom_text(aes(label=n),vjust=-0.5,position = position_dodge2(width = 0.9))
p6
#折线输出测试
p7=ggplot(data3,aes(x=factor(gender),y=ratio,group=1))+
  geom_line(group=1,linetype="dotted")+
  geom_point(size=4, shape=20)+
  geom_text(aes(x=gender,y=ratio,label=ratio),vjust=-0.5,hjust=0.5)
p7
#绘制双Y轴图，先绘制柱状图，再在此基础上绘制点折线图
p8=ggplot(data3,aes(x=gender,y=n,fill=factor(hospital_expire_flag)))+
  geom_bar(position = "stack",stat = "identity",color="white",alpha=0.8)+ #堆栈柱状图
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))+ #给柱状图填充指定颜色
  geom_text(aes(label=n),position=position_stack(),vjust=-0.3,hjust=0.5)+ #添加数据标注并调整位置
  labs(x='gender',y='Number of people counted')+  #设置坐标
  theme_test(base_size = 18)+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'))+
  geom_rect(aes(xmin=0.8,xmax=1,ymin=3500,ymax=3600),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=0.8,xmax=1,ymin=3700,ymax=3800),
            fill='#fec79e',color='#fec79e')+
  annotate(geom='text',x=1.2,y=3570,label='flag=1',size=4.5)+
  annotate(geom='text',x=1.2,y=3770,label='flag=0',size=4.5)+
  #设置双Y轴，对第二个坐标轴进行相应的放大，设置间隔
  scale_y_continuous(limits=c(0,4000),
                     breaks = seq(0,4000,500),
                     expand=c(0,0),
                     sec.axis = sec_axis(~./5000,name = 'ratio',breaks=seq(0,0.8,0.1)))+
  geom_point(data=data3, aes(factor(gender),ratio*5000),size=4)+
  geom_line(data=data3, aes(factor(gender),ratio*5000),linetype=("dotted"),group=1,cex=0.8)+
  geom_text(aes(x=gender,y=ratio*5000,label=ratio),vjust=-0.8,hjust=0.5)+
  scale_color_manual(values = c('#ff8c3e'))
  #theme_bw() + #白色背景+深灰色网格线
  p8
  

