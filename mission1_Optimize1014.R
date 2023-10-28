####
#date:2023-10-14
#function:对不同基线数据下患者出院状态的堆叠柱状图
####



#导入相关库
library(openxlsx)

library(ggplot2)
library(patchwork)
library(tidyverse)
library(reshape2)

#读取目录下mydata.csv表格中的数据,存储在mydata中
mydata_mean=read.xlsx( "mydata_mean.xlsx")

#########进行性别与出院死亡率数据合成
#取出列名为gender的数据赋给Var_Gender,类型为字符数组.同理其他
Var_Gender=mydata_mean$gender
Var_ex_flag= mydata_mean$hospital_expire_flag
#合并成数据框
Fre_Gender_Exflag=as.data.frame(table(Var_Gender,Var_ex_flag))
#重命名数据框列名称
colnames(Fre_Gender_Exflag)=c("gender","flag","number")
#因子化flag变量，不把他当成一个数值量
Fre_Gender_Exflag$flag=factor(Fre_Gender_Exflag$flag,levels = c("1","0"))
#计算不同性别的死亡比例
ratio = c(Fre_Gender_Exflag$number[3]/(Fre_Gender_Exflag$number[1]+Fre_Gender_Exflag$number[3]),Fre_Gender_Exflag$number[4]/(Fre_Gender_Exflag$number[2]+Fre_Gender_Exflag$number[4]),Fre_Gender_Exflag$number[3]/(Fre_Gender_Exflag$number[1]+Fre_Gender_Exflag$number[3]),Fre_Gender_Exflag$number[4]/(Fre_Gender_Exflag$number[2]+Fre_Gender_Exflag$number[4]))
#保留4位小数，四舍五入
ratio = round(ratio,4) 
#将data2与ratio合并为数据框
Fre_Gender_Exflag = data.frame(Fre_Gender_Exflag,ratio)




######性别与出院状态关系图开始

g1=ggplot(data = Fre_Gender_Exflag,aes(x=gender,y=number,fill=flag)) +
  geom_bar(position = "stack",stat = "identity",color="white",alpha=0.8)+
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))+ #给柱状图填充指定颜色
  geom_text(aes(label=number),position=position_stack(),vjust=-0.3,hjust=0.5)+ #添加数据标注并调整位置
  labs(x='gender',y='Number of people counted')+  #设置坐标
  theme_test(base_size = 15)+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'))+
  geom_rect(aes(xmin=0.7,xmax=0.9,ymin=3500,ymax=3600),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=0.7,xmax=0.9,ymin=3700,ymax=3800),
            fill='#fec79e',color='#fec79e')+
  geom_rect(aes(xmin=0.6,xmax=1.3,ymin=3400,ymax=3900),#211,211,211
            fill='#D3D3D3',color='#D3D3D3',alpha=0)+
  annotate(geom='text',x=1.1,y=3570,label='flag=0',size=4.5)+
  annotate(geom='text',x=1.1,y=3770,label='flag=1',size=4.5)+
  #设置双Y轴，对第二个坐标轴进行相应的放大，设置间隔
  scale_y_continuous(limits=c(0,4000),
                     breaks = seq(0,4000,500),
                     sec.axis = sec_axis(~./4000,name = 'ratio',breaks=seq(0,1,0.1)))+
  geom_point(data=Fre_Gender_Exflag, aes(factor(gender),ratio*4000),size=4)+
  geom_line(data=Fre_Gender_Exflag, aes(factor(gender),ratio*4000),group=1,cex=0.8)+
  geom_text(aes(x=gender,y=ratio*4000,label=ratio),vjust=-0.8,hjust=0.5)+
  scale_color_manual(values = c('#ff8c3e'))

#输出为tiff格式图片，分辨率600，按lzw进行压缩。
#之所以没有规定大小，由于图中文字导出不随图形大小改变，按照默认大小输出，然后具体按比例缩放可以在其它工具进行如画图，PS中。
ggsave("gender_flag1.tiff",dpi=600, compression = 'lzw') 
dev.off()
######性别与出院状态关系图结束


#########进行年龄与出院死亡率数据合成

Var_age=floor(mydata_mean$admission_age)
Fre_age_Exflag=data.frame(Var_age,mydata_mean$hospital_expire_flag)
Fre_age_Exflag$agegroup=cut(Fre_age_Exflag$Var_age,breaks=c(0,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,100),labels=c(20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95),right=TRUE,include.lowest=FALSE)
#合并成数据框
Fre_agegroup_Exflag=as.data.frame(table(Fre_age_Exflag$agegroup,Fre_age_Exflag$mydata_mean.hospital_expire_flag))
#重命名数据框列名称
colnames(Fre_agegroup_Exflag)=c("Age","flag","number")
#因子化flag变量，不把他当成一个数值量
Fre_agegroup_Exflag$flag=factor(Fre_agegroup_Exflag$flag,levels = c("1","0"))
#去因子化,这样对X轴进行更改可以利用相关函数操作,否则比较麻烦
Fre_agegroup_Exflag$Age=as.numeric(as.character(Fre_agegroup_Exflag$Age)) 
#计算各年龄层与死亡率的关系
age_ratio=rep(1.0,32)
for (i in 1:32){
  if(i<17)
  {
    age_ratio[i]=Fre_agegroup_Exflag$number[i+16]/(Fre_agegroup_Exflag$number[i+16]+Fre_agegroup_Exflag$number[i])
  }
  else
  {
    age_ratio[i]=Fre_agegroup_Exflag$number[i]/(Fre_agegroup_Exflag$number[i-16]+Fre_agegroup_Exflag$number[i])
  }
}
age_ratio = round(age_ratio,2) 
Fre_agegroup_Exflag = data.frame(Fre_agegroup_Exflag,age_ratio)
#########进行年龄与出院死亡率数据合成结束


######年龄与出院状态关系图开始

g2=ggplot(data = Fre_agegroup_Exflag,aes(x=Age,y=number,fill=flag)) +
  geom_bar(position = "stack",stat = "identity",alpha=0.9)+
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))+ #给柱状图填充指定颜色
#  geom_text(aes(label=number),position=position_stack(),vjust=-0.3,hjust=0.5)+ #添加数据标注并调整位置
  labs(x='age',y='Number of people counted')+  #设置坐标
  theme_test(base_size = 20)+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'),
        axis.title.y.right =element_text(vjust=2) )+
  geom_rect(aes(xmin=22,xmax=28,ymin=660,ymax=680),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=22,xmax=28,ymin=700,ymax=720),
            fill='#fec79e',color='#fec79e')+
  geom_rect(aes(xmin=20,xmax=45,ymin=630,ymax=745),#211,211,211
             fill='#D3D3D3',color='#D3D3D3',alpha=0)+
  annotate(geom='text',x=36,y=675,label='flag=0',size=4.5)+
  annotate(geom='text',x=36,y=715,label='flag=1',size=4.5)+
  scale_x_continuous(limits = c(15,100),breaks = seq(20,100,10))+
  # xlim(10,100)+
  #scale_x_discrete(limits = c("20", "25 ","30", "35 ","40", "45 ","50", "55 ","60", "65 ","70", " 75","80", "85 ","90", " 95"))
  #设置双Y轴，对第二个坐标轴进行相应的放大，设置间隔
   scale_y_continuous(limits=c(0,800),
                      breaks = seq(0,800,200),
                      #expand=c(0,0), #坐标轴不留空白
                      sec.axis = sec_axis(~./1000,name = 'ratio',breaks=seq(0,0.8,0.1)))+
   geom_point(data=Fre_agegroup_Exflag, aes(Age,age_ratio*1000),size=2)+
   geom_line(data=Fre_agegroup_Exflag, aes(Age,age_ratio*1000),cex=1)+
   geom_text(aes(x=Age,y=age_ratio*1000,label=age_ratio),size=3.5,vjust=-2,hjust=0.6)

#输出为tiff格式图片，分辨率600，按lzw进行压缩。
#之所以没有规定大小，由于图中文字导出不随图形大小改变，按照默认大小输出，然后具体按比例缩放可以在其它工具进行如画图，PS中。
ggsave("age_flag.tiff",dpi=600, compression = 'lzw') 
dev.off()
######性别与出院状态关系图结束


####体重与出院关系图处理
Var_weight=floor(mydata_mean$weight_admit)
Fre_weight_Exflag=data.frame(Var_weight,flag=mydata_mean$hospital_expire_flag)
Fre_weight_Exflag$weightgroup=cut(Fre_weight_Exflag$Var_weight,breaks=c(0,60,80,100,120,140,160,180,Inf),labels=c(60,80,100,120,140,160,180,200),right=TRUE,include.lowest=FALSE)
#合并成数据框
Fre_weightgroup_Exflag=as.data.frame(table(Fre_weight_Exflag$weightgroup,Fre_weight_Exflag$flag))
#重命名数据框列名称
colnames(Fre_weightgroup_Exflag)=c("Weight","flag","number")
#因子化flag变量，不把他当成一个数值量
Fre_weightgroup_Exflag$flag=factor(Fre_weightgroup_Exflag$flag,levels = c("1","0"))
#去因子化,这样对X轴进行更改可以利用相关函数操作,否则比较麻烦
Fre_weightgroup_Exflag$Weight=as.numeric(as.character(Fre_weightgroup_Exflag$Weight)) 
#计算各年龄层与死亡率的关系
weight_ratio=rep(1.0,length(Fre_weightgroup_Exflag$Weight))
for (i in 1:length(Fre_weightgroup_Exflag$Weight)){
  if(i<9)
  {
    weight_ratio[i]=Fre_weightgroup_Exflag$number[i+8]/(Fre_weightgroup_Exflag$number[i+8]+Fre_weightgroup_Exflag$number[i])
  }
  else
  {
    weight_ratio[i]=Fre_weightgroup_Exflag$number[i]/(Fre_weightgroup_Exflag$number[i-8]+Fre_weightgroup_Exflag$number[i])
  }
}
weight_ratio = round(weight_ratio,2) 
Fre_weightgroup_Exflag = data.frame(Fre_weightgroup_Exflag,weight_ratio)
#########进行年龄与出院死亡率数据合成结束

######体重与出院状态关系图开始

g3=ggplot(data = Fre_weightgroup_Exflag,aes(x=Weight,y=number,fill=flag)) +
  geom_bar(position = "stack",stat = "identity",alpha=0.9)+
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))+ #给柱状图填充指定颜色
  #  geom_text(aes(label=number),position=position_stack(),vjust=-0.3,hjust=0.5)+ #添加数据标注并调整位置
  labs(x='weight',y='Number of people counted')+  #设置坐标
  theme_test(base_size = 20)+
  #theme_bw()+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'),
        axis.title.y.right =element_text(vjust=2) )+
  geom_rect(aes(xmin=155,xmax=166,ymin=2580,ymax=2680),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=155,xmax=166,ymin=2770,ymax=2870),
            fill='#fec79e',color='#fec79e')+
  geom_rect(aes(xmin=150,xmax=19,ymin=2480,ymax=2970),#211,211,211
            fill='#D3D3D3',color='#D3D3D3',alpha=0)+
  annotate(geom='text',x=180,y=2630,label='flag=0',size=4.5)+
  annotate(geom='text',x=180,y=2820,label='flag=1',size=4.5)+
  scale_x_continuous(limits = c(50,210),breaks = seq(60,200,20))+
  # # xlim(10,100)+
  #scale_x_discrete(limits = c("60", "80 ","100", " ","40", "45 ","50", "55 ","60", "65 ","70", " 75","80", "85 ","90", " 95"))
  # #设置双Y轴，对第二个坐标轴进行相应的放大，设置间隔
  # 
  scale_y_continuous(limits=c(0,3000),
                      breaks = seq(0,3000,500),
                      #expand=c(0,0), #坐标轴不留空白
                      sec.axis = sec_axis(~./6000,name = 'ratio',breaks=seq(0,0.5,0.1)))+
  geom_point(data=Fre_weightgroup_Exflag, aes(Weight,weight_ratio*6000),size=2)+
  geom_line(data=Fre_weightgroup_Exflag, aes(Weight,weight_ratio*6000),cex=1)+
  geom_text(aes(x=Weight,y=weight_ratio*6000,label=weight_ratio),size=3.5,vjust=-2,hjust=0.6)

#输出为tiff格式图片，分辨率600，按lzw进行压缩。
#之所以没有规定大小，由于图中文字导出不随图形大小改变，按照默认大小输出，然后具体按比例缩放可以在其它工具进行如画图，PS中。
ggsave("weight_flag.tiff",dpi=600, compression = 'lzw') 
dev.off()

######体重与出院状态关系图结束



####身高与出院关系图处理
Var_height=floor(mydata_mean$height)
Fre_height_Exflag=data.frame(Var_height,flag=mydata_mean$hospital_expire_flag)
Fre_height_Exflag$heightgroup=cut(Fre_height_Exflag$Var_height,breaks=c(0,150,155,160,165,170,175,180,185,190,195,Inf),labels=c(150,155,160,165,170,175,180,185,190,195,200),right=TRUE,include.lowest=FALSE)
#合并成数据框
Fre_heightgroup_Exflag=as.data.frame(table(Fre_height_Exflag$heightgroup,Fre_height_Exflag$flag))
#重命名数据框列名称
colnames(Fre_heightgroup_Exflag)=c("Height","flag","number")
#因子化flag变量，不把他当成一个数值量
Fre_heightgroup_Exflag$flag=factor(Fre_heightgroup_Exflag$flag,levels = c("1","0"))
#去因子化,这样对X轴进行更改可以利用相关函数操作,否则比较麻烦
Fre_heightgroup_Exflag$Height=as.numeric(as.character(Fre_heightgroup_Exflag$Height)) 
#计算各年龄层与死亡率的关系
height_ratio=rep(1.0,length(Fre_heightgroup_Exflag$Height))
for (i in 1:length(Fre_heightgroup_Exflag$Height)){
  if(i<12)
  {
    height_ratio[i]=Fre_heightgroup_Exflag$number[i+11]/(Fre_heightgroup_Exflag$number[i+11]+Fre_heightgroup_Exflag$number[i])
  }
  else
  {
    height_ratio[i]=Fre_heightgroup_Exflag$number[i]/(Fre_heightgroup_Exflag$number[i-11]+Fre_heightgroup_Exflag$number[i])
  }
}
height_ratio = round(height_ratio,2) 
Fre_heightgroup_Exflag = data.frame(Fre_heightgroup_Exflag,height_ratio)
#########进行身高与出院死亡率数据合成结束

######身高与出院状态关系图开始

g4=ggplot(data = Fre_heightgroup_Exflag,aes(x=Height,y=number,fill=flag)) +
  geom_bar(position = "stack",stat = "identity",alpha=0.9)+
  scale_fill_manual(values=c('#fec79e','#8ec4cb'))+ #给柱状图填充指定颜色
  #  geom_text(aes(label=number),position=position_stack(),vjust=-0.3,hjust=0.5)+ #添加数据标注并调整位置
  labs(x='height',y='Number of people counted')+  #设置坐标
  theme_test(base_size = 20)+
  #theme_bw()+
  theme(legend.position = 'none',
        panel.border = element_rect(size=2,fill = 'transparent'),
        axis.text = element_text(color='black'),
        axis.title.y.right =element_text(vjust=2) )+
  geom_rect(aes(xmin=185,xmax=190,ymin=1190,ymax=1220),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=185,xmax=190,ymin=1270,ymax=1300),
            fill='#fec79e',color='#fec79e')+
  geom_rect(aes(xmin=182,xmax=203,ymin=1140,ymax=1340),#211,211,211
            fill='#D3D3D3',color='#D3D3D3',alpha=0)+
  annotate(geom='text',x=196,y=1210,label='flag=0',size=4.5)+
  annotate(geom='text',x=196,y=1290,label='flag=1',size=4.5)+
  scale_x_continuous(limits = c(145,205),breaks = seq(150,200,10))+
  # # xlim(10,100)+
  #scale_x_discrete(limits = c("60", "80 ","100", " ","40", "45 ","50", "55 ","60", "65 ","70", " 75","80", "85 ","90", " 95"))
  # #设置双Y轴，对第二个坐标轴进行相应的放大，设置间隔
  # 
   scale_y_continuous(limits=c(0,1350),
                      breaks = seq(0,1250,250),
                      #expand=c(0,0), #坐标轴不留空白
                     sec.axis = sec_axis(~./2700,name = 'ratio',breaks=seq(0,0.5,0.1)))+
   geom_point(data=Fre_heightgroup_Exflag, aes(Height,height_ratio*2600),size=2)+
   geom_line(data=Fre_heightgroup_Exflag, aes(Height,height_ratio*2600),cex=1)+
   geom_text(aes(x=Height,y=height_ratio*2600,label=height_ratio),size=3.5,vjust=-2,hjust=0.6)

#输出为tiff格式图片，分辨率600，按lzw进行压缩。
#之所以没有规定大小，由于图中文字导出不随图形大小改变，按照默认大小输出，然后具体按比例缩放可以在其它工具进行如画图，PS中。
ggsave("height_flag.tiff",dpi=600, compression = 'lzw') 
dev.off()

######身高与出院状态关系图结束
























