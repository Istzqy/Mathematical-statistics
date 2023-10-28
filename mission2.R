####
#date:2023-10-15
#function:对不同基线数据下患者出院状态进行卡方检验
####

#导入相关库
library(openxlsx)

#读取目录下mydata_mean.csv表格中的数据,存储在mydata中
mydata_mean=read.xlsx( "mydata_mean.xlsx")

#####性别与出院状态的卡方分析
#取出列名为gender的数据赋给Var_Gender,类型为字符数组.同理其他
Var_Gender=mydata_mean$gender
Var_ex_flag= mydata_mean$hospital_expire_flag
#统计性别与出院状态的频数表
Tab_Gender_Exflag=table(Var_Gender,Var_ex_flag)
#调用卡方分析函数
Result_Gender_Exflag=chisq.test(Tab_Gender_Exflag)
#####性别与出院状态的卡方分析


#####年龄与出院状态的卡方分析
Var_age=floor(mydata_mean$admission_age)
Agegroup=cut(Var_age,breaks=c(0,20,30,40,50,60,70,80,90,100),labels=c(20,30,40,50,60,70,80,90,100),right=TRUE,include.lowest=FALSE)
#统计年龄与出院状态的频数表
Tab_Age_Exflag=table(Agegroup,Var_ex_flag)
#调用卡方分析函数.由于理论计算值中含有小于5的值，利用修正的卡方分析或者fisher检验
ChiResult_Age_Exflag=chisq.test(Tab_Age_Exflag,simulate.p.value = TRUE)
FisResult_Age_Exflag=fisher.test(Tab_Age_Exflag,simulate.p.value = TRUE)
#####年龄与出院状态的卡方分析

#####体重与出院状态的卡方分析
Var_weight=floor(mydata_mean$weight_admit)
weightgroup=cut(Var_weight,breaks=c(0,40,60,80,100,120,140,160,180,Inf),labels=c(40,60,80,100,120,140,160,180,200),right=TRUE,include.lowest=FALSE)
#统计体重与出院状态的频数表
Tab_Weight_Exflag=table(weightgroup,Var_ex_flag)
#调用卡方分析函数
Result_Weight_Exflag=chisq.test(Tab_Weight_Exflag)
#####体重与出院状态的卡方分析


#####身高与出院状态的卡方分析
Var_height=floor(mydata_mean$height)
heightgroup=cut(Var_height,breaks=c(0,150,160,170,180,190,Inf),labels=c(150,160,170,180,190,200),right=TRUE,include.lowest=FALSE)
#统计身高与出院状态的频数表
Tab_height_Exflag=table(heightgroup,Var_ex_flag)
#调用卡方分析函数
Result_height_Exflag=chisq.test(Tab_height_Exflag)
#####身高与出院状态的卡方分析






