


crma=matrix(nrow = 5,ncol=1)  #行数不变，ncol=模拟次数
ccrma=matrix(nrow = 5,ncol=1)  #行数不变，ncol=模拟次数
ccrmcs=matrix(nrow = 15,ncol=1)   #nrow = （3+2*(变量和常数个数)）+（3+2*(区间变量和常数个数)） ，所以每次先跑一遍带常数项的回归，若p>0.05，将常数项去掉，ncol=模拟次数，
num_lucrm=matrix(nrow = 1,ncol=1)
num_luccrm=matrix(nrow = 1,ncol=1)
lucrm=matrix(nrow = 1,ncol=1)
luccrm=matrix(nrow = 1,ncol=1)
rcrma=matrix(nrow = 5,ncol=1)
mtsa=matrix(nrow = 5,ncol=1000)
num_lurcrm=matrix(nrow = 1,ncol=1)
lurcrm=matrix(nrow = 1,ncol=1)
s=1
#for (k in c(1:21,23:1001)){
#s=s+1
#set.seed(k)
#n<-1000 #100 #500 #1000
z=0
#center
library(openxlsx)
l = read.xlsx("BJPM2.5ccrm.xlsx",1)
u= read.xlsx("BJPM2.5ccrm.xlsx",2)
l
u
n<-nrow(l)
y_l<-l[,2]
x_l<-l[,3:9]
colnames(x_l)<-paste("x_l",0:6,sep="")
x_l
y_u<-u[,2]
x_u<-u[,3:9]
colnames(x_u)<-paste("x_u",0:6,sep="")
x_u
x_c<-(x_l+x_u)/2
colnames(x_c)<-paste("x_c",0:6,sep="")
x_r<-(x_u-x_l)/2
colnames(x_r)<-paste("x_r",0:6,sep="")
x_c
x_r0<-matrix(1,n,1)
x_r[,1]<-x_r0  #令x_r的常数项为1
x_r
y_c<-(y_l+y_u)/2
y_r<-(y_u-y_l)/2

x<-matrix(c(x_l[,1],x_l[,2],x_u[,2],x_l[,3],x_u[,3],x_l[,4],x_l[,5],x_l[,6],x_l[,7]),n,9)
colnames(x)<-paste("x",0:8,sep="")
y_deta<-y_u-y_l
# x_c<-matrix(0,n,5)
# for(i in 1:n){x_c[i,]=c(1,runif(1,1,10),runif(1,10,20),runif(1,20,30),rbinom(1,1,0.7))}
# for(j in 1:5){x_cj<-x_c[,j]}
# colnames(x_c)<-paste("x_c",0:4,sep="")
# b_c<-c(0,5,-1.5,-3.2,5)
# y_c<-x_c%*%b_c+rnorm(n,0,3)#3,9
# 
# #range
# x_r<-matrix(0,n,5)
# for(i in 1:n){x_r[i,]=c(1,runif(1,1,3),runif(1,1,3),runif(1,1,3),0)} #高可变范围runif(1,10,30)
# colnames(x_r)<-paste("x_r",0:4,sep="")
# b_r<-c(0,1.5,-2,1.5,0)
# y_r<-x_r%*%b_r+rnorm(n,0,1)#1,3
#lower bound
# x_l<-x_c-x_r
# colnames(x_l)<-paste("x_l",0:4,sep="")
# y_l<-y_c-y_r
# #upper bound
# x_u<-x_c+x_r
# colnames(x_u)<-paste("x_u",0:4,sep="")
# y_u<-y_c+y_r
# plot(1:n,y_l-y_u,xlab='数据集',ylab='数据集中响应变量下界与上界之差',pch=8)#25要改
# abline(h=0,lwd=2,col="black")#添加一条水平直线y=3，线宽为4，颜色蓝色


# for(i in 1:n){
#   if(y_l[i,]-y_u[i,]>0){
#     zj<-y_u[i,]
#     y_u[i,]<-y_l[i,]
#     y_l[i,]<-zj
#   }
# }
#生成数据集I
ds4<-data.frame(x_c,x_r,y_c,y_r,x_l,x_u,y_l,y_u,x,y_deta)
yll<-min(y_l)
yuu<-max(y_u)
yll

rc<-qr(x_c)$rank
rr<-qr(x_r)$rank

#######################


###########################

#     attach(ds4)
#     g1<-min(x_l[,2])-5
#     h1<-max(x_u[,2])+5
#     g2<-min(x_l[,3])-0.2
#     h2<-max(x_u[,3])+0.2
#     g3<-min(x_l[,4])-2
#     h3<-max(x_u[,4])+2
#     interplot<-function(a,b,aa,bb,number,g,h){plot(a,b,xlab=aa,ylab=bb,xlim = c(g,h),ylim = c(yll-30,yuu+30),col="white")
#       intpoint1<-function(i){
#         low<-paste('x_l',number,sep='')
#         up<-paste('x_u',number,sep='')
#         segments(get(low)[i],y_l[i],get(low)[i],y_u[i])
#         segments(get(low)[i],y_l[i],get(up)[i],y_l[i])
#         segments(get(up)[i],y_u[i],get(up)[i],y_l[i])
#         segments(get(up)[i],y_u[i],get(low)[i],y_u[i])
#       }
#       sapply(1:n,intpoint1)
#     }
# 
#      interplot(x_c1,y_c,'x_1','y','1',g1,h1)
#      interplot(x_c2,y_c,'x_2','y','2',g2,h2)
#      interplot(x_c3,y_c,'x_3','y','3',g3,h3)
#      detach(ds4)
# 
# #
#      # #画图二分类变量
#      interplot<-function(a,b,aa,bb,number){plot(a,b,xlab=aa,ylab=bb,xlim =c(-0.5,1.5) ,ylim = c(yll-5,yuu+5),col="white")
#        intpoint1<-function(i,j){
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]-0.05,y_u[i])
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]-0.05,y_u[i])
#        }
#        sapply(1:n,intpoint1,5)  #sapply(i,fun,j)，j=3表示第三列，即x2
#      }
# 
#      interplot(x_c[,5],y_c,'x_4','y','4')#
#      
#      # #画图二分类变量
#      interplot<-function(a,b,aa,bb,number){plot(a,b,xlab=aa,ylab=bb,xlim =c(-0.5,1.5) ,ylim = c(yll-5,yuu+5),col="white")
#        intpoint1<-function(i,j){
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]-0.05,y_u[i])
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]-0.05,y_u[i])
#        }
#        sapply(1:n,intpoint1,6)  #sapply(i,fun,j)，j=3表示第三列，即x2
#      }
#      
#      interplot(x_c[,6],y_c,'x_5','y','5')#
#      
#      # #画图二分类变量
#      interplot<-function(a,b,aa,bb,number){plot(a,b,xlab=aa,ylab=bb,xlim =c(-0.5,1.5) ,ylim = c(yll-5,yuu+5),col="white")
#        intpoint1<-function(i,j){
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]-0.05,y_u[i])
#          segments(x_c[i,j]-0.05,y_l[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]+0.05,y_l[i])
#          segments(x_c[i,j]+0.05,y_u[i],x_c[i,j]-0.05,y_u[i])
#        }
#        sapply(1:n,intpoint1,7)  #sapply(i,fun,j)，j=3表示第三列，即x2
#      }
#      
#      interplot(x_c[,7],y_c,'x_6','y','6')#
# 
######## 模拟数据集回归(以数据集 IV 为例，其他类似)
###基本函数定义

MMER <- function(y_l,y_u,YL_hat,YU_hat){
  0.5*mean(abs((y_l-YL_hat)/YL_hat)+abs((y_u-YU_hat)/YU_hat))
}
RMSE<- function(y,Y_hat){
  sqrt(mean((y-Y_hat)^2))
}
cor<- function(y,Y_hat){
  cov(y,Y_hat)/(sd(y)*sd(Y_hat))
}

###数据集 IV
##Seperating data into training set and testing set
index <- 1:nrow(ds4)
#set.seed(k)
index.training <- sample(index,length(index)*0.75,replace = FALSE)  ###要确定下0.75
index.testing <- setdiff(index,index.training)
ds4.training <- ds4[index.training,]
ds4.testing <- ds4[index.testing,]


lmc<-summary(lm(y_c~x_c1+x_c2+x_c3+x_c4+x_c5+x_c6,data= ds4.training))  ###要一起改变量
lmc<-summary(lm(y_c~x_c1+x_c2+x_c4+x_c5+x_c6,data= ds4.training))  ###要一起改变量
lmc
beta_c_3<-lmc$coefficients[,1]  ###要一起改变量个数
lmr<-summary(lm(y_r~x_r1+x_r2+x_r3,data= ds4.training))  ###要一起改变量
lmr<-summary(lm(y_r~x_r1+x_r2,data= ds4.training))  ###要一起改变量
lmr<-summary(lm(y_r~x_r1+x_r2-1,data= ds4.training))  ###要一起改变量
beta_r_3<-lmr$coefficients[,1]   ###要一起改变量
y_l_CRM4<-with(data=ds4.testing,y_l)
y_u_CRM4<-with(data=ds4.testing,y_u)
Yc_hat_CRM4 <- as.matrix(ds4.testing[,c('x_c0','x_c1','x_c2','x_c4','x_c5','x_c6')])%*%beta_c_3 
x_rlm<-c('x_r1','x_r2')
Yr_hat_CRM4 <- as.matrix(ds4.testing[,x_rlm])%*%beta_r_3
YL_hat_CRM4 <-Yc_hat_CRM4-Yr_hat_CRM4
YU_hat_CRM4 <-Yc_hat_CRM4+Yr_hat_CRM4
as.matrix(ds4.testing[,'y_l'])

num_lucrm[,s]=sum(YL_hat_CRM4-YU_hat_CRM4>0)
lucrm<-num_lucrm/(0.25*n)
m<-0.25*n
lul<-min(YL_hat_CRM4-YU_hat_CRM4)
luu<-max(YL_hat_CRM4-YU_hat_CRM4)
#sum(YL_hat_CRM4-YU_hat_CRM4>0)/(0.25*n) #仅一次的
# plot(c(1:m),YL_hat_CRM4-YU_hat_CRM4,ylim =c(lul-1,luu+1),xlab='MCRM',ylab='Difference Value',pch=8)#25要改
# abline(h=0,lwd=2,col="black")#添加一条水平直线y=3，线宽为4，颜色蓝色
#评价指标
MMER_CRM4 <- with(data = ds4.testing,MMER(y_l_CRM4,y_u_CRM4,YL_hat_CRM4,YU_hat_CRM4))
RMSE_L_CRM4 <- with(data = ds4.testing,RMSE(y_l_CRM4,YL_hat_CRM4))
RMSE_U_CRM4 <- with(data = ds4.testing,RMSE(y_u_CRM4,YU_hat_CRM4))
cor_L_CRM4 <- with(data = ds4.testing,cor(y_l_CRM4,YL_hat_CRM4))
cor_U_CRM4 <- with(data = ds4.testing,cor(y_u_CRM4,YU_hat_CRM4))
crm_m <-c(MMER_CRM4,RMSE_L_CRM4,RMSE_U_CRM4,cor_L_CRM4,cor_U_CRM4)
crma[,s]<-crm_m




# y_c_CRM4c<-with(data=ds4.testing[101:150,],y_c)
# y_r_CRM4r<-with(data=ds4.testing[101:150,],y_r)
# plot(c(1:50),y_c_CRM4c,xlab='MCCRM',ylab='Interval Center',type="o",pch=20,col="red")
# lines(c(1:50),Yc_hat_CRM4[101:150,],type="o",pch=17,col="blue",lty=2)
# 
# plot(c(1:50),y_r_CRM4r,xlab='MCCRM',ylab='Interval Radius',type="o",pch=20,col="red")
# lines(c(1:50),Yr_hat_CRM4[101:150,],type="o",pch=17,col="blue",lty=2)
# 
# y_l_CRM4c<-with(data=ds4.testing[101:150,],y_l)
# y_u_CRM4r<-with(data=ds4.testing[101:150,],y_u)
# plot(c(1:50),y_l_CRM4c,xlab='MCCRM',ylab='Interval Lower Bound',type="o",pch=20,col="red")
# lines(c(1:50),YL_hat_CRM4[101:150,],type="o",pch=17,col="blue",lty=2)
# 
# plot(c(1:50),y_u_CRM4r,xlab='MCCRM',ylab='Interval Upper Bound',type="o",pch=20,col="red")
# lines(c(1:50),YU_hat_CRM4[101:150,],type="o",pch=17,col="blue",lty=2)


Yc_hat_RCRM4<-Yc_hat_CRM4
x_rrcrm<-c('x_r0','x_r1','x_r2','x_r3')
A<-as.matrix(ds4.training[,x_rrcrm])
b<-as.vector(ds4.training[,'y_r'])
library(nnls)
beta_r_rcrm<-nnls(A,b)$x###nnls的系数估计
y_l_RCRM4<-with(data=ds4.testing,y_l)
y_u_RCRM4<-with(data=ds4.testing,y_u)
Yr_hat_RCRM4 <- as.matrix(ds4.testing[,x_rrcrm])%*%beta_r_rcrm
YL_hat_RCRM4 <-Yc_hat_RCRM4-Yr_hat_RCRM4
YU_hat_RCRM4 <-Yc_hat_RCRM4+Yr_hat_RCRM4

num_lurcrm[,s]=sum(YL_hat_RCRM4-YU_hat_RCRM4>0)
lurcrm<-num_lurcrm/(0.25*n)
# plot(c(1:m),YL_hat_RCRM4-YU_hat_RCRM4,ylim =c(lul-1,luu+1),xlab='MRCRM',ylab='Difference Value',pch=8)#25要改
# abline(h=0,lwd=2,col="black")#添加一条水平直线y=3，线宽为4，颜色蓝色
MMER_RCRM4 <- with(data = ds4.testing,MMER(y_l_RCRM4,y_u_RCRM4,YL_hat_RCRM4,YU_hat_RCRM4))
RMSE_L_RCRM4 <- with(data = ds4.testing,RMSE(y_l_RCRM4,YL_hat_RCRM4))
RMSE_U_RCRM4 <- with(data = ds4.testing,RMSE(y_u_RCRM4,YU_hat_RCRM4))
cor_L_RCRM4 <- with(data = ds4.testing,cor(y_l_RCRM4,YL_hat_RCRM4))
cor_U_RCRM4 <- with(data = ds4.testing,cor(y_u_RCRM4,YU_hat_RCRM4))
rcrm_m <-c(MMER_RCRM4,RMSE_L_RCRM4,RMSE_U_RCRM4,cor_L_RCRM4,cor_U_RCRM4)
rcrma[,s]<-rcrm_m





Yc_hat_CCRM4<-Yc_hat_CRM4
x_rnnls<-x_rlm
A<-as.matrix(ds4.training[,x_rnnls])
b<-as.vector(ds4.training[,'y_r'])
library(nnls)
beta_r_ccrm<-nnls(A,b)$x###nnls的系数估计
y_l_CCRM4<-with(data=ds4.testing,y_l)
y_u_CCRM4<-with(data=ds4.testing,y_u)
Yr_hat_CCRM4 <- as.matrix(ds4.testing[,x_rnnls])%*%beta_r_ccrm
YL_hat_CCRM4 <-Yc_hat_CCRM4-Yr_hat_CCRM4
YU_hat_CCRM4 <-Yc_hat_CCRM4+Yr_hat_CCRM4

num_luccrm[,s]=sum(YL_hat_CCRM4-YU_hat_CCRM4>0)
luccrm<-num_luccrm/(0.25*n)
# plot(c(1:m),YL_hat_CCRM4-YU_hat_CCRM4,ylim =c(lul-1,luu+1),xlab='MCCRM',ylab='Difference Value',pch=8)#25要改
# abline(h=0,lwd=2,col="black")#添加一条水平直线y=3，线宽为4，颜色蓝色
MMER_CCRM4 <- with(data = ds4.testing,MMER(y_l_CCRM4,y_u_CCRM4,YL_hat_CCRM4,YU_hat_CCRM4))
RMSE_L_CCRM4 <- with(data = ds4.testing,RMSE(y_l_CCRM4,YL_hat_CCRM4))
RMSE_U_CCRM4 <- with(data = ds4.testing,RMSE(y_u_CCRM4,YU_hat_CCRM4))
cor_L_CCRM4 <- with(data = ds4.testing,cor(y_l_CCRM4,YL_hat_CCRM4))
cor_U_CCRM4 <- with(data = ds4.testing,cor(y_u_CCRM4,YU_hat_CCRM4))
ccrm_m <-c(MMER_CCRM4,RMSE_L_CCRM4,RMSE_U_CCRM4,cor_L_CCRM4,cor_U_CCRM4)
ccrma[,s]<-ccrm_m


lm_l <- summary(lm(y_l~x1+x2+x3+x4+x5+x6+x7+x8, data= ds4.training))
lm_u <- summary(lm(y_u~x1+x2+x3+x4+x5+x6+x7+x8, data= ds4.training))
b_deta_hat<-lm_u$coefficients[,1]-lm_l$coefficients[,1]
y_detatr<-as.matrix(ds4.training[,c('y_deta')])
x_tr<-as.matrix(ds4.training[,c('x0','x1','x2','x3','x4','x5','x6','x7','x8')])
y_ltr<-as.matrix(ds4.training[,c('y_l')])
y_utr<-as.matrix(ds4.training[,c('y_u')])
# x_ltr<-as.matrix(ds4.training[,c('x_l')])
# x_utr<-as.matrix(ds4.training[,c('x_u')])
# Itr<-matrix(I[1:0.75*n,1],0.75*n,1)

e_deta_hat<-y_detatr-x_tr%*% b_deta_hat
sigma_m_hat<-sqrt(var(e_deta_hat))
start_params <- c(b_deta0=b_deta_hat[1],b_detal=b_deta_hat[2],b_detau=b_deta_hat[3],b_deta2=b_deta_hat[4],b_deta3=b_deta_hat[5],b_deta4=b_deta_hat[6],b_deta5=b_deta_hat[7],b_deta6=b_deta_hat[8],b_deta7=b_deta_hat[9], sigma_m=sigma_m_hat)
normx<-function(theta){
  b_deta0<-theta[1]
  b_detal<-theta[2]
  b_detau<-theta[3]
  b_deta2<-theta[4]
  b_deta3<-theta[5]
  b_deta4<-theta[6]
  b_deta5<-theta[7]
  b_deta6<-theta[8]
  b_deta7<-theta[9]
  sigma_m<-theta[10]
  if (sigma_m <= 0){
    return(NA)
  }  
  logL=(1/(0.75*n))*sum(log(dnorm((y_detatr/sigma_m+(-b_deta0*x_tr[,1]-b_detal*x_tr[,2]-b_detau*x_tr[,3]-b_deta2*x_tr[,4]-b_deta3*x_tr[,5]-b_deta4*x_tr[,6]-b_deta5*x_tr[,7]-b_deta6*x_tr[,8]-b_deta7*x_tr[,9])/sigma_m), 0,1) / (sigma_m*(1-pnorm((-b_deta0*x_tr[,1]-b_detal*x_tr[,2]-b_detau*x_tr[,3]-b_deta2*x_tr[,4]-b_deta3*x_tr[,5]-b_deta4*x_tr[,6]-b_deta5*x_tr[,7]-b_deta6*x_tr[,8]-b_deta7*x_tr[,9])/sigma_m)))))
  return(logL)
}
library(maxLik)
result<-maxLik(normx,start= start_params, method = "BFGS") #{NR, BHHH, BFGS, NM, SANN, BFGSR, CG}
#summary(result)
b_deta0_max<-summary(result)$estimate[1,1]
b_detal_max<-summary(result)$estimate[2,1]
b_detau_max<-summary(result)$estimate[3,1]
b_deta2_max<-summary(result)$estimate[4,1]
b_deta3_max<-summary(result)$estimate[5,1]
b_deta4_max<-summary(result)$estimate[6,1]
b_deta5_max<-summary(result)$estimate[7,1]
b_deta6_max<-summary(result)$estimate[8,1]
b_deta7_max<-summary(result)$estimate[9,1]
sigma_m_max<-summary(result)$estimate[10,1]
G_deta_hattr<-b_deta0_max*x_tr[,1]+b_detal_max*x_tr[,2]+b_detau_max*x_tr[,3]+b_deta2_max*x_tr[,4]+b_deta3_max*x_tr[,5]+b_deta4_max*x_tr[,6]+b_deta5_max*x_tr[,7]+b_deta6_max*x_tr[,8]+b_deta7_max*x_tr[,9]
tautr<-dnorm(G_deta_hattr/sigma_m_max)/pnorm(G_deta_hattr/sigma_m_max)
C_l<--0.2236
C_u<-0.2236
b_l_hat2<-summary(lm(y_ltr~x_tr[,1]+x_tr[,2]+x_tr[,3]+x_tr[,4]+x_tr[,5]+x_tr[,6]+x_tr[,7]+x_tr[,8]+x_tr[,9]+tautr))$coefficients[,1]
b_u_hat2<-summary(lm(y_utr~x_tr[,1]+x_tr[,2]+x_tr[,3]+x_tr[,4]+x_tr[,5]+x_tr[,6]+x_tr[,7]+x_tr[,8]+x_tr[,9]+tautr))$coefficients[,1]
# b_l_hatols<-summary(lm(y_ltr~x_tr[,1]+x_tr[,2]+x_tr[,3]+x_tr[,4]+x_tr[,5]+x_tr[,6]+x_tr[,7]))$coefficients[,1]
# b_u_hatols<-summary(lm(y_utr~x_tr[,1]+x_tr[,2]+x_tr[,3]+x_tr[,4]+x_tr[,5]+x_tr[,6]+x_tr[,7]))$coefficients[,1]
y_detatest<-as.matrix(ds4.testing[,c('y_deta')])
x_test<-as.matrix(ds4.testing[,c('x0','x1','x2','x3','x4','x5','x6','x7','x8')])
y_ltest<-as.matrix(ds4.testing[,c('y_l')])
y_utest<-as.matrix(ds4.testing[,c('y_u')])
G_deta_hattest<-b_deta0_max*x_test[,1]+b_detal_max*x_test[,2]+b_detau_max*x_test[,3]+b_deta2_max*x_test[,4]+b_deta3_max*x_test[,5]+b_deta4_max*x_test[,6]+b_deta5_max*x_test[,7]+b_deta6_max*x_test[,8]+b_deta7_max*x_test[,9]
tautest<-dnorm(G_deta_hattest/sigma_m_max)/pnorm(G_deta_hattest/sigma_m_max)
Yl_hat_MTS <-x_test[,1:9] %*%b_l_hat2[1:9]#+tautest*b_l_hat2[8]
Yu_hat_MTS <- x_test[,1:9]%*%b_u_hat2[1:9]#+tautest*b_l_hat2[8]
MMER <- function(y_l,y_u,YL_hat,YU_hat){
  0.5*mean(abs((y_l-YL_hat)/YL_hat)+abs((y_u-YU_hat)/YU_hat))
}
RMSE<- function(y,Y_hat){
  sqrt(mean((y-Y_hat)^2))
}
cor<- function(y,Y_hat){
  cov(y,Y_hat)/(sd(y)*sd(Y_hat))
}
#评价指标
MMER_MTS <- MMER(y_ltest,y_utest,Yl_hat_MTS,Yu_hat_MTS)
RMSE_L_MTS <- RMSE(y_ltest,Yl_hat_MTS)
RMSE_U_MTS <- RMSE(y_utest,Yu_hat_MTS)
cor_L_MTS <- cor(y_ltest,Yl_hat_MTS)
cor_U_MTS <- cor(y_utest,Yu_hat_MTS)
mts_m <-c(MMER_MTS,RMSE_L_MTS,RMSE_U_MTS,cor_L_MTS,cor_U_MTS)
mtsa[,s]<-mts_m


y_c_CCRM4c<-with(data=ds4.testing[1:50,],y_c)
y_r_CCRM4r<-with(data=ds4.testing[1:50,],y_r)
plot(c(1:50),y_c_CCRM4c,xlab='Random Samples',ylab='Interval Center',main='',type="o",pch=20,col="red")
#lines(c(1:50),Yc_hat_CRM4[101:150,],type="o",pch=8,col="green",lty=2)
lines(c(1:50),Yc_hat_CCRM4[1:50,],type="o",pch=17,col="blue",lty=2)
legend("topright",legend=c("True","MCCRM"),col=c("red","blue"), pch=c(20,17),lty=1,lwd=2)

plot(c(1:50),y_r_CCRM4r,xlab='Random Samples',ylab='Interval Radius',main='',type="o",pch=20,col="red")
#lines(c(1:50),Yr_hat_CRM4[101:150,],type="o",pch=8,col="green",lty=2)
lines(c(1:50),Yr_hat_CCRM4[1:50,],type="o",pch=17,col="blue",lty=2)
legend("topright",legend=c("True","MCCRM"),col=c("red","blue"), pch=c(20,17),lty=1,lwd=2)

y_l_CCRM4c<-with(data=ds4.testing[1:50,],y_l)
y_u_CCRM4r<-with(data=ds4.testing[1:50,],y_u)
plot(c(1:50),y_l_CCRM4c,xlab='Random Samples',ylab='Interval Lower Bound',main='',type="o",pch=20,col="red")
#lines(c(1:50),YL_hat_CRM4[101:150,],type="o",pch=8,col="green",lty=2)
lines(c(1:50),YL_hat_CCRM4[1:50,],type="o",pch=17,col="blue",lty=2)
legend("topright",legend=c("True","MCCRM"),col=c("red","blue"), pch=c(20,17),lty=1,lwd=2)

plot(c(1:50),y_u_CCRM4r,xlab='Random Samples',ylab='Interval Upper Bound',main='',type="o",pch=20,col="red")
#lines(c(1:50),YU_hat_CRM4[101:150,],type="o",pch=8,col="green",lty=2)
lines(c(1:50),YU_hat_CCRM4[1:50,],type="o",pch=17,col="blue",lty=2)
legend("topright",legend=c("True","MCCRM"),col=c("red","blue"), pch=c(20,17),lty=1,lwd=2)


# cs<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# crm_m <-c(0,0,0,0,0)
# crmcs[,k]<-cs
# crma[,k]<-crm_m

#}



#只将均值入表
col_meancrm<-colMeans(abs(Yr_hat_CRM4))
col_meanrcrm<-colMeans(abs(Yr_hat_RCRM4))
col_meanccrm<-colMeans(abs(Yr_hat_CCRM4))
col_meancrm
col_meancrm
col_meanrcrm
yrbi<-(col_meancrm-col_meanccrm)/col_meancrm
yrbi
var(Yr_hat_CRM4)
var(Yr_hat_CCRM4)
varyrbi<-(var(Yr_hat_CRM4)-var(Yr_hat_CCRM4))/var(Yr_hat_CRM4)
varyrbi
row_meanmtsa<-rowMeans(mtsa)
row_meancrma<-rowMeans(crma)
row_meanrcrma<-rowMeans(rcrma)
row_meanccrma<-rowMeans(ccrma)
row_meanlucrm<-rowMeans(lucrm)
row_meanlurcrm<-rowMeans(lurcrm)
row_meanluccrm<-rowMeans(luccrm)
ccrmpj<-cbind(row_meancrma,row_meanrcrma,row_meanccrma)
row_meancrma
lu<-cbind(row_meanlucrm,row_meanlurcrm,row_meanluccrm)
row_meanlucrm
sheets = list("评价" = ccrmpj,"mts" =row_meanmtsa)
library(openxlsx)
write.xlsx(sheets,"sl.xlsx",sep=",")
#write.xlsx(sheets,"E:\\1毕业论文\\论文\\2021刘晓毕业论文\\daima\\ccrm\\二分类\\标准情况\\ccrmr1000.xlsx",sep=",")
#write.xlsx(sheets,"E:\\1毕业论文\\论文\\2021刘晓毕业论文\\daima\\ccrm\\二分类\\r1030C2\\ccrmr1000.xlsx",sep=",")
#write.xlsx(sheets,"E:\\1毕业论文\\论文\\2021刘晓毕业论文\\daima\\ccrm\\二分类\\方差