setwd("E:/Aegis/Machine Learning/Data")
cpu11=read.csv("data set.csv",stringsAsFactors = TRUE)

colnames(cpu11)=c("vendor","model name","mcyt","mmin","mmax","cach","chmin","chmax","prp","erp")

cpu11$vendor=NULL
cpu11$`model name`=NULL
cpu11$erp=NULL

anyNA(cpu11)

cor(cpu11)

boxplot(cpu11,horizontal = TRUE)

outlier.removal.with.mean=function(a){
  for(j in 1:ncol(a)){
    v=summary(a[,j])
    rightlimit=v[5]+1.5*IQR(v)
    leftlimit=v[2]-1.5*IQR(v)
    
    for(i in 1:nrow(a)){
      if(a[i,j]>rightlimit || a[i,j]<leftlimit){
        a[i,j]=mean(a[,j])
      }
    }
  }
  return(a)
}

cpu11=outlier.removal.with.mean(cpu11)
boxplot(cpu11)


indi=sample(1:nrow(cpu11),round(0.70*(nrow(cpu11))))
cpu11train=cpu11[indi,]
cpu11test=cpu11[-indi,]

#fitt14=lm(formula = prp ~ mcyt + mmin + mmax + cach + chmin + chmax , data = cpu11train)
#summary(fitt11)
#plot(fitt11)

#fitt14=lm(log(prp) ~ mcyt + mmin + mmax + cach + chmin + chmax,cpu11train)#0.4865599
#summary(fitt12)
#plot(fitt12)


#fitt14=lm(log(prp)~ log(mcyt) + log(mmin) + log(mmax) + cach + chmin  ,cpu11train)
#summary(fitt13)
#plot(fitt13)


fitt14=lm(formula = log(prp) ~   log(mmax) + cach + chmax, data = cpu11train)
summary(fitt14)
plot(fitt14)
anova(fitt14)

out14=predict(fitt14,cpu11test)

error=out14-log(cpu11test[7])

RMSE14=sqrt(mean((error)^2))
RMSE14
 

