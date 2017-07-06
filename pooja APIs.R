x<-read.csv("~/Downloads/pooja2.csv",stringsAsFactors = F)
df<-data.frame(apiname=x$API.name,para=x$New.parameters)
param2<-unique(unlist(strsplit(x$New.parameters,",")) )
result<-sapply(X=param2,FUN=grepl,df$para,USE.NAMES = T)
row.names(result)<-df$apiname
write.csv(result,"result.txt")

