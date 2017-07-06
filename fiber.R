POD_names<-c("EC1","ITL","JCN")
POD_names1<-paste0(POD_names,"-")
section_names<-outer(POD_names1,POD_names,FUN = "paste0")

dim(section_names)<-c(9,1)
section_validity<-c(section_names,c(0,0,1,0,0,1,1,0,0))
dim(section_validity)<-c(9,2)
row.names(section_validity)<-section_names
colnames(section_validity)<-c("name","valid")


valid_sections<-section_validity[section_validity[,2]==1,]

total_subsections<-c(12,14,18)

section_summary<-matrix(c(valid_sections,total_subsections),nrow=3,ncol=3)
                       

section[[1]]<-vector("list",section_summary[1,3])
section[[2]]<-vector("list",section_summary[2,3])

set.seed(1)
rcodes<-r
branchpt<-4
for (i in 1:branchpt){
  section[[1]][[i]]<-list(sscode=sprintf("%04d",i), ssname=sprintf("%s-%03d",section_summary[1,1],i),cables=c(96,48,12),type="UG")
  section[[2]][[i]]<-list(sscode=sprintf("%04d",i), ssname=sprintf("%s-%03d",section_summary[2,1],i),cables=c(96,48,12),type="UG")
}
#-------
section[[1]][[1]]$up<-NULL
section[[2]][[1]]$up<-NULL

for (i in 2:branchpt){
  section[[1]][[i]]$up<-section[[1]][[i-1]]$ssname
  section[[1]][[i]]$down<-section[[1]][[i+1]]$ssname
  section[[2]][[i]]$up<-section[[1]][[i-1]]$ssname
  section[[2]][[i]]$down<-section[[1]][[i+1]]$ssname
}

for (i in branchpt+1:total_subsections[1]){
  section[[1]][[i]]<-list(sscode=sprintf("%04d",i), ssname=sprintf("%s-%03d",section_summary[1,1],i),cables=c(96,48,12),type="UG")
  section[[1]][[i]]$up<-section[[1]][[i-1]]$ssname
}

for (i in branchpt+1:total_subsections[2]){
  section[[2]][[i]]$up<-section[[2]][[i-1]]$ssname
}

  
}