library(xlsx)

##依据adj.P.Val的值来判断差异计算的方式
get.different.genes <- function(single.contrast) {
  p<-c()
  if(single.contrast[50,"adj.P.Val"]<=0.01){
    differencial_lists<-single.contrast[abs(single.contrast$logFC)>=1 
                                        & single.contrast$adj.P.Val<=0.01,]
    rownames(differencial_lists)[1]<-"adj.p"
  }
  else {
    differencial_lists<-single.contrast[abs(single.contrast$logFC)>=1 
                                        & single.contrast$P.Value<=0.01,]
    rownames(differencial_lists)[1]<-"p"
  }
  return(differencial_lists)
}

##只对P值提取
get.different.genes <- function(single.contrast) {
    differencial_lists<-single.contrast[abs(single.contrast$logFC)>=1 
                                        & single.contrast$P.Value<=0.01,]
  return(differencial_lists)
}
##依据总差异基因提取其中包含的CYP基因
get.different.CYPs<- function(different_genes,usefulCYPs){
  probe<-usefulCYPs$Probe
  different_CYPs = c()
  for (i in 1:length(probe)) {
    different_CYP<-different_genes[different_genes$X == probe[i],]
    different_CYPs<-rbind(different_CYPs,different_CYP)
  }
  return(different_CYPs)
}

##做超几何检验
get.exact.test<-function(different.genes,different.cyps){
  a<-length(different.cyps$X)
  b<-length(different.genes$X)
  d<- 217
  c<- 22810 - b
  P<-dhyper(a,b,c,d)
  row.data<-c(a,b,P)
  return(row.data)
}
  

setwd("E:/newArabidopsis/stress/abiotic")
categroy<-list.files()
CYPs<-read.csv("E:/newArabidopsis/useful_CYP_annotation.csv",stringsAsFactors = FALSE)
for (i in 1:length(categroy)) {
  path<-paste(categroy[i],"对照差异结果",sep = "/")
  oldwd<-setwd(path)
  dir.create("显著差异结果")
  contrasts<-list.files(pattern = "[csv]")
  hyper.test<-c()
  for (j in 1:length(contrasts)) {
    single.contrast<-read.csv(contrasts[j],stringsAsFactors = FALSE)
    oldwd1<-setwd("显著差异结果")
    differencial_genes<- get.different.genes(single.contrast)
    write.csv(differencial_genes,paste("significant_total",contrasts[j],sep = "_"))
    different_CYPs<-get.different.CYPs(differencial_genes,CYPs)
    write.csv(different_CYPs,paste("significant_CYPs",contrasts[j],sep = "_"))
    row.data<-get.exact.test(differencial_genes,different_CYPs)
    name<-sub(".csv","",contrasts[j],fixed = TRUE)
    row.data<-c(name,row.data)
    hyper.test<-rbind(hyper.test,row.data)
    setwd(oldwd1)
  }
  write.csv(hyper.test,"超几何分布检验.csv")
  setwd(oldwd)
}
