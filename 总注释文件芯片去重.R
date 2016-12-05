library(xlsx)

setwd("E:/newArabidopsis/stress/abiotic")
total_annotation<-c()
files<-list.files()
for (i in 1:length(files)) {
  oldwd<-setwd(paste(files[i],"注释文件",sep = "/"))
  annotation<-read.xlsx("Total_annotation.xlsx",1)
  total_annotation<-rbind(total_annotation,annotation)
  
  setwd(oldwd)
}
write.csv(total_annotation,"Total_annotation.csv")

##按列提取的提取方式为匹配第一个，而行提取为匹配所有，因此想要去重可以采取如下方法
##必须将GSMs转换成字符型，否则其为numeric
GSMs<-as.character(unique(total_annotation$GSM_ID))
t.total.annotation<-t(total_annotation)
colnames(t.total.annotation)<-total_annotation$GSM_ID
processed.annotation<-c()
for (i in 1:length(GSMs)) {
  row<-t.total.annotation[,GSMs[i]]
  processed.annotation<-cbind(processed.annotation,row)
}
processed.annotation<-t(processed.annotation)
processed.annotation<-as.data.frame(processed.annotation)
write.csv(processed.annotation,"芯片去重后注释.csv")