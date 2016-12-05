Get_colnames<-function(GSM,Total_GSM){
  colnames<-c()
  for (k in 1:length(GSM)) {
    name<-grep(GSM[k],Total_GSM,value = TRUE)
    colnames<-c(colnames,name)
  }
  return(colnames)
}

setwd("E:/newArabidopsis/stress/abiotic")

PCA.data<-c()
annotation<-read.csv("芯片去重后注释.csv",stringsAsFactors = FALSE)
total.GSEs<-unique(annotation$GSE_ID)
files<-list.files()

for (i in 1:length(files)) {
  path<-paste(files[i],"RMA数据",sep = "/")
  oldwd<-setwd(path)
  GSEs<-list.files(pattern = "[csv]")
  GSEs.num<-sub(".csv","",GSEs,fixed = TRUE)
  GSEs.num<-sub("GSE","",GSEs.num,fixed = TRUE)
  for (j in 1:length(GSEs)) {
    rmadata<-read.csv(GSEs[j],stringsAsFactors = FALSE)
    total.rma.names<-colnames(rmadata)
    rma.names<-annotation[annotation$GSE_ID==GSEs.num[j],"GSM_ID"]
    colnames<-Get_colnames(rma.names,total.rma.names)
    for (k in 1:length(colnames)) {
      single.data<-rmadata[colnames[k]]
      single.data<-t(single.data)
      PCA.data<-rbind(PCA.data,single.data)
    }
  }
  setwd(oldwd)
}
colnames(PCA.data)<-rmadata$X