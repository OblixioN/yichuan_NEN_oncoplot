library("heatmap.plus")
mydata<-read.table("/media/nsjb2/coplot_NEN/CNV_matrix_FACET_ALL_10192017",header=TRUE,row.names=1)
my_palette <- colorRampPalette(c("white", "red", "darkred"))(n = 299)
#NEC(black)/NET(grey)
theColor_3=matrix(c("grey","grey","grey","grey","black","black","black","black","grey","grey","grey","black","grey","black","black","grey","grey","grey","grey","grey","grey","grey","black","grey","black","grey","grey","black","grey","black","black","black","black","grey","grey","grey","black","black","grey","black","black","grey","black","black","black","black","grey","black","grey","black"))
#G1(blue)/G2(green)/G3(red)
theColor_2=matrix(c("red","green","green","green","red","red","red","red","red","green","red","red","blue","red","red","blue","green","green","green","green","green","green","red","green","red","blue","green","red","green","red","red","red","red","green","blue","green","red","red","green","red","red","green","red","red","red","red","red","red","green","red"))
#gastric(orange)/intestine(purple)/lung(dark red)
theColor_1=matrix(c("orange","orange","orange","purple","dark red","orange","orange","dark red","orange","orange","orange","orange","orange","dark red","dark red","orange","purple","purple","orange","orange","purple","purple","orange","purple","dark red","orange","purple","dark red","orange","dark red","orange","orange","orange","orange","orange","orange","dark red","orange","purple","orange","orange","purple","dark red","orange","orange","orange","purple","dark red","orange","orange"))
png(file="/media/nsjb2/coplot_NEN/heatmap.png",height=6000,width=6000,res=600)
heatmap.plus(as.matrix(mydata),Rowv=NA,labRow=FALSE, scale=c("row"),margins=c(15,3),ylab='Genes',xlab='Samples',col=my_palette,ColSideColors=cbind(theColor_1,theColor_2,theColor_3))
dev.off()


#hc<-hclust(dist(as.matrix(t(mydata))))
#

#order.dendrogram(hc)<-c(50,1:49)
#library("ggplot2")
#library("ggdendro")

hc <- hclust(dist(as.matrix(t(mydata))))

final<-c("S10","S04","S09","S08","S05","S01","S07","S06","S02","S03","S11","S14","S15","S17","S12","S13","S16","S21","S19","S27","S18","S22","S28","S29","S24","S25","S23","S20","S26","S46","S32","S39","S33","S44","S30","S43","S35","S36","S38","S48","S45","S47","S41","S50","S42","S49","S40","S34","S31","S37")
final_order<-c(rep(0,length(final)))
for (i in 1:length(hc$label)){
final_order[which(final==hc$label[i])]<-i
}
hc$order<-final_order
colorkey<-t(cbind(theColor_3, theColor_2, theColor_1))
colnames(colorkey)<-colnames(mydata)
colorkey<-colorkey[,final]

pdf(file="/media/nsjb2/coplot_NEN/dendro_and_key.pdf",height=10,width=10)
plot(hc,hang=-1)
plot(x=NULL,y=NULL,xlim=c(0,55),ylim=c(0,55))
rect(xleft=c(1:50),xright=c(1:50)+0.9,ytop=4.9,ybottom=4,col=colorkey[1,],border=NA)
rect(xleft=c(1:50),xright=c(1:50)+0.9,ytop=3.9,ybottom=3,col=colorkey[2,],border=NA)
rect(xleft=c(1:50),xright=c(1:50)+0.9,ytop=2.9,ybottom=2,col=colorkey[3,],border=NA)
text(x=c(1:50)+0.5,y=7.5, colnames(colorkey), srt=90,cex=0.7)
dev.off()
