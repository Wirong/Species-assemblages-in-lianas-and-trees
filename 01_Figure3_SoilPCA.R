#-----------------------------------------------------
#
#Visualize PCA of soil parameters (Figure 3.)
#
#-----------------------------------------------------
library(ade4)
library(factoextra)

rm(list=ls())

Soil_Dat = read.csv('Data/Soil_Krigged.csv')

#PCA
PCAsoil=dudi.pca(Soil_Dat[3:length(Soil_Dat)],scannf = F)

#Create polygons for overlaying
ForPoly = data.frame (Type = c("a","a","a","a","b","b","b","b"),
                      x=c(-1.3,-18,-18,-1.3,8,2,2,8),y=c(-6.8,-6.8,5,5,-6.8,-6.8,5,5))

pdf("Figures/Figure3.pdf",width = 8,height = 6)
  sp=fviz_pca_biplot(PCAsoil, repel = TRUE,xlim=c(-19,10),label = "var",
                   col.var = "#FC4E07",col.ind = "#2E9FDF",title = "PCA-soil nutrients")
        sp + geom_vline(xintercept = 1.9,col="yellow3",linetype="longdash")+
        geom_vline(xintercept = -1.3,col="green2",linetype="longdash")+
        geom_text(x=-1.2, y=-6.65, label="-1.3",size=3,col="green3")+
        geom_text(x=2.3, y=-6.65, label="2.1",size=3,col="yellow3")+
        geom_polygon(data=ForPoly,aes(x=x,y=y,group=Type,fill=Type),alpha=0.08)+
        theme(legend.position="none")+
        scale_fill_manual(values = c("green3","yellow3")) 

dev.off()

