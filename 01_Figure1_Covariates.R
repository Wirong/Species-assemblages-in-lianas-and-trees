#-------------------------------------
#
# Create Figure1 in the main text
#
#-------------------------------------

load(paste("Data/Covariates.RData"))

#Because the coordinates of P50 is UTM-WGS84N, we convert it to the (0,0) system
library(terra)


#Visualize the covariates selected in the main text
adj_man=0

pdf("Figures/Figure1.pdf",width = 8.4,height = 7.08) #8.5,7.8
    par(mfrow=c(2,2),cex.main=1.5)
    
    #PCA1
    plot(Covar$PCA1,col=rainbow(8,start=0,end = 0.78,rev=T),main="PC1 of all soil nutrients",
         cex.axis=1,cex.main=1.3,ylab = "Plot width (m)")
    mtext("(a)", side=3, line=1.5, adj=0, cex=1)
    
    #CHM P25th
    plot(Covar$P50,col=topo.colors(20,rev=F),main=" TCH (the 25th perc.)",cex.main=1.3,
         legend.args=list(text='', side=3, font=5, line=10))
    mtext("(b)", side=3, line=1.5, adj=0, cex=1)
    
    #TWI
    plot(Covar$TWI_LiDAR,col=topo.colors(18,rev=T),main="Wetness index",cex.axis=1,cex.main=1.3,
         ylab = "Plot width (m)",xlab = "Plot length (m)",legend.args=list(text='', side=3, font=5, line=0.8,cex=2))
    mtext("(c)", side=3, line=1.5, adj=0, cex=1)
    
    #Solar radiation (convert from Kwh to Mwh)
    plot(Covar$Solar_Kwh/1000,col=heat.colors(15,rev=T),main="Solar radiation",cex.axis=1,cex.main=1.3,
         xlab= "Plot length (m)")
    mtext("(d)", side=3, line=1.5, adj=0, cex=1)

dev.off()

