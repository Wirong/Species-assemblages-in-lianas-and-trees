#------------------------------------------
#
# Visualize the map of the MRT result (Figure2.)
#------------------------------------------
library(RColorBrewer)

rm(list = ls())

FigPath="Figures/"

#Colors applied
col_sel=brewer.pal(n = 12, name = "Set3")
col_sel2=brewer.pal(n = 8, name = "Set2")
#This is from colorBrewer Set3
Col_Used = c("#FFFFB3", "#FDB462" ,"#FB8072", "#BEBADA", "#B3DE69" ,"#8DD3C7" ,"#80B1D3",col_sel[8:12],"cyan2",brewer.pal(n = 8, name = "Set2"))

All_Files04 <- list.files("Data/Result_MRT/")

#load covariates
load("Data/Covariates.RData")

File_Order = c(7,2,6,4,1,3)
All_Files_Sel <- All_Files04[File_Order]
#Name for title
Names_Title = c("Tree seedlings","All establ. trees" ,"Large trees",
                "Liana seedlings","All establ. lianas","Large lianas")


All_Names =data.frame(All_Files_Sel,Names_Title)


toCutwhite = function(i){
  load(paste("Data/Result_MRT/",All_Names[i,1],sep=""))
  cutvec<-as.factor(findInterval(y,1:100))
  
  rm(i)
  if(Type=="Tree_Small.2016" |Type=="All_Tree.2016" ){
    index3=which(cutvec==3)
    index4=which(cutvec==4)
    index5=which(cutvec==5)
    #replace
    cutvec[index3] <-5  #High solar
    cutvec[index5] <-3  #PCA2 <
    #repeat stupid!!
    index4=which(cutvec==4)
    index5=which(cutvec==5)
    cutvec[index5] <-4
    cutvec[index4] <-5
  }else if(Type=="Tree_Large.2016"){
    index1=which(cutvec==1)   
    index2=which(cutvec==2)   
    index3=which(cutvec==3)  # high solar
    index4=which(cutvec==4)  
    
    cutvec[index2] <-4
    cutvec[index4] <-2
    #repeat 
    index1=which(cutvec==1)
    index4=which(cutvec==4)
    cutvec[index1] <-4
    cutvec[index4] <-1
    
  }else if(Type=="All_Liana.g2cm.2016"){
    index1=which(cutvec==1)
    index2=which(cutvec==2)
    index3=which(cutvec==3)
    #replace
    cutvec[index1] <-2
    cutvec[index2] <-3
    cutvec[index3] <-1
  }else if(Type=="Small_Liana.g2cm_cut5cm.2016"){
    index1=which(cutvec==1)
    index2=which(cutvec==2)
    #replace
    cutvec[index1] <-2
    cutvec[index2] <-1
   
  }else if(Type=="Tree_Seedling.2018"){
    index1=which(cutvec==1)   
    index2=which(cutvec==2)  
    #index3=which(cutvec==3)  
    index4=which(cutvec==4)  
    #replace
    cutvec[index1] <-2 
    cutvec[index2] <-1 
    
  }
  
  cutwhite <- im(cutvec, xcol = seq(1, 30, length = 30), yrow = seq(1, 25, length = 25))
  return(cutwhite) 
}

Tab_Col = matrix(NA,nrow=length(All_Files_Sel),ncol=5)
Tab_Name = matrix(NA,nrow=length(All_Files_Sel),ncol=5)

#Give name manually
#Tree seedling
Tab_Name[1,1:4] <- c("matrix","near-Bs","Low canopy","Ripar")
Tab_Col[1,1:4] <- c(18,2,19,13)

#All Trees 16
Tab_Name[2,1:5] <- c("matrix","Soil Bs","Al.sat(PCA2>)","Solar","Al-Fe(PCA2<")
Tab_Col[2,1:5] <- c(18,3,1,7,6)

#Large Trees 16
Tab_Name[3,1:4] <- c("matrix","Soil Bs","Solar","Al-Fe")
Tab_Col[3,1:4] <- c(18,3,1,7)

#Liana seedling 
Tab_Name[4,1:2] <- c("matrix","Soil Bs")
Tab_Col[4,1:2] <- c(18,3)

#All Lianas 16
Tab_Name[5,1:3] <- c("PCA>=-1.8","Soil Bs","low canopy") 
Tab_Col[5,1:3] <- c(19,18,3)

#Large Lianas 16
Tab_Name[6,1:2] <- c("P10<20","P10>20")
Tab_Col[6,1:2] <- c(19,18)


#----------------------------------------------
#Create Figure 
col_mtext = "black"
Ribbon_sep =0.07
ribw=0.05
adj_man = 0

pdf(paste(FigPath,"Figure2.pdf",sep=""), bg="transparent", height=6,width = 8.6)
#6 9.5
   # par(mfrow=c(2,3),cex.main=1.2,cex=1,col.main=col_mtext,cex.axis=1.2)
    par(mfrow=c(2,3),cex.main=1.1,cex=1,col.main=col_mtext,cex.axis=1)
    par(mar=c(0.3,0.7,1.2,1.5))

    i=1
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    mtext("(a)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)
    
    i=2
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    
    mtext("(b)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)
    
    i=3
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    mtext("(c)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)
    
    
    i=4
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    mtext("(d)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)
    
    i=5
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    mtext("(e)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)
    
    i=6
    Sel = Tab_Col[i,]
    Sel = Sel[is.na(Sel)==F]
    plot(toCutwhite(i), col=Col_Used[Sel], ribsep=Ribbon_sep,ribwid = ribw,  ribbon=T,main=Names_Title[i])
    mtext("(f)", side=3, line=0, adj=adj_man, cex=1.3, col=col_mtext)

dev.off()


#----------------------------------------------
