#--------------------------------------------------------
#
# Make the barplot presenting indicator species result
#
#------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(openxlsx)

rm(list = ls())
source("make.mnemomic.r")
Path_Setting =  "Data/Table_Indicators"

col_sel=brewer.pal(n = 12, name = "Set3")
col_sel2=brewer.pal(n = 8, name = "Set2")

#This is from colorBrewer Set3
Col_Used = c("#FFFFB3", "#FDB462" ,"#FB8072", "#BEBADA", "#B3DE69" ,"#8DD3C7" ,"#80B1D3",col_sel[8:12],"cyan2",brewer.pal(n = 8, name = "Set2"))
Tab_Col = matrix(NA,nrow=5,ncol=6)
Tab_Name = matrix(NA,nrow=5,ncol=6)

#Give name manually
#Tree seedling
Tab_Name[1:4,6] <- c("Matr1","Matr2","L-Cano","Ripar")
Tab_Col[1:4,6] <- c(18,2,19,13)

#All Trees 16
Tab_Name[1:5,2] <- c("Matr","Alf","Ul-Sol","Ul-Al.Fe","Ul-Al")
Tab_Col[1:5,2] <- c(18,3,1,7,6)

#Large Trees 16
Tab_Name[1:4,5] <- c("Matr","Alf","Ul-Sol","Ul")
Tab_Col[1:4,5] <- c(18,3,1,7)

#Liana seedling 
Tab_Name[1:2,4] <- c("Matr","Alf")
Tab_Col[1:2,4] <- c(18,3)

#All Lianas 16
Tab_Name[1:3,1] <- c("L-Cano","Matr","Alf") 
Tab_Col[1:3,1] <- c(19,18,3)

#Large Lianas 16
Tab_Name[1:2,3] <- c("L-Cano","Matr")
Tab_Col[1:2,3] <- c(19,18)

#Get files
All_Files = list.files(Path_Setting)
All_Files =  All_Files[ All_Files!= "Table1.csv" & !grepl("Supplement_Indicator",All_Files)&
                          !grepl("Small",All_Files)& !grepl("Table3",All_Files)] # remove  the main text table

All_Files = data.frame(Files = All_Files,Tot_Spp = c(131,270,118,105,212,177))

#Visualize function
vis.covar = function(index,Title,max_ylim,Cap_label,Axislab1,Axislab2){
    #Read file
    tmp = read.csv(paste0(Path_Setting,'/',All_Files[index,1]))
   
     #Change the assemblage to factor
    tmp$Assemblage <- as.factor(tmp$Assemblage)
    
    #Find indicator species
    indic = tmp %>%
      filter(Indicator.value >0.25 & P.value <0.05 & No..Idividual >20) %>%
      group_by(Assemblage) %>%
      summarise(N=n()) %>%
      mutate(Percent = round(100*N/All_Files$Tot_Spp[index],digits = 1))%>%
      #mutate(Assemblage = Tab_Name[,i])%>%
      mutate(Types = "indic")
   
    #Join data
    assoc_indic_joined = tmp %>%
      filter(P.value <0.05 ) %>%
      group_by(Assemblage) %>%
      summarise(N=n()) %>%
      mutate(Percent = round(100*N/All_Files$Tot_Spp[index],digits = 1))%>%
      mutate(Types = paste("Assoc",Assemblage,"_"))%>%
      left_join(indic,by=c("Assemblage"),suffix = c(".assoc", ".indic"))
    
    #Total species richness
    All_Spp = All_Files$Tot_Spp[index]
    
    #Remove NA from color table
    Col_Used2 =Col_Used[Tab_Col[,index]]
    Col_Used2 = Col_Used2[is.na(Col_Used2)==F]
    
    #Label or not label on the axis 1 (left)
    if(Axislab1 == "Yes") {
      name_Y1 = "Number of species"
    }else{
      name_Y1 = ""
    }
    #Label or not label on the axis 2 (right)
    if(Axislab2 == "Yes") {
      name_Y2 = "Percent"
    }else{
      name_Y2 = ""
    }
    #Visualize
    p = ggplot(assoc_indic_joined, aes(x=Assemblage, y=N.assoc,fill=Assemblage)) +
      geom_bar(stat="identity", width=0.7)+ 
      geom_bar(stat="identity",aes(x=Assemblage,y=N.indic, fill = Types.indic),width = 0.7)+
      scale_fill_manual(values = c(Col_Used2,alpha("grey15",0.3)))+
      #scale_y_continuous(name="Number of species",limits = c(0, max_ylim),
      scale_y_continuous(name=name_Y1,limits = c(0, max_ylim),
                         #sec.axis=sec_axis(trans=~(./All_Spp)*100,name="Percent"))+
                          sec.axis=sec_axis(trans=~(./All_Spp)*100,name=name_Y2))+
      scale_x_discrete( labels=Tab_Name[,index])+
      labs( y = "% Indicator species", x="",title = paste0(Cap_label,Title," (N=",All_Spp,")"))+
      theme(title = element_text(size=8),     # 14
            #axis.title.y = element_text(size=13),
            axis.text.x = element_text(size=8),  #13
            axis.text.y = element_text(size=8), #11
            legend.position="none")
    #p
    return(p)
  }
  
  
  #Create a figure
    source("multiplot.r")
  
  #png(filename = "Figures/Figure4_Barplots.png",height = 2500,width = 4000, res=300)
 # pdf("Figures/Figure4.pdf", bg="transparent", height=9,width = 13)
pdf("Figures/Figure4.pdf", bg="transparent", height=6,width = 8.66)
#pdf("Figures/Figure4.pdf", bg="transparent", height=9,width = 13)
       multiplot(vis.covar(6,"Tree seedlings",max_ylim = 40,"(A)   ",Axislab1 = "Yes",Axislab2 = ""),
            vis.covar(4,"Liana seedlings",max_ylim = 40,"(D)   ",Axislab1 = "Yes",Axislab2 = ""),
            vis.covar(2,"Established trees",max_ylim = 40,"(B)   ",Axislab1 = "",Axislab2 = ""),
            vis.covar(1,"Established lianas",max_ylim = 40,"(E)   ",Axislab1 = "",Axislab2 = ""),
            vis.covar(5,"Large trees",max_ylim = 40,"(C)   ",Axislab1 = "",Axislab2 = "Yes"),
            vis.covar(3,"Large lianas",max_ylim = 40,"(F)   ",Axislab1 = "",Axislab2 = "Yes"),
            cols=3)
  dev.off()
  
  