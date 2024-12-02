#----------------------------------------------
#
# MRT analysis 
#
#-----------------------------------------------

ResPath = "Data/Result_MRT/"

#Select all files in the folder
All_Files <- list.files(paste("Data/Spp_Covariates",sep=""))

#Remove Covar.RData
All_Files <- All_Files[!(All_Files%in%"Covariates.RData")]

#Automatically run all analyses
for(i in 1:length(All_Files)){
  
  load(paste("Data/Spp_Covariates/",All_Files[i],sep=""))
  
  #remove x and y
  extr$x = NULL
  extr$y = NULL
  
  #Before pruning
  fit <- mvpart(gdist(extr[,1:numsps],meth="bray",full=TRUE,sq=TRUE)~
                  PCA1+PCA2+P90+P75+P50+P25+P10 + TWI_LiDAR+ 
                  curvature+slope+aspect+Solar_Kwh+CV5m,
                 data = extr, method="mrt", xv="min")
  
  #summary(fit)
  plotcp(fit)
  
  #We used 'if else' becuase both type could not be prunned anymore.
  if(Type =="Large_Lianas"|Type =="Lia_Seedlings"){ 
    
    #find quadrat numbers
    cells <- rowSums(extr[,1:numsps])
    empty_cells_ids = which(cells==0)
    final_mrt <- fit
    #This is only for making the table
    pruned_table = as.data.frame(final_mrt$cptable)
    
    m = as.numeric(with(final_mrt, where))
    Orig_m_size =length(m)
    #insert missing
    for (k in 1:length(empty_cells_ids)){
      m <- insert(m, ats=empty_cells_ids[k], values=NA)
    }
  }else{
    #To select best cp
    revised_stop = cp.select(fit)
    
    #Run with prunning
    final_mrt = mvpart(gdist(extr[,1:numsps],meth="bray",full=TRUE,sq=TRUE)~ 
                         PCA1+PCA2+P90+P75+P50+P25+P10+TWI_LiDAR+ 
                         curvature+slope+aspect+Solar_Kwh+CV5m,
                       data = extr, pretty = T, xv = "min", minauto = T,
                       which = 4, bord = T, uniform = F, text.add = T, branch = 1,
                       xadj = .7, yadj = 1.2, use.n = T, margin = 0.05, keep.y = F,
                       bars = T, all.leaves = F, control = rpart.control(cp = revised_stop,xval=10),size =5 ,
                       plot.add = T)
    
    
    #final cp table
    pruned_table = as.data.frame(final_mrt$cptable)
    #print(pruned_table)
    sink(paste(ResPath,"Pruned_table_",Type,".txt",sep=""))
    print(pruned_table)
    sink() 
    
    #Identify and Plot the mrt groups/classes
    m= as.numeric(with(final_mrt, where))
    #Identify and Plot the mrt groups/classes
    Orig_m_size =length(m) ## check number of sites equals to total(750)?
    # If less than total (750), then Find cells that have no species in them and fill with NA
    if(Orig_m_size < 750){
      
      #find quadrat numbers
      cells <- rowSums(extr[,1:numsps])
      empty_cells_ids = which(cells==0)
      #insert missing
      for (k in 1:length(empty_cells_ids)){
        m <- insert(m, ats=empty_cells_ids[k], values=NA)
      }
    }
    
  }
  m <- match(m, sort(unique(m))) # renumbering clusters to consecutive integers
  print(sort(unique(m)))  #number of classes/groups
  
  mat <- matrix(m, nrow = 25, ncol = 30, byrow =TRUE)
  x<-mat
  
  # Flip matrix (upside-down)
  flip.matrix <- function(x) {
    mirror.matrix(rotate180.matrix(x))
  }
  
  # Mirror matrix (left-right)
  mirror.matrix <- function(x) {
    xx <- as.data.frame(x);
    xx <- rev(xx);
    xx <- as.matrix(xx);
    xx;
  }
  
  # Rotate matrix 180 clockworks
  rotate180.matrix <- function(x) {
    xx <- rev(x);
    dim(xx) <- dim(x);
    xx;
  }
  
  y<-flip.matrix(x)
  
  cutvec<-as.factor(findInterval(y,1:100))
  cutwhite <- im(cutvec, xcol = seq(0, 2, length = 30), yrow = seq(0, 1, length = 25))
  
  #create color
  col_Index =c(564,50,150,494,552, 45, 100, 380, 125)
  #sampling 
  tmpc = 1:length(unique(m[is.na(m)==F]))
  
  #-------------To find Indicator species -------------------------
  ##Run "indval" Dufrene-Legendre Indicator Species Analysis in pakckage "labdsv"
  m = as.numeric(with(final_mrt, where))
  m <- match(m, sort(unique(m))) # renumbering clusters to consecutive integers
  
  tmp <- extr[,1:numsps]
  
  if(Orig_m_size < 750){
    tmp <- tmp[!row.names(tmp)%in%empty_cells_ids,]
  }
  
  ind <- indval(tmp,m) # runs indicator species analysis
  
  sink(paste("Data/Result_Indicator_Species/IndicatorSpp_p25_",Type,".txt",sep=""))
  print(summary(ind,p=0.25,too.many=180))
  sink()
  
  #--------------------------------------------------------------
  
  save.image(paste(ResPath,"Results_",Type,".RData",sep=""))
}
