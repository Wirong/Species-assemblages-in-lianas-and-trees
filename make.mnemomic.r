#Create mnemonic from  genus and species
#s= "Pterospermum cinnamomeum"
make.mnemonic = function(Spp){
  a =unlist(strsplit(as.character(Spp)," +"))
  return(toupper(paste(substr(a[1],1,4),substr(a[2],1,2),sep="")))
}
