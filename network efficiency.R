library(igraph) 
library(intergraph); 

network.efficiency<-function(G){ 
  N<-vcount(G); 
  geo.recip<-1/shortest.paths(G); 
  geo.recipNA<-ifelse(geo.recip==Inf,NA,geo.recip); 
  networkEff<-sum(geo.recipNA,na.rm=TRUE)/(N*(N-1)); 
  return(networkEff)  # Computing network efficiency
} 

workPath<-"D:\\Desktop\\protected\\R\\deletenode\\EN_length.net";	#Workpath
number<-read.table('D:\\Desktop\\protected\\R\\deletenode\\BC.csv',header=TRUE)
forMax.V<-nrow(number);	#Times of nodes deletion simulation


#================Import network=========================
originigraph<-read.graph(workPath,"pajek"); 


#===============Delete nodes simulation====================
number.vertices<-vcount(originigraph);	#Total number of vertices 


#-------------------------------Prepare array----------------------------------------
gEf.V<-numeric();	#Network efficiency after deleting nodes

#-------------------Implement nodes deletion calculation-------------------------
verDelNets<-originigraph;
for(i in 1: forMax.V){ 
  startTime<-Sys.time(); 
  #~~~~~~~~~~~~~~~~~Prepare network~~~~~~~~~~~~~~~~~~~~~~
  b<-number[i,1]
  verDelNets<-delete.vertices(verDelNets,as.character(b));
  
  #~~~~~~~~~~~~~~~Compute Network Efficiency~~~~~~~~~~~~~~~~
  gEf.V[i]<-network.efficiency(verDelNets); 
  
  endTime<-Sys.time(); 
  cat("The",i,"of",forMax.V,"total iteration need",endTime-startTime,"seconds","\n"); 
} 

write.csv(gEf.V,"D:\\Desktop\\protected\\R\\deletenode\\gEf_BC_del.csv")
