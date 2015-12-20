
#load required libraries
library(ape)

#============================== Mantel's Test on PCA ===============================================
#create empty data frame
record = data.frame()

#initialize loop variables
k = 2
v = nrow(vx)

while(v != k )
{
  c <-prcomp(vx[1:k,])
  d <-as.data.frame(c$rotation)
  e <-d[c('PC1','PC2')]
  
  #create empty matrix
  n <-matrix(nrow = nrow(e),ncol = nrow(e))
  colnames(n) <-rownames(e)
  rownames(n) <-rownames(e)
  
  #populate matrix
  for (i in 1:nrow(e)){
    for (j in 1:nrow(e)){
      n[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
    }
  }
  
  c <-prcomp(vx[1:v,])
  d <-as.data.frame(c$rotation)
  e <-d[c('PC1','PC2')]
  
  #create empty matrix
  m <-matrix(nrow = nrow(e),ncol = nrow(e))
  colnames(m) <-rownames(e)
  rownames(m) <-rownames(e)
  
  #populate matrix
  for (i in 1:nrow(e)){
    for (j in 1:nrow(e)){
      m[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
    }
  }
  
  
  #mantel's permutation test
  mantle <-mantel.test(m,n,graph = FALSE)
  
  rec = c(k,v,mantle$p)
  record = rbind(record,rec)
  
  #increment counter
  k = k+1
  v = v-1
} 
