library(openxlsx)
excel <- read.xlsx("C:/Users/Kamil/Documents/I-O WIOD PL/WIOT2014_Nov16_ROW.xlsx",colNames=F,rowNames = FALSE)

# excel[1855:1910,1853:1908]
excel[11,3]

####tu WIOT ################

K<-data.matrix(excel[7:2470,5:2468])
mode(K)


X<-data.matrix(excel[2478, 5:2468])
X<-as.vector(X)



A=t(t(K)/X)   
A[is.nan(A)]=0
print(A[10:100,10:100])


I=diag(1,2464,2464)
print(I)

Y=(I-A)%*%X
print(Y)

T<-solve(I-A)%*%Y
print(T[11:15])
print(X[11:15])

# print(t(t(A)*X))


vaa<-X-as.vector(colSums(K)) # wektorek wartości dodanej

Z<-A

print(excel[5,1855])
print(K[1849,1849])


#### PRZEPLYWY DO SEKTORÓW POLSKICH ####
for (i in 1:2464){
  for(j in 1849:1904){
    A[i,j]=(A[i,j]+0.01)
    W<-solve(I-A)%*%Y
    W<-as.vector(W)
    E<-t(t(A)*W)
    s<-colSums(E)
    v<-W-s
    roz<-(vaa-v)
    print(paste(excel[6+i,3],  excel[6+i,2]))
    print(roz[abs(roz)>10])
    A<-Z #to dlatego, żeby po każdej zmianie macierzy A wracał do jej początkowej wartości
    # przed kolejnym obiegiem petli
    
  }
  
}


#### PRZEPŁYWY Z SEKTORÓW POLSKICH #####

for (i in 1849:1904){
  for(j in 1:2464){
    A[i,j]=(A[i,j]+0.01)
    W<-solve(I-A)%*%Y
    W<-as.vector(W)
    E<-t(t(A)*W)
    s<-colSums(E)
    v<-W-s
    roz<-(vaa-v)
    print(paste(excel[6+i,3],  excel[6+i,2], "to sector",excel[5,4+j], excel[4,4+j]))
    print(roz[abs(roz)>10])
    A<-Z #to dlatego, żeby po każdej zmianie macierzy A wracał do jej początkowej wartości
    # przed kolejnym obiegiem petli
    
  }
  
}
