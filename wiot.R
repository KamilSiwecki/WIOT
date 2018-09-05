
library(openxlsx)
system.time({

excel <- read.xlsx("C:/Users/Kamil/Desktop/WIOT2014_Nov16_ROW.xlsx",colNames=F,rowNames = FALSE)

# excel[1855:1910,1853:1908]
excel[11,3]

####tu WIOT ################

K<-data.matrix(excel[7:2470,5:2468])
mode(K)

print(K)

X<-data.matrix(excel[2478, 5:2468])
X<-as.vector(X)
print(X)




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


vaa<-X-as.vector(colSums(K)) # wektorek wartosci dodanej

Z<-A

print(excel[5,1855])
print(K[1849,1849])

#### lista krajow####

LKw<-excel[7:2470,3]
print(LKw[100])
LKk<-excel[5,5:2468]
print(LKk[1900])

# print(kraje<-unique(excel[7:2470,3]))
# length(kraje)


#### przeplywy wybrany kraj -> wybrany kraj ##########

print(nk<-K[LKw=="CHN",LKk=="POL"])
print(tnk<-K[LKk=="POL",]) #stworzylismy macierz z wybranymi na odwrot krajami, 
#zeby dostac sie do indeksu - po kolmnach byl z 'x' na poczatku

print(startW<-row.names(nk)[1])
stw<-as.double(startW)-6
mode(stw) # tutaj znalezlismy, dla jakiego indeksu zaczynamy w kraju z ktorego idzie przeplyw


print(startK<-row.names(tnk)[1])
stk<-as.double(startK)-6
print(stk) # tutaj znalezlismy pierwszy indeks kraju, do ktorego idzie przeplyw



n=1
przeplyw<-vector()
procVA<-vector()
procVAa<-vector()
dVA<-vector() ### wektor zmiany procentowego udzialu warotsci dodanej PL w stos do VA calego swiata


for (i in 1:56){
  for(j in 1:56){
    A[stw-1+i,stw-1+j]=(A[stk-1+i,stk-1+j]-0.01)
    # W<-solve(I-A)%*%Y
    W<-qr(I-A)
    W<-solve.qr(W, Y)
    W<-as.vector(W)
    E<-t(t(A)*W)
    s<-colSums(E)
    v<-W-s
    roz<-(v-vaa)
    print(przeplyw[n]<-(paste(excel[stw+5+i,3],  excel[stw+5+i,2], "to sector ",excel[5,stk+3+j], excel[4,stk+3+j])))
    procVA[n]<-(sum(v[stk:(stk+55)]))
    all<-sum(v)
    print(a<-(procVA[n]/all))
    
    procVAa[n]<-(sum(vaa[stk:(stk+55)]))
    ALL<-sum(vaa)
    print(b<-(procVAa[n]/ALL))
    
    print(dVA[n]<-100*(a-b)/b) ## *100 zeby bylo w procentach
    dane<-data.frame(przeplyw[n],dVA[n])
    write.table(dane,file="CHN.csv", row.names = FALSE, col.names=FALSE, sep=",", append = TRUE)
    
    A<-Z #to dlatego, ¿eby po ka¿dej zmianie macierzy A wraca³o do jej poczatkowej wartosci
    # przed kolejnym obiegiem petli
    n=n+1
    
  }
  
}
})
#dVA=dVA*100

namArg=vector()

namArg=paste(rep((1:5), times=c(5,5,5,5,5)),rep(c(1:5),5))
print(namArg) 
### indeksy przeplywow - 1 2 oznacza 1sektor do 2sektora  etc
print(dVA)

tytul<-"Wykres zmian wartosci dodanej w efekcie zmiany przeplywu"

barplot(dVA[1:25],col=rainbow(5),density=45, xlab="przeplywy" ,names.arg = namArg, main=tytul)

z<-data.frame(namArg, dVA)

View(z)

#### Ju¿ WYBRANE PPRZEPLYWY -  DLA POLSKI#####

#### PRZEPLYWY DO SEKTOROW POLSKICH ####
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
    A<-Z 
    
  }
  
}


#### PRZEPLYWY Z SEKTOROW POLSKICH #####

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
    A<-Z 
    
  }
  
}










