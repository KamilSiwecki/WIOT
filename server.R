
library(shiny)
library(openxlsx)
plik<-"C:/Users/Kamil/Desktop/vaio/Documents/I-O WIOD PL/WIOT2014_Nov16_ROW.xlsx"
excel <- read.xlsx(plik,colNames=F,rowNames = FALSE)


#### WIOT ################

K<-data.matrix(excel[7:2470,5:2468])
mode(K)


X<-data.matrix(excel[2478, 5:2468])
X<-as.vector(X)
write.csv(X, "wektorX.csv", row.names =FALSE, quote=FALSE, col.names = FALSE)



A=t(t(K)/X)   
A[is.nan(A)]=0
#print(A[10:100,10:100])


I=diag(1,2464,2464)
#print(I)

Y=(I-A)%*%X
#print(Y)

##sprawdzenie czy odwracanie dziala
T<-solve(I-A)%*%Y
print(T[11:15])
print(X[11:15])
# print(t(t(A)*X))


vaa<-X-as.vector(colSums(K)) # wektorek wartosci dodanej

Z<-A

#print(excel[5,1855])
#print(K[1849,1849])

#### lista krajow####




n=1
przeplyw<-vector()
procVA<-vector()
procVAa<-vector()
dVA<-vector() ### wektor zmiany procentowego udzialu warotsci dodanej PL w stos do VA calego swiata

GetWIOT<-function(incA, k1,k2,lp){ ## lp - liczba przeplywow
  
  lp<-as.numeric(lp)
  k1<-as.numeric(k1)
  k2<-as.numeric(k2)
  LKw<-excel[7:2470,3]
  print(LKw[100])
  kraje<-unique(LKw)
  write.csv(kraje, "dane.csv", row.names =FALSE, quote=FALSE, col.names = FALSE)
  print(kraje)
  a<-kraje[k1]
  b<-kraje[k2]
  LKk<-excel[5,5:2468]
  print(LKk[1900])
  print(nk<-K[LKw==a,LKk==b])
  print(tnk<-K[LKk==b,])
  #stworzylismy macierz z wybranymi na odwrot krajami, zeby dobrac sie do indeksu - 
  #po kolmnach byl z 'x' na poczatku
  
  
  print(startW<-row.names(nk)[1])
  stw<-as.double(startW)-6
  #mode(stw) # tutaj znalezlismy, dla jakiego indeksu zaczynamy w kraju z ktorego idzie przeplyw
  
  
  
  print(startK<-rownames(tnk)[1])
  stk<-as.double(startK)-6
  #print(stk) # tutaj znalezlismy pierwszy indeks kraju, do ktorego idzie przeplyw
  

for (i in 1:lp){
  for(j in 1:lp){
    A[stw-1+i,stw-1+j]=(A[stk-1+i,stk-1+j]-incA)
    W<-solve(I-A)%*%Y
    W<-as.vector(W)
    E<-t(t(A)*W)
    s<-colSums(E)
    v<-W-s
    roz<-(v-vaa)
    print(przeplyw[n]<-(paste(excel[stw+5+i,3],  excel[stw+5+i,2], "to sector",excel[5,stk+3+j], excel[4,stk+3+j])))
    procVA[n]<-(sum(v[stk:(stk+55)]))
    all<-sum(v)
    print(a<-(procVA[n]/all))
    
    procVAa[n]<-(sum(vaa[stk:(stk+55)]))
    ALL<-sum(vaa)
    print(b<-(procVAa[n]/ALL))
    
    print(dVA[n]<-100*(a-b)/b) ## *100 zeby bylo w procentach
    A<-Z 
    n=n+1
    
  }
  
}

  GetWIOT<-dVA
}
#dVA=dVA*100
#print(GetWIOT(0.01, 11,34))
#print(dp<-read.csv("dane.csv"))


namArg=vector()

namArg=paste(rep((1:5), times=c(5,5,5,5,5)),rep(c(1:5),5))
print(namArg) 
### indeksy przeplywow- 1 2 oznacza 1 ektor do 2 sektora  etc
print(dVA)

  kraje<-read.csv("dane.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  a<-reactive({tapply(X, LKw, mean )})
  output$print<-renderPrint({summary(a())})
  
  b<-reactive({tapply(X, LKw, sum)})
  output$plot<-renderPlot({
    
    tt<-"Wykres sum produktow globalnych dla krajow "
    plot(kraje, b(),type="b", main=tt,col="red",xlab = "kraje", ylab = "wartosc prod global",col.axis="violet" ,col.main="blue", col.lab="blue")
         })
  
  d<-reactive({(GetWIOT(input$incA, input$kraj1, input$kraj2, input$lp))})

  output$print1<-reactive({d()[]})


  output$distPlot <- renderPlot({
tyt<-"Wykres zmian wartosci dodanej w efekcie zmiany przeplywu"
    barplot(d(),col=terrain.colors(5),xlab="przeplywy" ,col.main=84, col.lab="purple", col.axis="violet", main=tyt)


  })
  
})