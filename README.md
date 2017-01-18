----------------------------------------------------------------
#Citire date din fisiere csv
citireCSV = function(numeFisier){
  tryCatch(
    {
      tabel = read.csv(file = numeFisier,row.names = 2) #rowNames -> coloanele pe care le considera a fi numele 
      return(tabel)
    },
    error = function(e){
      stop(paste("Eroare citire din fisier!",e))
    }
  )
}
-----------------------------------------------
#Eliminare valori lipsa prin inlocuire cu media
eliminareNA = function(tabel){
  tryCatch(
    {
      n = length(tabel)
      for(i in 1:n){
        if(is.numeric(tabel[,i])){
          tabel[is.na(tabel[,i]),i]=mean(tabel[,i],na.rm = T)
        }
      }
      return(tabel)
    },
    error = function(e){
      stop(paste("Eroare eliminare NA!",e))
    }
  )
}

------------------------------------------------
#calcul indicatori asociati variabilelor din setul de date
descriere = function(tabel){
  tryCatch(
    {
      k = select.list(choices = names(tabel),multiple = T,graphics = T)
      sapply(X = tabel[k],FUN = function(coloana){
        if(is.numeric(coloana)){
          m = mean(x = coloana)
          varianta = var(x = coloana)
          std = sqrt(varianta)
          mediana = median(x = coloana)
          simetrie = moments::skewness(x = coloana)
          aplatizare = moments::kurtosis(coloana)
          return(c(
            Media=m,
            Varianta=varianta,
            AbatereaStandard=std,
            Mediana=mediana,
            Simetria=simetrie,
            Aplatizarea=aplatizare))
        }
      })
    },
    error = function(e){
      stop(paste("Eroare eliminare NA!",e))
    }
  )
}

----------------------------------------------------
#functie pentru acp 
acp = function(setDate){
  tryCatch(
    {
      m = length(setDate)
      for(j in 1:m){
        setDate[is.na(setDate[,j]),j]=mean(x = setDate[,j],na.rm = T)
      }
      rez = princomp(x = setDate,cor = T)
      #Abateri standard componente
      sd = rez$sdev
      #Variantele componentelor
      alpha = sd*sd
      #Legatura dintre variabilele cauzale si componente
      a = rez$loadings
      #Componentele principale
      C = rez$scores
      #Corelatiile dintre componente si variabile cauzale
      R = matrix(0,nrow = m,ncol = m)
      for(j in 1:m){
        R[,j] = a[,j] * sd[j]
      }
      return(
        list(
          Varianta = alpha,
          Componente = C,
          Corelatii = R
        )
      )
    },
    error = function(e){
      stop(paste("Eroare ACP!",e))
    }
  )
}

---------------------------------------------------------

#centralizare varianta-no idea what this is
centralizareVarianta = function(alpha){
  alphaC = cumsum(alpha)
  proc = alpha*100/sum(alpha)
  procC = cumsum(proc)
  return(
    data.frame(varianta=alpha,Varianta.Cumultata=alphaC,Procent.Varianta=proc,
               Procent.Cumulat=procC)
  )
}

---------------------------------------
#exemplu script implementare
source("Seminar2_1071.R")
tabel = read.csv("Mortalitate/Mortalitate.csv",row.names = 2)
k = select.list(choices = names(tabel),multiple = T,graphics = T)
rez = acp(setDate = tabel[k])
tabelVarianta = centralizareVarianta(rez$Varianta)
View(tabelVarianta)
R = rez$Corelatii
rownames(R) = k
View(R)
write.csv(x = tabelVarianta,file = "DistributieVarianta.csv",row.names = T)
write.csv(x = R,file = "CorelatiiFactoriale.csv",row.names = T)


--------------------------------------------------
#alta metoda de acp cred
acp = function(tabel){
  tryCatch(
    {
       m = length(tabel)
       #Suplinire cu media a valorilor lipsa
       for(j in 1:m){
         tabel[is.na(tabel[,j]),j]=mean(tabel[,j],na.rm = T)
       }
       #standardizare tabel
       n = nrow(tabel)
       x = sapply(X = tabel,FUN = function(coloana){
         b = coloana - mean(coloana)
         std = sqrt(sum(b*b)/n)
         return(b/std)
       })
       #Calcul matrice de corelatie
       Cor = (1/n)*t(x)%*%x
       #Calcul vectori si valori proprii
       eigv = eigen(x = Cor)
       alpha = eigv$values
       a = eigv$vectors
       #Calcul componente
       C = x%*%a
       #Calcul corelatii componente - variabile cauzale
       R = cor(x = x,y = C)
       return(
         list(
           X = x,
           Cor = Cor,
           Valori.Proprii=alpha,
           Vectori.Proprii=a,
           Componente=C,
           R=R
         )
       )
    },
    error = function(e){
      stop(paste("Eroare PCA!",e))
    }
  )
}

-----------------------------------------------------

tabelareVarianta = function(alpha){
  alphaC = cumsum(alpha)
  procent=alpha*100/sum(alpha)
  procentC=cumsum(procent)
  return(
    data.frame(
      Varianta=alpha,
      Varianta.Cumulata=alphaC,
      Procent.Varianta=procent,
      Procent.Cumulat=procentC
    )
  )
}

-------------------------------------------
#desenare grafic
plotComponente = function(c1,c2){
  if(names(dev.cur())!="windows"){
    dev.new()
  }
  plot(x = c1,y = c2,
       main = "Reprezentare in spatiul principal",
       xlab = "Componenta 1",
       ylab = "Componenta 2",
       col="blue")
  text(x = c1,y = c2,
       labels = rownames(tabel),col="red",pos = 1)
}
----------------------------------
#apel
tabel = read.csv(file = "GrupeDeSange/GrupeS.csv",row.names = 1)
k = select.list(choices = names(tabel),multiple = T,graphics = T,
                title = "Selectie variabile")
rezultateAcp = acp(tabel = tabel[k])
tabelVarianta = tabelareVarianta(rezultateAcp$Valori.Proprii)
#View(tabelVarianta)
#View(rezultateAcp$R)
write.csv(x = tabelVarianta,file = "DistributieVarianta.csv",row.names = T)
write.csv(x = rezultateAcp$R,file = "CorelatiiFactoriale.csv",row.names = T)
#plot componente
plotComponente(rezultateAcp$Componente[,1],rezultateAcp$Componente[,2])

----------------------------------------------------------
acp = function(setDate){
  tryCatch(
    {
      m = length(setDate)
      for(j in 1:m){
        setDate[is.na(setDate[,j]),j]=mean(x = setDate[,j],na.rm = T)
      }
      rez = princomp(x = setDate,cor = T)
      #Abateri standard componente
      sd = rez$sdev
      #Variantele componentelor
      alpha = sd*sd
      #Legatura dintre variabilele cauzale si componente
      a = rez$loadings
      #Componentele principale
      C = rez$scores
      #Corelatiile dintre componente si variabile cauzale
      R = matrix(0,nrow = m,ncol = m)
      for(j in 1:m){
        R[,j] = a[,j] * sd[j]
      }
      return(
        list(
          Varianta = alpha,
          Componente = C,
          Corelatii = R
        )
      )
    },
    error = function(e){
      stop(paste("Eroare ACP!",e))
    }
  )
}

----------------------------------------------
centralizareVarianta = function(alpha){
  alphaC = cumsum(alpha)
  proc = alpha*100/sum(alpha)
  procC = cumsum(proc)
  return(
    data.frame(varianta=alpha,Varianta.Cumultata=alphaC,Procent.Varianta=proc,
               Procent.Cumulat=procC)
  )
}
---------------------------------
#' Plot valori proprii
plotValoriProprii = function(alpha){
  tryCatch(
    {
      dev.new(noRStudioGD = T)
      plot(x = 1:length(alpha), y = alpha,type = "l",main="Distributia variantei",
           xlab = "Numar axa",ylab = "Varianta",col="blue")
      points(x = 1:length(alpha), y = alpha,col="red")
      abline(h = 1,col="green")
    },
    error = function(e){
      stop(paste("Eroare plot VP!",e))
    }
    
  )
}
-------------------------------
plotInstnte = function(X,Y,etichete,titlu,k1,k2){
  tryCatch(
    {
      dev.new(noRStudioGD = T)
      plot(x = X,y = Y,main = titlu,xlab = k1,ylab = k2,col="blue")
      text(x = X,y = Y,labels = etichete,col="red",pos = 1)
      abline(h = 0,col="green")
      abline(v = 0,col="green")
    },
    error = function(e){
      stop(paste("Eroare plot instante!",e))
    }    
  )
}
------------------------------
cerculCorelatiilor = function(X,Y,etichete,k1,k2){
  tryCatch(
    {
      dev.new(noRStudioGD = T)
      t = seq(from=0,to=2*pi,length.out = 100)
      plot(x = cos(t),y = sin(t),main = "Cercul corelatiilor",
           type = "l",xlab = k1,ylab = k2)
      points(x = X,y = Y,col="blue")
      text(x = X,y = Y,labels = etichete,col="red",pos = 1)
      abline(h = 0,col="green")
      abline(v = 0,col="green")
    },
    error = function(e){
      stop(paste("Eroare plot instante!",e))
    }    
  )
}
----------------------------------------------
scoruri = function(C,alpha){
  tryCatch(
    {
      m=ncol(C)
      CS=matrix(0,nrow = nrow(C),ncol = m)
      for(j in 1:m){
        CS[,j]=C[,j]/alpha[j]
      }
      return(CS)
    },
    error = function(e){
      stop(paste("Eroare calcul scoruri!",e))
    }     
  )
}
----------------------------------------------------
seminar = function() {
  source("Seminar3_1071.R")
  tabel = read.csv("Mortalitate/Mortalitate.csv", row.names = 2)
  k = select.list(
    choices = names(tabel),
    title = "Variabilele modelului",
    multiple = T,
    graphics = T
  )
  if (length(k) > 1) {
    rez = acp(setDate = tabel[k])
    tabelVarianta = centralizareVarianta(rez$Varianta)
    #View(tabelVarianta)
    R = rez$Corelatii
    rownames(R) = k
    #View(R)
    write.csv(x = tabelVarianta,
              file = "DistributieVarianta.csv",
              row.names = T)
    write.csv(x = R,
              file = "CorelatiiFactoriale.csv",
              row.names = T)
    plotValoriProprii(alpha = rez$Varianta)
    C = rez$Componente
    plotInstante(
      X = C[, 1],
      Y = C[, 2],
      etichete = rownames(tabel),
      titlu =
        "Plot instante - Componente",
      k1 = "Comp 1",
      k2 = "Comp 2"
    )
    cerculCorelatiilor(
      X = R[, 1],
      Y = R[, 2],
      etichete = k,
      k1 = "Componenta 1",
      k2 = "Componenta 2"
    )
    CS = scoruri(C = C, alpha = rez$Varianta)
    write.csv(x = CS,
              file = "Scoruri.csv",
              row.names = T)
    plotInstante(
      X = CS[, 1],
      Y = CS[, 2],
      etichete = rownames(tabel),
      titlu = "Plot instante - Scoruri",
      k1 = "S1",
      k2 = "S2"
    )
  }
}

------------------------------------
#cercul corelatiilor - analiza canonica
cerculCorelatiilor = function(X,
                              titlu = "",
                              titluX = "",
                              titluY = "") {
  windows(width = 9, height = 7)
  #dev.new(noRStudioGD = T)
  par(mai = c(1, 1, 1, 2),xpd=T)
  t = seq(from = 0,
          to = 2 * pi,
          length.out = 100)
  plot(
    x = cos(t),
    y = sin(t),
    main = titlu,
    xlab = titluX,
    ylab = titluY,
    type = "l"
  )
  m = length(X)
  c = rainbow(m)
  for (i in 1:m) {
    points(x = X[[i]][, 1],
           y = X[[i]][, 2],
           col = c[i])
    text(
      x = X[[i]][, 1],
      y = X[[i]][, 2],
      pos = 1,
      labels = rownames(X[[i]]),
      col = c[i]
    )
  }
  #legend(x = 1.1,y = 1,legend = names(X),fill = c)
  legend(x = 1.2, y = 1, legend = names(X), fill = c)
}
----------------------------------------------------
#analiza canonica
seminar = function(){
  set1 = xlsx::read.xlsx(file = "EnergieEU/BalantaEnergeticaEuropa.xlsx",sheetIndex = 2)
  set2 = xlsx::read.xlsx(file = "EnergieEU/BalantaEnergeticaEuropa.xlsx",sheetIndex = 3)
  k1 = select.list(choices = names(set1),title = "Variabile set 1",multiple = T,
                   graphics = T)
  if(length(k1)<2){
    stop("Insuficiente variabile selectate!")
  }
  k2 = select.list(choices = names(set2),title = "Variabile set 2",multiple = T,
                   graphics = T)
  if(length(k2)<2){
    stop("Insuficiente variabile selectate!")
  }
  rezCCA=CCA::cc(X = set1[k1],Y = set2[k2])
  r = rezCCA$cor
  r2 = r*r
  n = nrow(set1)
  p = length(k1)
  q=length(k2)
  m = min(p,q)
  df = p:(p-m+1)*q:(q-m+1)
  pvalue = numeric(m)
  for(i in 1:m){
    lambda = prod(1-r2[i:m])
    chi2 = (-n+1+(p+q+1)/2)*log(lambda)
    pvalue[i] = pchisq(q = chi2,df = df[i],lower.tail = F)
  }
  semnificatieRadacini = data.frame(R = r, R2= r2, Grade.libertate=df, P.Values=pvalue,
                                    row.names = paste("Root",1:m))
  #Corelatiile variabile-variabile canonice
  rxz = rezCCA$scores$corr.X.xscores
  ryu = rezCCA$scores$corr.Y.yscores
  rxu = rezCCA$scores$corr.X.yscores
  ryz = rezCCA$scores$corr.Y.xscores
  rezultate = list(SemnificatieRadacini = semnificatieRadacini,
                   Corelatii.X.z = rxz,Corelatii.Y.u = ryu,
                   Corelatii.X.u=rxu,Corelatii.Y.z=ryz)
  library(xlsx)
  wb = createWorkbook()
  nrez = length(rezultate)
  for(j in 1:nrez){
    sh = createSheet(wb = wb,sheetName = names(rezultate)[[j]])
    addDataFrame(x = rezultate[[j]],sheet = sh,startRow = 1)
  }
  saveWorkbook(wb = wb,file = "CCA_rez.xlsx")
  
  coordonate = list(
    rxz=rxz[,1:2],ryu=ryu[,1:2],rxu=rxu[,1:2],ryz=ryz[,1:2]
  )
  cerculCorelatiilor(X = coordonate,titlu = "Reprezentare corelatii in spatiile 1 si 2",
                     titluX = "Radacina canonica 1 (z1/u1)",titluY = "Radacina canonica 2 (z2/u2)")
}

---------------------------------------------------------------
seminar4 = function(){
  setDate = read.csv(file = "Teritorial/Teritorial.csv",row.names = 1)
  k1= select.list(choices = names(setDate),title = "Variabile set 1",
                  multiple = T,graphics = T)
  if(length(k1)<2){
    stop("Numar prea mic de variabile!!!!")
  }
  k2= select.list(choices = names(setDate),title = "Variabile set 2",
                  multiple = T,graphics = T)
  if(length(k2)<2){
    stop("Numar prea mic de variabile!!!!")
  }
  varC = intersect(k1,k2)
  if(length(varC)!=0){
    stop("Suprapunere de variabile!!!!")
  }
  rezCCA = CCA::cc(X = setDate[k1],Y = setDate[k2])
  n = nrow(setDate)
  p = length(k1)
  q = length(k2)
  R = rezCCA$cor
  m = length(R)
  R2 = R*R
  #Aplicare test Bartlett pentru semnificatia radacinilor canonice
  df = p:(p-m+1)*q:(q-m+1)
  p.values = numeric(m)
  for(i in 1:m){
    lambda = prod(1-R2[i:m])
    chi2 = (-n+1+(p+q+1)/2)*log(lambda)
    p.values[i] = pchisq(q = chi2,df = df[i],lower.tail = F)
  }
  rezTest = p.values<0.01
  if(!is.element(el = TRUE,rezTest)){
    stop("Nu exista legatura intre seturi!")
  }
  testSemnificatie = data.frame(R=R,R2=R2,Grade.Libertate=df,Probabilitati=p.values,
                                row.names = paste("Root",1:m))
  rxz = rezCCA$scores$corr.X.xscores
  ryu = rezCCA$scores$corr.Y.yscores
  rxu = rezCCA$scores$corr.X.yscores
  ryz = rezCCA$scores$corr.Y.xscores
  rezultateAnaliza = list(
    Analiza.Semnificatie.Radacini = testSemnificatie,
    Corelatii.X.Z = rxz,
    Corelatii.Y.U = ryu,
    Corelatii.X.U = rxu,
    Corelatii.Y.Z = ryz
   )
  if(!"package:xlsx"%in%search()){
    library(xlsx)
  }
  wb = createWorkbook()
  nrRez=length(rezultateAnaliza)
  for(i in 1:nrRez){
    sh = createSheet(wb = wb,sheetName = names(rezultateAnaliza)[i])
    addDataFrame(x = rezultateAnaliza[[i]],sheet = sh,startRow = 1)
  }
  saveWorkbook(wb = wb,file = "Out_CCA.xlsx")
  
  coordCorel = list(
    Corelatii.X.Z = rxz[,1:2],
    Corelatii.Y.U = ryu[,1:2],
    Corelatii.X.U = rxu[,1:2],
    Corelatii.Y.Z = ryz[,1:2]
  )
  #Cercul corelatiilor. Dati click in grafic pentru afisarea legendei
  cerculCorelatiilor(X = coordCorel,titlu = "Corelatii variabile-variabile canonice",
                     titluX = "z1/u1",titluY = "z2/u2")
}

--------------------------------------------------------------------
#ceva cu acp
source("prelucrari.R")
library(zoom)

#citire fisier date
mortalitate <- citireDate("Mortalitate.csv", 2)
View(mortalitate)

#inlocuire celule NA
mortalitate <- eliminareNA(mortalitate)
View(mortalitate)

#extragere coloane cu date
setm <- mortalitate[,2:length(mortalitate)]
setm
names(setm)

write.csv(setm,"SetMortalitateFinal.csv")
#Aplicare test de concordanta Shapiro
m <- ncol(setm)
pvalues = numeric(m)
pvalues <- testNormalitate(setm)
View(pvalues)
write.csv(data.frame(names(setm),pvalues),"ShapiroTest.csv")

R <- cor(setm)
View(R)
write.csv(R,"R.csv")
pca <- princomp(setm,cor="TRUE")
pca
valp

write.csv(valp$values,"ValoriProprii.csv")
write.csv(valp$vectors,"VectoriiProprii.csv")
names(pca)
#calcul variante
pca$sdev*pca$sdev

plot(pca,type="lines")

#Afisarea componentelor principale
compp <- pca$scores
write.csv(compp,"ComponentePrincipale.csv")
dev.new()
plot(compp[,1],compp[,2],xlab="C1",ylab="C2")
text(compp[,1],compp[,2],rownames(compp),col="red",pos=3,cex=0.7)

#Calcul corelatii factoriale
corfact <- cor(setm,compp)
write.csv(corfact,"CorelatiiFactoriale.csv")
n <- nrow(setm)
cosin <- array(0,c(n,m))
for(i in 1:n){
	for(j in 1:m){
		cosin[i,j]=compp[i,j]*compp[i,j]/sum(compp[i,]*compp[i,])
	}
}
write.csv(cosin,"Cosinusuri.csv")
contrib <- array(0,c(n,m))
for(i in 1:n){
	for(j in 1:m){
		contrib[i,j]=compp[i,j]*compp[i,j]/sum(compp[,j]*compp[,j])
	}
}
write.csv(contrib,"Contributii.csv")
dev.new()
a <- seq(0,2*pi,length=100)
plot(cos(a),sin(a),type="l",col="blue",xlab="C1",ylab="C2")
text(corfact[,1],corfact[,2],rownames(corfact),col="red",cex=0.7)

---------------------------------------------------
#functii analiza discriminant
testareModel = function(X,g,q){
  n = nrow(X)
  WSS = DiscriMiner::withinSS(variables = X,group = g)/(n-q)
  BSS = DiscriMiner::betweenSS(variables = X,group = g)/(q-1)
  F = diag(BSS)/diag(WSS)
  pValues = pf(q = F,df1 = q-1,df2 = n-q,lower.tail = FALSE)
  return(list(F=F,PValues = pValues))
}

analizaLiniaraDiscriminanta = function(X,g){
  ald = MASS::lda(x = X,g)
  #Axe discriminante (vectorii proprii)
  b = ald$scaling
  #Normalizare axe
  b=sapply(X = as.data.frame(b),FUN = function(y){return(y/sqrt(sum(y*y)))})
  #Putere de discriminare variabile discriminante
  #svd reprezinta radical din puterea de discriminare
  lambda = ald$svd^2
  #Calcul variabile discriminante
  z = as.matrix(X)%*%b
  return(list(AxeD=b,PutereD=lambda,VarD = z))
}

plotScoruriDiscriminante = function(z1,z2,c1,c2,grupe,g){
  dev.new(noRStudioGD = T)
  plot(x = z1,y = z2,main="Distributia pe grupe in axele discriminante 1 si 2",type = "n",xlab = "z1",ylab = "z2")
  q = length(grupe)
  culori = rainbow(q)
  #Calcul centrii
  c1=numeric(q);c2=numeric(q)
  for (i in 1:q) {
    zg1 = subset(x = z1,g==grupe[i])
    zg2 = subset(x = z2,g==grupe[i])
    c1[i]=mean(zg1);c2[i]=mean(zg2)
    et = subset(names(z1),g==grupe[i])
    text(x = zg1,y = zg2,labels = et,col=culori[i])
  }
  for (i in 1:q) {
    text(x = c1[i],y = c2[i],labels = grupe[i],col=culori[i],cex = 3)
  }
}
------------------------------------------------------
#implementare analiza discriminant
seminar5 = function(){
  source("Functii.R")
  setAntrenament = read.csv(file = "ProiectBucuresti/ProiectB.csv",row.names=1)
  setTest = read.csv(file = "ProiectBucuresti/ProiectBEstimare.csv",row.names = 1)
  dVar = setAntrenament[,"VULNERAB"]
  setAntrenament[,"RiscZidarie"] = as.numeric(factor(setAntrenament[,"RiscZidarie"]))
  setAntrenament[,"RiscStructura"] = as.numeric(factor(setAntrenament[,"RiscStructura"]))
  grupe = levels(dVar)
  q = length(grupe)
  testModel = testareModel(X = setAntrenament[1:(length(setAntrenament)-2)],
                           g = dVar,q=q)
  write.csv(file = "TestModel.csv",x = testModel)
  ald = analizaLiniaraDiscriminanta(X =setAntrenament[1:(length(setAntrenament)-2)],
                                    g = dVar)
  write.csv(file = "VariabileDiscriminante.csv",x = ald$VarD)
  write.csv(file = "PutereDiscriminare.csv",x = ald$PutereD)
  write.csv(file = "AxeDiscriminante.csv",x = ald$AxeD)
  z1 = ald$VarD[,1]
  z2=ald$VarD[,2]
  names(z1)=paste("i",rownames(setAntrenament),sep = "")
  names(z2)=paste("i",rownames(setAntrenament),sep = "")
  plotScoruriDiscriminante(z1 = z1,z2 = z2,grupe = grupe,g = dVar)
}
