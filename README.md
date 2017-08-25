# ScottKnottN
 ScottKnott packager function for balanced and unbalanced data 


x <- read.csv(file.choose(),sep=";",dec = ".",header = T)
r<- 3
x2 <- NULL
x1 <- (rep(t(c(x$Genotipo)), each = r))
for (i in 1:5) {
  x2 <- c(x2, as.matrix(x[i, (4:6)]))
}
 inputdata<- list(data.frame(cbind(x1,x2)))
names(inputdata[[1]]) <-  c("trat", "valor")
dados <- inputdata

KnottScottN <- function(dados) {
  resultado <- list()
  for(jj in 1:length(dados)){
    B0 = NULL
    lambda = NULL
    
    if(length(unique(dados[[jj]]$trat))>1){

      media <- sort(tapply(as.numeric(as.character(dados[[jj]]$valor)),list(dados[[jj]]$trat),mean))
      
      k <- length(is.element(dados[[jj]][,1],names(media)[1:length(media)]))/r
      
      modelo <- aov(dados[[jj]]$valor ~ dados[[jj]]$trat)
      anova <- summary(modelo)
      names(anova[[1]]) <-  c("df", "sum.sq", "mean.sq", "f.value", "p.value")
      v = anova[[1]]$df[2]
      
      if (anova[[1]]$p.value[1] < 0.05) {
        
        for (i in 1:(k - 1)) {
          B0[i] <-(((sum(media[1:i]) ^ 2) / i) + ((sum(media[-(1:i)]) ^ 2) / (k -i)) - ((sum(media) ^ 2) / k))
        }

        MQE <- anova1[[1]]$mean.sq[2] 
        Sy <- MQE / r
        mtot <- sum(media) / k
        
        sigma <- (1 / (k + v)) * ((sum((media - mtot) ^ 2) + (v * Sy)))
        
        for (i in 1:(k - 1)) {
          lambda[i] <- (pi / (2 * (pi - 2))) * (B0[i] / sigma)
        }
        
        p=which(lambda[1:i] > qchisq(.95, df=k))
        
        if (length(p)==0){
          lista=dados
        }else {
          lista=list(dados[[jj]][is.element(dados[[jj]][,1],names(media)[1:p]),],dados[[jj]][!is.element(dados[[jj]][,1],names(media)[1:p]),])
        }
      }
    }else{
      lista=(dados[[jj]])
    }
    browser()
    if(length(lista)>1){
      for(kk in 1:2){
        resultado[[length(resultado)+1]] <- lista[[kk]]
      }
    }else{
      resultado[length(resultado)+1] <- lista[[1]]
    }
  }
  
  return(resultado)
}

for(i in 1:length(dados)){

copia <- verdadeiros  
tamanhoinicio <- length(copia)
aa <- 2
while(aa>tamanhoinicio){
  tamanhoinicio <- length(copia)
  copia <- KnottScottN(copia)
  aa <- length(copia)
}


final <- KnottScottN(verdadeiros)
dados1 <-list(final[[1]])
dados2 <- list(dados1[[2]])
dados1 <- KnottScottN(dados1)
dados2 <- KnottScottN(dados2)
dados1.2 <- list(dados1[[2]])
dados1.2 <- KnottScottN(dados1.2)
}

library(ScottKnott)
?ScottKnott
sk1 <- with(dados[[1]],
            SK(x=as.numeric(as.character(dados[[1]]$valor)),
               y=dados[[1]]$trat,
               model='x ~ y',
               which='y'))
summary(sk1)
