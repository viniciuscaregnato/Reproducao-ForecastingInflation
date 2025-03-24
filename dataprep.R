View(data)
#abrir o arquivo "data.rda" antes de iniciar o trabalho

dataprep = function(ind,df,variable,horizon,add_dummy = TRUE, univar = FALSE, factonly = FALSE, nofact = FALSE)
{
  #ind= numero das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  #df = um data.frame
  #variable = nome da variavel do dataframe a ser analisada, será o y
  #
  df=df[ind,]
  y=df[,variable]
  
  if(nofact==TRUE){
    if(univar==FALSE){
      x=df
    }else{
      x = as.matrix(df[,variable])
    }
  }else{
    if(univar==FALSE){
      factors=princomp(scale(df))$scores[,1:4]
      if(factonly == TRUE){
        x = cbind(df[,variable],factors)
      }else{
        x=cbind(df,factors)
      }
    }else{
      x = as.matrix(df[,variable])
    }
  }
  
  
  X=embed(as.matrix(x),4) # X é a matriz x, com 4 defasagens
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] #remove as ultimas linhas, conforme variavel horizon q eu criar
  Xout=X[nrow(X),] #armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
  Xout=t(as.vector(Xout)) # garante que Xout seja de fato uma linha, n apenas um vetor
  yin=tail(y,nrow(Xin)) # yin pega o numero de linhas em Xin da variavel y definida la em cima através de y=df[,variable] 
  
  
  
  if("2008-11-01" %in% names(yin)){                 #se 2008-11-01 está no names(yin):
    
    dummy=rep(0,length(yin))                       # cria um vetor de zeros, chamado dummy, com a extensao de yin
    intervention=which(names(yin)=="2008-11-01")   # "intervention" apenas guarda o valor do indice de "2008-11-01"
    dummy[intervention]=1                          # tona a variavel "dummy" igual a 1, para o indice de internvention (????? confirmar)
    if(add_dummy == TRUE){
      Xin=cbind(Xin,dummy)
      Xout=cbind(Xout,0)
    }
    
  }else{                                           #se 2008-11-01 nao está no names(yin):
    dummy = rep(0,length(yin))                     # cria um vetor de zeros, chamado dummy, com a extensao de yin
    if(add_dummy == TRUE){                         # ainda, se add_dummy for TRUE na função:
      Xin=cbind(Xin,dummy)                         # adiciona coluna "dummy" à matriz "Xin"
      Xout=cbind(Xout,0)                           # adiciona uma coluna de zeros à matriz "Xout"
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}



#### APLICANDO A FUNÇÃO DATAPREP ####
data
