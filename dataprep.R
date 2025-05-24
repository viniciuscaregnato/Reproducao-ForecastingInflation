View(data)
#abrir o arquivo "data.rda" antes de iniciar o trabalho

dataprep = function(ind,df,variable,horizon,add_dummy = TRUE, univar = FALSE, factonly = FALSE, nofact = FALSE)

  # ind: indices das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  # df: um data.frame
  # variable: nome da variavel do dataframe a ser analisada, será o y
  # horizon: horizonte de previsao, que determina quantas últimas observações serão retiradas da matriz X ao criar Xin e Xout
  # add_dummy (padrao TRUE): adiciona coluna "dummy" para a variavel "2008-11-01" na matriz "Xin" e uma coluna de zeros à matriz "Xout"
  # univar (padrao FALSE): caso for TRUE, os dados df[,variabele] sao transformados na matriz X, tornado a serie apenas determinada pelas suas defasagens
  #                        caso for FALSE, dai depende de factonly e nofact
  # factonly (padrao FALSE): só existe se nofactor for FALSE, caso for TRUE, é usado apenas fatores(primeiros 4 componentes principais) como variaveis
  # nofact (padrao FALSE): se for TRUE, ignora a extração de componentes pricipais PCA, e usa apenas os dados do modelo

{
  
  df=df[ind,]
  y=df[,variable]
  
  if(nofact==TRUE){                                  # (Se nofact = TRUE, nao há PCA
    if(univar==FALSE){                               # (e se univar = FALSE,
      x=df                                           # as variveis explicativas sao como df foi definido inicialmente)
    }else{                                           # (e univar = FALSE,
      x = as.matrix(df[,variable])                   # a variavel dependente se torna uma matriz X, para mais adiante receber as defasagens))
    }                                                #
  }else{                                             # (Se nofact = FALSE, há PCA
    if(univar==FALSE){                               # (e univar = FALSE,
      factors=princomp(scale(df))$scores[,1:4]       # é criado a variavel factors,
      if(factonly == TRUE){                          # (e factonly = TRUE, 
        x = cbind(df[,variable],factors)             # apenas a variable(y) e factors(x) fazem a matriz x)
      }else{                                         # (e factonly = FALSE,
        x=cbind(df,factors)                          # a tabela df é unida com a tabela factors)
      }                                              # )
    }else{                                           # (e univar = TRUE, 
      x = as.matrix(df[,variable])                   # (a variavel dependente se torna uma matriz X, para mais adiante receber as defasagens)))
    }                                                
  }
  
  
  X=embed(as.matrix(x),4)                            # X é a matriz x, com 4 defasagens
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            # remove as ultimas horizon linhas de X
  Xout=X[nrow(X),]                                   # armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
  Xout=t(as.vector(Xout))                            # garante que Xout seja de fato uma linha, n apenas um vetor
  yin=tail(y,nrow(Xin))                              # yin pega o numero de linhas de Xin da variavel y definida la em cima através de y=df[,variable] 
  
  
  
  if("2008-11-01" %in% names(yin)){                  # (se 2008-11-01 está no names(yin):
    
    dummy=rep(0,length(yin))                         # cria um vetor de zeros, chamado dummy, com a extensao de yin
    intervention=which(names(yin)=="2008-11-01")     # "intervention" apenas guarda o valor do indice de "2008-11-01"
    dummy[intervention]=1                            # tona a variavel "dummy" igual a 1, para o indice de internvention
    if(add_dummy == TRUE){                           # (ainda se, add_dumy == TRUE, 
      Xin=cbind(Xin,dummy)                           # adiciona coluna "dummy" à matriz "Xin"
      Xout=cbind(Xout,0)                             # adiciona uma coluna de zeros à matriz "Xout"))
    }
    
  }else{                                             # (se 2008-11-01 nao está no names(yin):
    dummy = rep(0,length(yin))                       # cria um vetor de zeros, chamado dummy, com a extensao de yin
    if(add_dummy == TRUE){                           # (ainda, se add_dummy for TRUE na função:
      Xin=cbind(Xin,dummy)                           # adiciona coluna "dummy" à matriz "Xin"
      Xout=cbind(Xout,0)                             # adiciona uma coluna de zeros à matriz "Xout"))
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}
