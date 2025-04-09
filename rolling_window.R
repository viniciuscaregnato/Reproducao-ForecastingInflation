rolling_window=function(fn,df,nwindow=1,horizon,variable,...)
  
  # fn: função que será aplicada à rolling window
  # df: dataframe
  # nwindow: numero de janelas que se pode observar um termo fora
  # horizon: horizonte de previsao
  # variable: variavel alvo
  # ...: demais paramertos que podem ser inclusos
  

  {
  ind=1:nrow(df)                                     # ind: indices das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  window_size=nrow(df)-nwindow                       # window_size: tamanho da janela
  indmat=matrix(NA,window_size,nwindow)              # cria uma matriz indmat de tamanho (window_size × nwindow), para cada rolling window
  indmat[1,]=1:ncol(indmat)                          # primeira linha é preenchida com os valores da primeira janela
  for(i in 2:nrow(indmat)){                          # demais janelas sao preenchidas
    indmat[i,]=indmat[i-1,]+1                        #
  }
  
  
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  # aqui estamos aplicando a função sobre a rolling window #
  # indmat: a matriz que contem as rolling window
  # 2: função será aplicada nas colunas(2), nao nas linhas(1)
  # fn: função que será aplicada
  # df= dataframe
  # horizon: horizonte de previsao
  # variable: variavel alvo
  
  forecast=unlist(lapply(rw,function(x)x$forecast))
  # lapply() aplica a função function(x) ao termo rw, definido acima
  
  outputs=lapply(rw,function(x)x$outputs)
  return(list(forecast=forecast, outputs=outputs))
  
}
