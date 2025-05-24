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
  indmat=matrix(NA,window_size,nwindow)              # cria uma matriz indmat de tamanho (window_size × nwindow)
  indmat[1,]=1:ncol(indmat)                          # primeira linha são os indices iniciais de cada janela
  for(i in 2:nrow(indmat)){                          # demais indices de df para cada janela sao preenchidas
    indmat[i,]=indmat[i-1,]+1                        #
  }
  
  
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  """ aqui estamos aplicando a função sobre cada coluna(janela) de indmat """
  # indmat: a matriz que contem as janelas
  # 2: função será aplicada nas colunas(2), nao nas linhas(1)
  # fn: função que será aplicada
  # df= dataframe
  # horizon: horizonte de previsao
  # variable: variavel alvo
  """ apply retorna os resultados e previsoes das janelas, ou seja,
  rw <- list(
  list(forecast = ..., results = ...),  # resultado da 1ª coluna(janela)
  list(forecast = ..., results = ...),  # resultado da 2ª coluna(janela)
  list(forecast = ..., results = ...)   # resultado da 3ª coluna(janela)
            )"""
  
  forecast=unlist(lapply(rw,function(x)x$forecast))
  """ lapply() aplica a função " function(x){x$forecast} " a cada sublistas de rw,
  assim, ele consegue extrair $forecasts de cada sublista. Afinal, rw nao tem nehum retorno $forecast, 
  pois retorna apena uma lista, que contem listas"""
  
  outputs=lapply(rw,function(x)x$outputs)
  """ lapply() aplica a função "function(x){x$outputs}" para cada sublistas contidas em rw,
  assim, ele extrai examente o retorno $outputs das subslistas. afinal, rw pode apenas retornar
 uma unica lista, que contem sublistas de forecast e outputs."""  
    
  return(list(forecast=forecast, outputs=outputs))
  
}
