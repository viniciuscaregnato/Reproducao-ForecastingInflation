library(ipred)

runbagging=function(ind,df,variable,horizon)
  # ind: observar dataprep para saber mais
  # df: observar dataprep para saber mais
  # variable: nobservar dataprep para saber mais
  # horizon: observar dataprep para saber mais
  
  
  {
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  
  modelest=bagging(Xin,yin,R=100,l=5,pre.testing = "group-joint")
  #R=100: é o numero de modelos criados em bootstrapped agreggate que serao treinado
  #l=5: numero maximo de divisoes (em caso de arvores)
  # pre.testing = "group-joint": diz sobre a avaliação do desempenho
  
  forecast = predict(modelest,Xout)
  
  ## outputs
  
  nselect=modelest$coefficients
  nselect[nselect!=0]=1
  nselect[is.na(nselect)]=0
  nselect=colSums(nselect)
  
  outputs = list(nselect = nselect)
  
  return(list(forecast=forecast, outputs = outputs))
}
