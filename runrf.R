
library(randomForest)

runrf=function(ind,df,variable,horizon)
   
   # ind: indices das linhas q serao usadas 
   # df: o dataframe
   # variable: a variavel dependente
   # horizon: o horizonte de previsao

  {
  prep_data = dataprep(ind,df,variable,horizon)     # arquivo configurado conforme o dataprep informado
  Xin = prep_data$Xin                               # remove as linhas conforme o horizon
  yin = prep_data$yin                               # ajusta o numero de linhas igual Xin
  Xout = prep_data$Xout                             # adiciona uma coluna de zeros (dummies)
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)  # aplica rf aos parametros e cria o modelo modelest
  forecast=predict(modelest,Xout)                   # aplica modelest aos dados Xout        
  
  ## outputs
  importance = randomForest::importance(modelest)   # resultados
  outputs = list(importance = importance)           # 
  
  return(list(forecast=forecast, outputs = outputs))