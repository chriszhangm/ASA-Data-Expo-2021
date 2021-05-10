
library(caret)
library(glmnet)
library(stringr)
library(ggplot2)
library(ggcorrplot)

df.lasso1= read.csv('full_data.csv',header = T)
#remove states,counties.
colnames(df.lasso1)[3:107] = str_replace_all(colnames(df.lasso1)[3:107],pattern = 'asa_','')
df.lasso1$score_infection = scale(df.lasso1$score_infection)
df.lasso1$score_death = scale(df.lasso1$score_death)
df.lasso1 = df.lasso1[,-3]

df.lasso = df.lasso1[,-c(1:2)]
y.infect = df.lasso[,1]
y.death = df.lasso[,2]
#scale the data
y.infect = scale(y.infect)
y.death = scale(y.death)

show_df = function(data=df.lasso1,with_interation = FALSE){
  if (with_interation) {
    df.lasso = df.lasso1
  }
  else{
    df.lasso = df.lasso1[,1:19]
  }
  return(df.lasso)
}

consult_lasso = function(w=1,with_interation = FALSE,nfold=10,lambda=exp(seq(-2,0,0.1)),seed=9999,title=''){
  asa.state = df.lasso1$asa.states
  asa.counties = df.lasso1$asa.county
  
  if (with_interation) {
    x = as.matrix(df.lasso[,3:104])
  }
  else{x = as.matrix(df.lasso[,3:17])}
  set.seed(seed)
  y = w*y.infect + (1-w)*y.death
  cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "gaussian",nfolds = nfold,
                        lambda = lambda)
  idx.infect = which(coef(cv.lasso, cv.lasso$lambda.min)!=0)
  df = data.frame(variable = rownames(coef(cv.lasso, cv.lasso$lambda.min))[idx.infect],
                  coefficient =coef(cv.lasso, cv.lasso$lambda.min)[idx.infect])
  #results
  plot(cv.lasso)
  title(title,line = 3)
  
  #top 10 counties overperformed and underperformed
  score.county.pre = predict(cv.lasso.death,x,s="lambda.min")
  residual.county.score = y - score.county.pre 
  idx.best= numeric(10)
  idx.worst= numeric(10)
  for (i in 1:10) {
    idx.best[i] = which(rank(residual.county.score)==i)
    idx.worst[i] = which(rank(residual.county.score)==328-i+1)
  }
  
  result = paste(asa.state[idx.best],':',asa.counties[idx.best])
  result2 = paste(asa.state[idx.worst],':',asa.counties[idx.worst])
  
  result_df = data.frame("overperformed_counties" = result,
                         "underperformed_counties" = result2)
  
  
  return(list(df = df, result_df = result_df))
}

consult_lasso(w=1)

corr <- round(cor(df.lasso[1:17]), 2)


