
library(caret)
library(glmnet)
library(stringr)
library(ggplot2)
library(ggcorrplot)
library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)   
library(latex2exp)

df.lasso1= read.csv('full_data.csv',header = T)
df.lasso1$asa.county[103] = "El paso"
df.lasso1$asa.county[270] = 'San Patricio'
df.lasso1$asa.county[298] = 'Tom Green'
df.lasso1$asa.county[207] = 'Miami-Dade'

asa.county.new = numeric(328)
for (i in 1:328) {
  bc=unlist(strsplit(df.lasso1$asa.county[i],' '))
  specified_strings =c("County","Parish","City","of")
  bc2=bc[!grepl(paste(specified_strings, collapse = "|"), bc)]
  if(length(bc2)==2){asa.county.new[i] = paste(bc2[1],bc2[2])}
  else{asa.county.new[i]=bc2}
}
df.lasso1$asa.county = asa.county.new
#df.lasso1$asa.county[103] = 'El paso County'
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
corr <- round(cor(df.lasso[c(1:17)]), 2)
show_df = function(data=df.lasso1){
  df.lasso = df.lasso1[,1:19]
  colnames(df.lasso) = c('County','States','Score_Infection','Score_Death',
                         'Under 5 Years','15 to 44 Years','65 Years and Over','75 Years and Over',
                         'Bachelor','Disability Rate','Employment','High Risk',
                         'Less than High School','No Computer','No Insurance','No Internet',
                         'Poverty','Black','White')
  sumdf = summary(df.lasso)
  
  return(sumdf)
}

consult_lasso = function(w=1,lambda=exp(seq(-2,0,0.1)),seed=9999,title=''){
  asa.state = df.lasso1$asa.states
  nfold=length(asa.state) #Leave-one-out cross-validation
  asa.counties = df.lasso1$asa.county
  x = as.matrix(df.lasso[,c(3:17)])
  set.seed(seed)
  y = w*y.infect + (1-w)*y.death
  cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "gaussian",nfolds = nfold,
                        lambda = lambda,grouped = F)
  idx.infect = which(coef(cv.lasso, cv.lasso$lambda.min)!=0)
  df = data.frame(variable = rownames(coef(cv.lasso, cv.lasso$lambda.min))[idx.infect],
                  coefficient =coef(cv.lasso, cv.lasso$lambda.min)[idx.infect])
  #results
  plot(cv.lasso)
  title(title,line = 3)
  
  #top 10 counties overperformed and underperformed
  score.county.pre = predict(cv.lasso,x,s="lambda.min")
  residual.county.score = y - score.county.pre 
  idx.best= numeric(10)
  idx.worst= numeric(10)
  for (i in 1:10) {
    idx.best[i] = which(rank(residual.county.score)==i)
    idx.worst[i] = which(rank(residual.county.score)==328-i+1)
  }
  
  result = paste(asa.state[idx.best],':',asa.counties[idx.best])
  result2 = paste(asa.state[idx.worst],':',asa.counties[idx.worst])
  result_full_name = numeric(10)
  result_full_name2 = numeric(10)
  for (i in 1:10) {
    result_full_name[i] = tolower(state.name[which(state.abb==asa.state[idx.best][i])])
    result_full_name2[i] = tolower(state.name[which(state.abb==asa.state[idx.worst][i])])
  }
  result_full_name_county = tolower(asa.counties[idx.best])
  result_full_name2_county = tolower(asa.counties[idx.worst])

  
  result_df = data.frame("overperformed_counties" = result,
                         "underperformed_counties" = result2,
                         "best_state" = result_full_name,
                         "best_counties" = result_full_name_county,
                         "worst_state"=result_full_name2,
                         "worst_counties" = result_full_name2_county)
  
  
  
  return(list(df = df, counties = result_df[,1:2],best_state=result_df[,3],
              best_county=result_df[,4],worst_state=result_df[,5],worst_county=result_df[,6]))
}

map_lasso = function(ovpstate,ovpcounty,udpstate,udpcounty){
  #ovpstate: overperformed state
  #ovpcounty: overperformed county
  #udpstate: underperformed state
  #udpcounty: underperformed county
  #include all states in the paper
  states = map_data("state")
  ut_df = subset(states, region %in% c("north carolina",'texas','louisiana','alabama',
                                        'arkansas','florida','georgia','kentucky','mississippi',
                                        'oklahoma','south carolina','tennessee','virginia'))
  
  counties = map_data("county")
  ut_county = subset(counties, region %in% c("north carolina",'texas','louisiana','alabama',
                                              'arkansas','florida','georgia','kentucky','mississippi',
                                              'oklahoma','south carolina','tennessee','virginia'))
  ut_base = ggplot(data = ut_df, mapping = aes(x = long, y = lat, group = 
                                                  group))+coord_fixed(1.3)+geom_polygon(color = "black", fill = "white")
  overperform_county = numeric(0)
  underperform_county = numeric(0)
  for (i in 1:10) {
    overperform_county = rbind(overperform_county,subset(ut_county, subregion==ovpcounty[i]&region==ovpstate[i]))
    underperform_county = rbind(underperform_county,subset(ut_county, subregion==udpcounty[i]&region==udpstate[i]))
  }
  
  # Fill the selected subregion with a predefined color and
  # plot a colored point with a specified long. and lat.
  p=ut_base + theme_void() +
    geom_polygon(data = ut_county, fill = NA, color = "white") +
    geom_polygon(color = "black", fill = NA) +
    geom_polygon(data = underperform_county, fill = "red", color = "black")+
    geom_polygon(data = overperform_county, fill = "green", color = "black")+
    ggtitle("Overperformed counties (green) and underperformed counties (red)")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  return(p)
}
counties_prj = read.csv('counties_prj.csv')
states_SE = read.csv('states_SE.csv')
mapw <- function(w, data=counties_prj, data_states=states_SE){
  if(w<0 | w>1){stop("w must be between 0 and 1")}
  if(w==0){legend_name = "Death Score"}
  else if(w==1){legend_name = "Infection Score"}
  else if(w==0.5){legend_name = "Total Score"}
  else{legend_name = "Score"}
  data$w_score <- w*data$IS + (1-w)*data$DS
  #x11()
  p <- ggplot(data = data,
              mapping = aes(x = long, y = lat, group = group))
  p + geom_polygon(col="black", aes(fill=w_score)) + 
    scale_fill_gradient(name=legend_name,low = "green",  high = "red") + 
    guides(color=FALSE) +
    geom_path(data = data_states, color="black", mapping = aes(long,lat))+xlab('Longitude')+ylab('Latitude')
}

#Figure 1
pdf('Death_Score.pdf')
mapw(0)
dev.off()
#Figure 2
pdf('Infection_Score.pdf')
mapw(1)
dev.off()
#Figure 3
pdf('Total_Score.pdf')
mapw(0.5)
dev.off()
#Figure 4
pdf('Correlation_plot.pdf',height = 8.23,width = 8.92)
ggcorrplot(corr,type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),lab=TRUE)
dev.off()
#Figure 5
####Load Data
TD2 <- read.csv("Fill_in_cities.csv")
TD2 <- TD2[,-1]

TD2$IS_scaled <- scale(TD2$score_infection)
TD2$DS_scaled <- scale(TD2$score_death)

####scatterplots
colnames(TD2)
TD2$TS <- .5*(TD2$IS_scaled + TD2$DS_scaled)
TD2_FL <- TD2[TD2$asa.states=="FL",]
pdf("Scatterplots.pdf")
par(mfrow=c(2,2))
plot(TD2$asa.65andOver,TD2$IS_scaled, xlab="", ylab = "")
abline(a=0.6373,b=-3.77,col="red",lty=2)
text(0.5,3,expression(paste(rho==-0.205)),cex=1.3)
title(ylab=expression(paste("Infection Score ",(w==1))), xlab="65 and Over", line=2.2, cex.lab=1.3)
plot(TD2$asa.65andOver,TD2$DS_scaled, xlab="", ylab = "")
abline(a=-0.531,b=3.141,col="red",lty=2)
text(0.5,4,expression(paste(rho==0.170)),cex=1.3)
title(ylab=expression(paste("Death Score ",(w==0))),xlab="65 and Over", line=2.2, cex.lab=1.3)
plot(TD2$asa.65andOver,TD2$TS, xlab="", ylab = "")
abline(a=0.053,b=-0.314,col="red",lty=2)
text(0.5,2,expression(paste(rho==-0.020)),cex=1.3)
title(ylab=expression(paste("Total Score ",(w==0.5))),xlab="65 and Over", line=2.2, cex.lab=1.3)
dev.off()
#Figure 6 
#ScreenShot
#Figure 7
pdf("LOOCV-Result.pdf")
par(mfrow=c(2,1))
p=consult_lasso(w=1,title = 'Infection Score (Weight = 1)')
p=consult_lasso(w=0,title = 'Death Score (Weight = 0)')
dev.off()

