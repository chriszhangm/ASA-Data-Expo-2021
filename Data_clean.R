library(stringr)
library(tidyr)
states.asa = c('AL','AR','FL','GA','KY','LA','MS','NC','OK','SC','TN','TX','VA')

###############
###Education###
###############
filepath_edu = paste0('Education/ACSST1Y2019.S1501-',states.asa,'.csv')

asa.county = NULL
asa.bachelor = NULL
asa.lessHighSchool = NULL
asa.numberofCounty= NULL
for (j in filepath_edu) {
  r1 = read.csv(file = j,header = T)
  asa.numberofCounty = c(asa.numberofCounty,length(seq(2,dim(r1)[2]-13,by=12)))
  for (i in seq(2,dim(r1)[2]-13,by=12)) {
    asa.county = c(asa.county,strsplit(colnames(r1)[i],split = '\\.')[[1]][1])
    asa.lessHighSchool = c(asa.lessHighSchool,
                          sum(c(as.numeric(gsub(',','',r1[3,i])),as.numeric(gsub(',','',r1[8,i])),as.numeric(gsub(',','',r1[9,i])))))
    asa.bachelor = c(asa.bachelor,
                    sum(c(as.numeric(gsub(',','',r1[6,i])),as.numeric(gsub(',','',r1[16,i])))))
  }
}
asa.states = rep(states.asa,asa.numberofCounty)
###############
###Emplpyment##
###############

filepath_employ = paste0('Employment - Occupation/ACSST1Y2019.S2401-',states.asa,'.csv')
asa.employment = NULL
asa.highrisk = NULL

for (j in filepath_employ) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(2,dim(r1)[2]-11,by=10)) {
    asa.employment = c(asa.employment,as.numeric(gsub(',','',r1[1,i])))
    asa.highrisk = c(asa.highrisk,as.numeric(gsub(',','',r1[18,i])))
  }
}

###############
###Disability##
###############
filepath_disability = paste0('Health - Disability/ACSST1Y2019.S1810-',states.asa,'.csv')
asa.disabilityrate = NULL
asa.white = NULL
asa.black = NULL
for (j in filepath_disability) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(6,dim(r1)[2]-7,by=6)) {
    asa.disabilityrate = c(asa.disabilityrate,as.numeric(gsub('%','',r1[1,i]))/100)
  }
}
for (k in filepath_disability) {
  r1 = read.csv(file = k,header = T)
  for (i in seq(2,dim(r1)[2]-7,by=6)) {
    asa.white = c(asa.white,as.numeric(gsub(',','',r1[6,i])))
    asa.black = c(asa.black,as.numeric(gsub(',','',r1[7,i])))
  }
}
#N means very small numbers, so we impute NA with 0.
asa.black[which(is.na(asa.black))] = 0


###############
###Internet##
###############
filepath_internet = paste0('Internet/ACSST1Y2019.S2801-',states.asa,'.csv')
asa.nointernet = NULL
asa.nocomputer = NULL
asa.household = NULL
for (j in filepath_internet) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(2,dim(r1)[2]-5,by=4)) {
    asa.household = c(asa.household,as.numeric(gsub(',','',r1[1,i])))
    asa.nocomputer = c(asa.nocomputer,as.numeric(gsub(',','',r1[12,i])))
    asa.nointernet = c(asa.nointernet,as.numeric(gsub(',','',r1[21,i])))
  }
}

###############
###Population##
###############
filepath_pop = paste0('Population /ACSST1Y2019.S0101-',states.asa,'.csv')
asa.population = NULL
asa.under5yr = NULL
asa.15yrto44yr = NULL
asa.65andOver = NULL
asa.75andOver = NULL
for (j in filepath_pop) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(2,dim(r1)[2]-13,by=12)) {
    asa.population = c(asa.population,as.numeric(gsub(',','',r1[1,i])))
    asa.under5yr = c(asa.under5yr,as.numeric(gsub(',','',r1[3,i])))
    asa.15yrto44yr = c(asa.15yrto44yr,as.numeric(gsub(',','',r1[26,i])))
    asa.65andOver = c(asa.65andOver,as.numeric(gsub(',','',r1[32,i])))
    asa.75andOver = c(asa.75andOver,as.numeric(gsub(',','',r1[33,i])))
  }
}

###############
###Poverty####
###############
filepath_poverty = paste0('Poverty/ACSST1Y2019.S1701-',states.asa,'.csv')
asa.poverty = NULL
for (j in filepath_poverty) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(6,dim(r1)[2]-7,by=6)) {
    asa.poverty = c(asa.poverty,as.numeric(gsub('%','',r1[1,i]))/100)
  }
}
###############
###Insurance###
###############
filepath_insurance = paste0('Health - Total Insurance/ACSST1Y2019.S2701-',states.asa,'.csv')
asa.noinsurance = NULL
asa.county1 = NULL
for (j in filepath_insurance) {
  r1 = read.csv(file = j,header = T)
  for (i in seq(6,dim(r1)[2]-5,by=5)) {
    asa.noinsurance = c(asa.noinsurance,as.numeric(gsub('%','',r1[1,i]))/100)
    asa.county1 = c(asa.county1,strsplit(colnames(r1)[i],split = '\\.')[[1]][1])
  }
}
idx.la = which(asa.states == 'LA')
###############
###Merge###
###############
asa.county[328] = 'Virginia Beach'
asa.county[66] = "St. Johns"
asa.county[67] = "St. Lucie"
asa.county[137] = "St. Landry"
asa.county[138] = "St. Tammany"
asa.county =c(paste(asa.county[1:123],'County '),
              paste(asa.county[124:140],'Parish '),
              paste(asa.county[141:317],'County '),
              paste('City of',asa.county[318:328]))[1:328]



asa.total = cbind(asa.states,asa.county,asa.population,asa.under5yr,asa.15yrto44yr,asa.65andOver,asa.75andOver,asa.bachelor,
                  asa.disabilityrate,asa.employment,asa.highrisk,asa.household,
                  asa.lessHighSchool,asa.nocomputer,asa.noinsurance,asa.nointernet,
                  asa.poverty,asa.black,asa.white)

asa.total = data.frame(asa.total)

#####################
###Infection/Death###
#####################

infect.asa = read.csv('covid_confirmed_usafacts.csv',header = T)
death.asa = read.csv('covid_deaths_usafacts.csv',header = T)

####Change the name of Counties
##AL##
infect.asa$County.Name[59] = "St County "
##FL##
infect.asa$County.Name[360] = "Indian County "
infect.asa$County.Name[373] = "Miami County "
infect.asa$County.Name[380] = "Palm County "
infect.asa$County.Name[387] = "Santa County "
##LA##
infect.asa$County.Name[1149] = "East Parish "
##NC##
infect.asa$County.Name[1989] = "New County "
##TX
infect.asa$County.Name[2638] = "El County "
infect.asa$County.Name[2646] = "Fort County "
infect.asa$County.Name[2772] = "San County "
infect.asa$County.Name[2793] = "Tom County "
##VA
infect.asa$County.Name[2914] = "James County "
infect.asa$County.Name[2940] = "Prince County "
infect.asa$County.Name[2985] = "City of Newport"

#-----------------------------------------------------------#
##AL##
death.asa$County.Name[59] = "St County "
##FL##
death.asa$County.Name[360] = "Indian County "
death.asa$County.Name[373] = "Miami County "
death.asa$County.Name[380] = "Palm County "
death.asa$County.Name[387] = "Santa County "
##LA##
death.asa$County.Name[1149] = "East Parish "
##NC##
death.asa$County.Name[1989] = "New County "
##TX
death.asa$County.Name[2638] = "El County "
death.asa$County.Name[2646] = "Fort County "
death.asa$County.Name[2772] = "San County "
death.asa$County.Name[2793] = "Tom County "
##VA
death.asa$County.Name[2914] = "James County "
death.asa$County.Name[2940] = "Prince County "
death.asa$County.Name[2985] = "City of Newport"

#-----------------------------------------------------------#
idx.asa = NULL
for (i in 1:dim(infect.asa)[1]) {
  if (sum(infect.asa$State[i]==states.asa)==1) {
    idx.asa = c(idx.asa,i)
  }
}

#take monthly level data
labeltime = c(paste0('X2020.0',3:9,'.27'),
              paste0('X2020.',10:12,'.27'),
              paste0('X2021.0',1:3,'.27'))
timeidx = labeltime
temp.asa.infect = infect.asa[idx.asa,]
temp.asa.infect = temp.asa.infect[,-c(1,4)]

temp.asa.death = death.asa[idx.asa,]
temp.asa.death = temp.asa.death[,-c(1,4)]
#coltochoose.idx=sapply(1:13, function(x){which(colnames(temp.asa.infect) == timeidx[x])})

#temp.asa.infect = temp.asa.infect[,c(1,2,coltochoose.idx)]
colnames(temp.asa.infect)[1] = 'asa.county'
colnames(temp.asa.infect)[2] = 'asa.states'

#temp.asa.death = temp.asa.death[,c(1,2,coltochoose.idx)]
colnames(temp.asa.death)[1] = 'asa.county'
colnames(temp.asa.death)[2] = 'asa.states'


temp.merge.infect = merge(temp.asa.infect,asa.total,by=c('asa.county','asa.states'))
temp.merge.death = merge(temp.asa.death,asa.total,by=c('asa.county','asa.states'))

#Keep Only Time-related data
temp.merge.infect = temp.merge.infect[,1:433]
temp.merge.death = temp.merge.death[,1:433]

#write.csv(temp.merge.infect,'USASE_infect.csv')
#write.csv(temp.merge.death,'USASE_death.csv')

states.asa = c('AL','AR','FL','GA','KY','LA','MS','NC','OK','SC','TN','TX','VA')

df.infect = temp.merge.infect
df.death = temp.merge.death
df.infect[,5:435] = df.infect[,5:435]/df.infect[,4]
df.death[,5:435] = df.death[,5:435]/df.infect[,4]

weekly_states_level_inf = NULL
weekly_states_level_death = NULL

for(i in 1:13){
  weekly_states_level_inf=rbind(weekly_states_level_inf,as.numeric(apply(df.infect[which(df.infect$asa.states==states.asa[i]),seq(5,435,by=7)], 2, mean)))
  weekly_states_level_death=rbind(weekly_states_level_death,as.numeric(apply(df.death[which(df.death$asa.states==states.asa[i]),seq(5,435,by=7)], 2, mean)))
}
weekly_totalstates_level_inf = as.numeric(apply(df.infect[,seq(5,435,by=7)], 2, mean))
weekly_totalstates_level_death = as.numeric(apply(df.death[,seq(5,435,by=7)], 2, mean))

weekly_states_level_inf = as.data.frame(weekly_states_level_inf)
weekly_states_level_death = as.data.frame(weekly_states_level_death)
weekly_states_level_inf[,63] = states.asa
weekly_states_level_death[,63] = states.asa



df.weekly.infect = df.infect[,c(2,3,seq(5,435,by=7))]
df.weekly.death = df.death[,c(2,3,seq(5,435,by=7))]

#add national-wise infection rate
df.weekly.infect[,3:64] = 2*as.matrix(df.weekly.infect[,3:64]) - weekly_totalstates_level_inf
df.weekly.death[,3:64] = 2*as.matrix(df.weekly.death[,3:64]) - weekly_totalstates_level_death

#add statewise
for (i in 1:328) {
  df.idx = which(df.weekly.infect$asa.states[i] == weekly_states_level$V63)
  df.weekly.infect[i,3:64] = df.weekly.infect[i,3:64] - weekly_states_level_inf[df.idx,1:62]
  df.weekly.death[i,3:64] = df.weekly.death[i,3:64] - weekly_states_level_death[df.idx,1:62]
}

#create a new score variable
score_inf = apply(df.weekly.infect[,3:64],1,sum)
score_death = apply(df.weekly.death[,3:64],1,sum)
score_df = data.frame(states = df.death$asa.states,county = df.death$asa.county,score_inf = score_inf,score_death=score_death)





