getwd()
setwd("D:/LIPIKA_OFFICIALS/Edvencer/PROJECT R/PROJECT R/RETAIL PROJECT")
library(dplyr) 
library(randomForest) 
library(tidyr)
library(tree)
library(pROC)
library(cvTools)
library(car)
store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test=read.csv("store_test.csv",stringsAsFactors = F) 

store_test$store=NA
store_train$data='train'
store_test$data='test'
store_all=rbind(store_train,store_test)
glimpse(store_all)

sum(unique(table(store_all$state_alpha)))

store_all=store_all %>% 
  select(-state_alpha)

store_all=store_all %>% 
  select(-countyname)

store_all=store_all %>% 
  select(-countytownname)

store_all=store_all %>% 
  select(-Areaname)

store_all$population[is.na(store_all$population)]=round(mean(store_all$population,na.rm=T),0)
store_all$country[is.na(store_all$country)]=round(mean(store_all$country,na.rm=T),0)




CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

char_logical=sapply(store_all,is.character)
cat_cols=names(store_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','store'))]
cat_cols

for(col in cat_cols){
  store_all=CreateDummies(store_all,col,50)
}

glimpse(store_all)


store_all=store_all[!((is.na(store_all$store)) & store_all$data=='train'), ]
for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

store_train=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data,-store)


set.seed(2)
s=sample(1:nrow(store_train),0.75*nrow(store_train))
train_75=store_train[s,] 
test_25=store_train[-s,]  


for_vif=lm(store~.-Id-sales0-sales2-sales3-sales1,data=train_75)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

fit=glm(store~.-Id-sales0-sales2-sales3-sales1,data=train_75) #32 predictor var
fit=step(fit)


summary(fit)
formula(fit)

fit=glm(store ~ sales4 + CouSub + population + storecode_METRO12620N23019 + 
          storecode_METRO14460MM1120,data=train_75) #32 predictor var

library(pROC)
scoreLG=predict(fit,newdata =test_25,type = "response")
roccurve=roc(test_25$store,scoreLG) 
auc(roccurve)


#DT
DT= tree(as.factor(store)~.-Id,data=train_75)

DTscore=predict(DT,newdata=test_25,type="vector")[,2]
auc(roc(test_25$store,DTscore))


#RF
rf.model3= randomForest(as.factor(store)~.-Id,data=train_75)
test.score3=predict(rf.model3,newdata=test_25,type="prob")[,2]
auc(roc(test_25$store,test.score3))






library(cvTools)
##
store_train$store=as.factor(store_train$store)
glimpse(store_train)

param=list(mtry=c(3,4,6,8,10),
           ntree=c(50,100,200,500,700,800,900), 
           maxnodes=c(5,10,15,20,30,50,100,300,500,600,700),
           nodesize=c(1,2,5,10,20,30,40)       
)

mycost_auc=function(store,yhat){  #Real #Predicted
  roccurve=pROC::roc(store,yhat)
  score=pROC::auc(roccurve)
  return(score)
}  

subset_paras=function(full_list_para,n=10){  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}




num_trial=100
my_params=subset_paras(param,num_trial)
my_params

myauc=0


for(i in 1:num_trial){  
  params=my_params[i,]
  
  k=cvTuning(randomForest,
             store~.-Id, 
             data =store_train,
             tuning =params,
             folds = cvFolds(nrow(store_train), K=15, type ="random"),
             cost =mycost_auc, 
             seed =2,
             predictArgs = list(type="prob"))
  
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    
    myauc=score.this
    
    best_params=params
  }}
   myauc
   
   
   
   ci.rf.final=randomForest(store~.-Id,
                            mtry=best_params$mtry,
                            ntree=best_params$ntree,
                            maxnodes=best_params$maxnodes,
                            nodesize=best_params$nodesize,
                            data=store_train
   )
   
   
   test.score_final=predict(ci.rf.final,newdata=store_test, type="prob")[,2]
   write.csv(test.score_final,'Lipika_Biswas_P2_part2.csv',row.names = F)
   
   
   
   
   
   library(dplyr)
   
   library(tidymodels)
   library(visdat)
   library(tidyr)
   library(car)
   library(pROC)
   library(ggplot2)
   library(tidyr)
   library(fastDummies)
   getwd()
   setwd("D:/LIPIKA_OFFICIALS/Edvencer/PROJECT R/PROJECT R/RETAIL PROJECT")
   
   
   
   
  rg_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
  rg_test=read.csv("store_test.csv",stringsAsFactors = FALSE)
  store_train.csv
   
  View(rg_train) 
   glimpse(rg_train)
   
   vis_dat(rg_train)
   names(rg_train)
   
   table(rg_train$CouSub)
   
   sort(table(rg_train$storecode),decreasing = T)[1:10]
   length(unique(rg_train$state_alpha))
   
   storecode_func=function(x){
     x=substr(x,1,5)
     
     return(x)
   }
   
   
   dp_pipe=recipe(store ~ .,data=rg_train) %>% 
     update_role(Id,CouSub,countytownname,new_role = "drop_vars") %>%
     update_role(Areaname,countyname,state_alpha,store_Type,new_role="to_dummies") %>% 
     step_rm(has_role("drop_vars")) %>% 
     step_mutate_at(storecode,fn=storecode_func) %>% 
     update_role(storecode,new_role="to_dummies") %>% 
     step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
     step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
     step_dummy(has_role("to_dummies")) %>%
     step_impute_median(all_numeric(),-all_outcomes())
   
   dp_pipe
   
   dp_pipe=prep(dp_pipe)
   
   train=bake(dp_pipe,new_data = NULL)
   test=bake(dp_pipe,new_data=rg_test)
   
   vis_dat(train)
   
   
   set.seed(2)
   s=sample(1:nrow(train),0.8*nrow(train))
   t1=train[s,]
   t2=train[-s,]
   
   
   
   for_vif=lm(store~.-store_Type_X__other__-storecode_X__other__-sales0-sales2-sales3
              ,data=t1)
   
   sort(vif(for_vif),decreasing = T)[1:3]
   
   summary(for_vif)
   
   log_fit=glm(store~.-store_Type_X__other__-storecode_X__other__-sales0-sales2-sales3,data=t1,
               family = "binomial")
   
   summary(log_fit)
   log_fit=stats::step(log_fit)
   
   ####
   
   summary(log_fit)
   
   formula(log_fit)
   
   summary(log_fit)
   
   #### performance on t2 with auc score
   
   val.score=predict(log_fit,newdata = t2,type='response')
   
   pROC::auc(pROC::roc(t2$store,val.score))
   
   ### now fitting model on the entire data
   
   for_vif=lm(store~.-store_Type_X__other__-storecode_X__other__-state_alpha_X__other__
              -sales0-sales2-sales3,data=train)
   
   sort(vif(for_vif),decreasing=T)[1:3]
   
   log_fit.final=glm(store~.-store_Type_X__other__-storecode_X__other__-state_alpha_X__other__
                     -sales0-sales2-sales3,data=train,family="binomial")
   
   summary(log_fit.final)
   
   log_fit.final=stats::step(log_fit.final)
   
   summary(log_fit.final)
   
   #### performance on total data with auc score
   
   val.score_train=predict(log_fit.final,newdata = train,type='response')
   
   pROC::auc(pROC::roc(train$store,val.score_train))
   
   ### submission
   
   
   
   
   
   
   
   