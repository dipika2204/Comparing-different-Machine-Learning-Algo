trial = read.csv("wine.csv")
str(trial)
trial
#Dividing the data
library(caTools)
no=mydata[,1]
boolean_wine = sample.split(no,SplitRatio = 2/3,group = NULL)
Train.x=cbind(MALIC.ACID,MAGNESIUM,TOTAL.PHENOLS,NONFLAVANOID.PHENOLS,PROLINE,HUE)[boolean_wine,]
Test.x=cbind(MALIC.ACID,MAGNESIUM,TOTAL.PHENOLS,NONFLAVANOID.PHENOLS,PROLINE,HUE)[!boolean_wine,]
Train.output=trial$TYPE[boolean_wine]
length(Train.output)
Test.output=trial$TYPE[!boolean_wine]
length(Test.output)
#converting into 3 types
trial1=replace(Train.x$TYPE, Train.x$TYPE==2, 0)
new_trial_type1=replace(trial1, tria1==3, 0)
new_trial_type1
trial2=replace(Train.x$TYPE, Train.x$TYPE==1, 0)
new_trial_type2=replace(trial2, trial2==3, 0)
new_trial_type2=replace(new_trial_type2, new_trial_type2==2, 1)
new_trial_type2
trial3=replace(Train.x$TYPE, Train.x$TYPE==1, 0)
new_trial_type3=replace(trial2, trial2==2, 0)
new_trial_type3=replace(new_trial_type3, new_trial_type3==3, 1)
new_trial_type3
#applying the GLM function on the 3
glm.trial=glm((new_trial)~MALIC.ACID+ALCALINITY.OF.ASH+TOTAL.PHENOLS+NONFLAVANOID.PHENOLS+COLOR.INTENSITY+OD280.OD315.OF.DILUTED.WINES,data=trial,family = binomial)
glm.trial1=glm((new_trial_type2)~MALIC.ACID+ALCALINITY.OF.ASH+TOTAL.PHENOLS+NONFLAVANOID.PHENOLS+COLOR.INTENSITY+OD280.OD315.OF.DILUTED.WINES,data=trial,family = binomial)
glm.trial2=glm(new_trial_type3~MALIC.ACID+ALCALINITY.OF.ASH+TOTAL.PHENOLS+NONFLAVANOID.PHENOLS+COLOR.INTENSITY+OD280.OD315.OF.DILUTED.WINES,data=trial,family = binomial)
summary ( glm.trial )
summary ( glm.trial1 )
summary ( glm.trial2 )
# individual prediction
T1=predict(glm.trial,trial,type = "response")
T2=predict(glm.trial1,trial,type = "response")
T3=predict(glm.trial2,trial,type="response")
# Weighted mean of the 3
wt_mean=1:178
for(k in 1:178){
wt_mean[k]=weighted.mean(T1[k],T2[k],T3[k])
}
Print("Probabilites:")
wt_mean
