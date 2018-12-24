
library(Amelia)
library(ROCR)

HRA1 <- read.csv("D:/imarticus/Project/Attrition.csv",na.strings=c(""," ","NA"))
View(HRA1)
colSums(is.na(HRA1))
summary(HRA1)
names(HRA1)
dim(HRA)

library(ggplot2)

ggplot(HRA1, aes(x=Attrition)) + ggtitle("Attrition") + xlab("Attrition") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Count") + coord_flip() + theme_minimal()

summary(HRA1)


HRA1$Education <- as.factor(HRA1$Education)
HRA1$JobLevel <- as.factor(HRA1$JobLevel)
HRA1$PerformanceRating <- as.factor(HRA1$PerformanceRating)
HRA1$StockOptionLevel <- as.factor(HRA1$StockOptionLevel)

HRA <-HRA1
#________________________________SMOTE_______________________________
#Data Imbalanced Smote function

Classcount = table(HRA1$Attrition)
# Over Sampling
over = ( (0.6 * max(Classcount)) - min(Classcount) ) / min(Classcount)
# Under Sampling
under = (0.4 * max(Classcount)) / (min(Classcount) * over)

over = round(over, 1) * 100
under = round(under, 1) * 100
#Generate the balanced data set

library(DMwR)
HRA = SMOTE(Attrition~., HRA1, perc.over = 210, k = 5, perc.under = 100)
View(HRA)
table(HRA$Attrition)
table(HRA1$Attrition)

# let check the output of the Balancing

library(ggplot2)

ggplot(HRA, aes(x=Attrition)) + ggtitle("Attrition") + xlab("Attrition") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Count") + coord_flip() + theme_minimal()

summary(HRA)

#_____________________EDA____________________________________________________________
#converting numeric data into as.factors

#removing insignificant varaibles
HRA$EmployeeNumber<- NULL
HRA$StandardHours <- NULL
HRA$Over18 <- NULL
HRA$EmployeeCount <- NULL

summary(HRA)


#bivariate analysis
library(corrplot)
corrplot(cor(sapply(HRA,as.integer)),method = "pie")


#Feature Engineering

#adding all work related ratings 
HRA$TotlaSatisfaction <- 
  as.numeric(HRA$EnvironmentSatisfaction)+
  as.numeric(HRA$JobInvolvement)+
  as.numeric(HRA$JobSatisfaction)+
  as.numeric(HRA$RelationshipSatisfaction)+
  as.numeric(HRA$WorkLifeBalance)

View(HRA)

HRA <- HRA[,-c(9,12,15,23,27)]

HRA$AgeGroup <- as.factor(
  ifelse(HRA$Age<=24,"Young", ifelse(
    HRA$Age<=54,"Middle-Age","Adult"
  ))
)
table(HRA$AgeGroup)

HRA <- HRA[,-c(1)]
View(HRA)

summary(HRA)

HRA$Incomelevel <- as.factor(
  ifelse(HRA$MonthlyIncome<ave(HRA$MonthlyIncome),"Low","High")
)
table(HRA$Incomelevel)

#__________________________________GGPLOT______________________________________


library(magrittr)
library(ggplot2)
library(knitr)
library(ggthemes)
library(dplyr)
library(forcats)



numeric=HRA %>% dplyr::select(Age,DailyRate,DistanceFromHome,HourlyRate,MonthlyIncome,MonthlyRate,NumCompaniesWorked,PercentSalaryHike,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager,TotalWorkingYears,TrainingTimesLastYear,StockOptionLevel)
corrplot(cor(numeric),method="circle",type="upper")


## Distribution of Age
ggplot(HRA,aes(Age))+geom_histogram(binwidth=5,aes(y=..count..),fill="green4")+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Count",title="Distribution of Age")


#Age Distribution of people who leave
ggplot(HRA, aes(Age))+geom_histogram(binwidth=5,aes(y=round(((..count..)/sum(..count..))*100,2)),fill="red")+theme_few()+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Percentage",title="Age distribution of people who leave")+scale_y_continuous(limits=c(0,30),breaks=seq(0,30,5))+scale_x_continuous(limits=c(15,60),breaks=seq(15,60,5))

#Age Distribution of people who stay
ggplot(HRA,aes(Age))+geom_histogram(binwidth=5,aes(y=round(((..count..)/sum(..count..))*100,2)),fill="green4")+theme_few()+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Percentage",title="Age distribution of people who Stay")+scale_y_continuous(limits=c(0,30),breaks=seq(0,30,5))+scale_x_continuous(limits=c(15,60),breaks=seq(15,60,5))

#salary with gender
ggplot(HRA,aes(Gender,MonthlyIncome,fill=Gender))+geom_boxplot()+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=10))+labs(x="Gender",y="Salary",title="Salary with Gender")+scale_fill_canva(palette="Neon and bold")+coord_flip()

#Attrition count with gender
ggplot(HRA,aes(MaritalStatus,..count..,fill=Attrition))+geom_bar(position=position_dodge())+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16))+labs(title="Attrition Count Vs Marital Status")

#Attrition wrt change in Age income and MartialStatus
ggplot(HRA,aes(Age,MonthlyIncome,size=Age,col=factor(Attrition)))+geom_point(alpha=0.3)+theme_minimal()+facet_wrap(~MaritalStatus)+labs(x="Age",y="MonthlyIncome",title="Attrition Level Comparision ",subtitle="How attrition is observed with change in Age,Income and MaritalStatus",col="Attrition")+theme(legend.position="bottom",plot.title=element_text(size=16,hjust=0.5),plot.subtitle = element_text(size=10))+scale_color_brewer(palette="Set2")

#Attrition vs department
ggplot(HRA,aes(x=Department,group=Attrition))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+facet_grid(~Attrition)+theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+labs(x="Department",y="Percentage",title="Attrition  % Vs Department")+ geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) 

#department vs attrition %
ggplot(HRA,aes(x=Attrition,group=Department))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+facet_grid(~Department)+theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+labs(x="Attrition",y="Percentage",title="Department Vs Attrition %")+ geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) 


## Attrition Vs Distance From Home:
ggplot(HRA,aes(DistanceFromHome,fill=Attrition))+geom_density(alpha=0.5)+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16))+labs(x="Distance from Home",title="Attrition Vs Distance From Home")+scale_fill_canva(palette="Bold feature colors")

## Attrition Vs Business Travel
ggplot(HRA,aes(BusinessTravel,fill=Attrition))+geom_bar(stat="count",aes(y=..count..),position=position_dodge())+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16),axis.text.x = element_text(angle=90))+labs(x="Travel Frequency",y="Count",title="Attrition Vs Business Travel")

## Attrition Vs Payrates
g1=ggplot(HRA,aes(Attrition,DailyRate,fill=Attrition))+geom_boxplot()+theme_few()+theme(plot.title=element_text(hjust=0.5),legend.position="bottom")+scale_y_continuous(limits=c(100,1500),breaks=seq(100,1500,100))+coord_flip()+labs(title="Attrition Vs Daily Wages")
g1
g2=ggplot(HRA,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()+theme_few()+theme(plot.title=element_text(hjust=0.5),legend.position="bottom")+coord_flip()+labs(title="Attrition Vs Monthly Income")
g2
g3=ggplot(HRA,aes(Attrition,HourlyRate,fill=Attrition))+geom_boxplot()+theme_few()+theme(plot.title=element_text(hjust=0.5),legend.position="bottom")+coord_flip()+labs(title="Attrition Vs Hourly Wages")
g3

### Percentage of salary hike
ggplot(HRA,aes(PercentSalaryHike,..count..,fill=Attrition))+geom_histogram(binwidth=5)+theme_few()+theme(plot.title=element_text(hjust=0.5),legend.position="none")+labs(title="Histogram of SalaryHike")+scale_y_continuous(limits=c(0,1500),breaks=seq(0,1500,150))


#percentage of hike vs Years of experience
ggplot(HRA,aes(TotalWorkingYears,PercentSalaryHike,col=factor(Attrition),size=PercentSalaryHike))+geom_point(alpha=0.6)+theme(legend.position="bottom",plot.title = element_text(size=15,hjust=0.5))+labs(title="Percentage of Hike Vs Years of Experience",col="Attrition")

# Years at company VS Percentage of hike
ggplot(HRA,aes(YearsAtCompany,PercentSalaryHike,size=PercentSalaryHike))+geom_point(color="purple",alpha=0.5)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=16))+labs(title="Years at Company Vs Percentage of Hike")


### Which role is paid more?
temp=HRA %>% group_by(JobRole) %>% summarise(salary=median(MonthlyIncome)) %>% arrange(desc(salary))
ggplot(temp,aes(factor(JobRole,levels=JobRole),salary))+geom_bar(stat="identity",fill="gold4")+coord_polar()+labs(x="Job Role",y="Median Salary",title="Who gets more??")+theme_few()+theme(axis.text.x=element_text(vjust=300),plot.title=element_text(hjust=0.5,size=16),axis.text.y=element_blank())


#Attrition by job role
ggplot(HRA,aes(x=reorder(JobRole,Attrition),y=Attrition)) + geom_bar(stat='identity',alpha=0.5,fill="red") + theme_fivethirtyeight()+coord_flip()+theme(axis.text.x=element_text(angle=0,vjust=0.5),legend.position='bottom',plot.title = element_text(size=12)) +labs(title="Attrition Rate by Job Role")


## Education,EducationField:
temp= HRA %>% mutate(Education=factor(Education)) %>% mutate(Education=fct_recode(Education,'Below College'='1','College'='2','Bachelor'='3','Master'='4','Doctor'='5'))
ggplot(temp,aes(Education,fill=Attrition))+geom_bar(stat="count",aes(y=..count..),position=position_dodge())+theme_few()+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16),axis.text.x = element_text(angle=90))+labs(x="Education Level",y="Count",title="Trend of Attrition with Education Level")+scale_fill_canva(palette="Golden afternoon")

# Education levels and field of eduaction
ggplot(temp,aes(Education,fill=Attrition))+geom_bar(stat="count",aes(y=..count..),position=position_dodge())+theme_few()+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16),axis.text.x = element_text(angle=90))+labs(x="Education Level",y="Count",title="Education levels and field of education")+scale_fill_canva(palette="Unique and striking")+facet_grid(~EducationField)


##Education Vs Satisfaction Levels Vs Attrition:
temp %>% mutate(JobSatisfaction=factor(JobSatisfaction)) %>% mutate(JobSatisfaction=fct_recode(JobSatisfaction,"Low"="1","Medium"="2","High"="3","Very High"="4")) %>% ggplot(aes(Education,fill=JobSatisfaction))+geom_bar(stat="count",position = position_dodge())+theme_few()+facet_wrap(~Attrition)+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16),axis.text.x = element_text(angle=90,hjust=0.5))+labs(x="Education",y="Satisfaction Count",title="Comparing attrition with Education")

## Number of companies worked:
temp = HRA %>% group_by(Attrition,NumCompaniesWorked) %>% tally(sort=TRUE)
ggplot(temp,aes(NumCompaniesWorked,n,fill=Attrition,label=n))+geom_bar(stat="identity",position=position_dodge())+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16))+labs(x="Number of Companies",y="Count",title="Number of Companies worked")+coord_cartesian(xlim=c(0,9))+scale_x_continuous(breaks=seq(0,9,1))


g1=ggplot(HRA,aes(Attrition,TotalWorkingYears,fill=Attrition))+geom_boxplot()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5))+labs(x="Attrition",y="Years of Experience",title="Attrition trend with number of years of experience")+coord_flip()
g1
g2=HRA %>% filter(Attrition=="Yes") %>% ggplot(aes(TotalWorkingYears,..count..))+geom_histogram(binwidth=5,alpha=0.8,fill="#575da9")+labs(x="Years of Experience",y="Count",title="Histogram of Years of experience",subtitle="Attrition=Yes")+theme_few()+theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.3))
g2
g3=HRA %>% filter(Attrition=="No") %>% ggplot(aes(TotalWorkingYears,..count..))+geom_histogram(binwidth=5,alpha=0.8,fill="#336b87")+labs(x="Years of Experience",y="Count",title="Histogram of Years of experience",subtitle="Attrition=No")+theme_few()+theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.3))
g3


# Attrition Vs Categorical Variables:

#job involvement vs attrition rates
temp = HRA %>% mutate(JobInvolvement=factor(JobInvolvement)) %>% mutate(JobInvolvement=fct_recode(JobInvolvement,"Low"="1","Medium"="2","High"="3","Very High"="4"))
ggplot(temp,aes(x=JobInvolvement,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="Job Involvement",y="Percentage",title="Job Involvement Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

## Job Satisfaction
temp = HRA %>% mutate(JobSatisfaction=factor(JobSatisfaction)) %>% mutate(JobSatisfaction=fct_recode(JobSatisfaction,"Low"="1","Medium"="2","High"="3","Very High"="4"))
ggplot(temp,aes(x=JobSatisfaction,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="Job Satisfaction",y="Percentage",title="Job Satisfaction Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

## Performance Rating:
temp= HRA %>% mutate(PerformanceRating=factor(PerformanceRating)) %>% mutate(PerformanceRating=fct_recode(PerformanceRating,"Low"="1","Good"="2","Excellent"="3","Outstanding"="4"))
ggplot(temp,aes(x=PerformanceRating,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="PerformanceRating",y="Percentage",title="Performance Rating Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)


## Relationship Satisfaction:
temp= HRA %>% mutate(RelationshipSatisfaction=factor(RelationshipSatisfaction)) %>% mutate(RelationshipSatisfaction=fct_recode(RelationshipSatisfaction,"Low"="1","Medium"="2","High"="3","Very High"="4"))
ggplot(temp,aes(x=RelationshipSatisfaction,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="RelationshipSatisfaction",y="Percentage",title="RelationshipSatisfaction Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)


## Worklife balance:
temp= HRA %>% mutate(WorkLifeBalance=factor(WorkLifeBalance)) %>% mutate(WorkLifeBalance=fct_recode(WorkLifeBalance,"Bad"="1","Good"="2","Better"="3","Best"="4"))
ggplot(temp,aes(x=WorkLifeBalance,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="WorkLifeBalance",y="Percentage",title="Worklifebalance Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

## Environment Satisfaction:
temp= HRA %>% mutate(EnvironmentSatisfaction=factor(EnvironmentSatisfaction)) %>% mutate(EnvironmentSatisfaction=fct_recode(EnvironmentSatisfaction,"Low"="1","Medium"="2","High"="3","Very High"="4"))
ggplot(temp,aes(x=EnvironmentSatisfaction,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="EnvironmentSatisfaction",y="Percentage",title="Environment satisfaction Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)


## Attrition Vs OverTime:
ggplot(HRA,aes(x=OverTime,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="Overtime",y="Percentage",title="Overtime Vs Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)



#_________________________________________Splitting______________________________________
library(caTools)
set.seed(010)
splitHRA<-sample.split(HRA$Attrition, SplitRatio = 0.80)
trainHRA<-subset(HRA,splitHRA==T)
testHRA<-subset(HRA,splitHRA==F)

summary(HRA)

#_________________________________________DECESION TREES____________________________________

library(rpart)
modelHRADT2 <- rpart(formula = Attrition ~., data=trainHRA)
plot(modelHRADT2)
text(modelHRADT2)


#validation


HRADT2_pred = predict(modelHRADT2, newdata = testHRA, type = 'class')
class(HRADT1_pred)
class(testHRA$Attrition)

# confusion matrix
cmHRADT2 = table(HRADT2_pred, testHRA$Attrition)
cmHRADT2

library(caret)
cfHRADT2<-confusionMatrix(HRADT2_pred,testHRA$Attrition)
cfHRADT2


Raw.rf.plot<- plot.roc(as.numeric(testHRA$Attrition), as.numeric(HRADT2_pred),lwd=2, type="b",print.auc=TRUE,col ="blue")

#_________________________________RANDOM FOREST_______________________________

library(data.table)
library(dplyr)
library(DT)
library(gridExtra)
library(ggplot2)
library(caret)
library(Metrics)
library(randomForest)
library(pROC)
library(e1071)
library(DMwR)

View(HRA)
summary(HRA)
#fitting random forest classification to the training set
library(randomForest)
randomforestHRA2 = randomForest(x = trainHRA[-1],y = trainHRA$Attrition, ntree = 50)

#predicting the test set results
randomHRA_pred2 = predict(randomforestHRA2,newdata = testHRA[-1],type="response")
#making the confucion matrix
cmHRArandom2 = table(testHRA$Attrition,randomHRA_pred2)
acc(cmHRArandom2)

library(caret)
cfHRARF2<-confusionMatrix(randomHRA_pred2,testHRA$Attrition)
class(randomHRA_pred2)
class(testHRA$Attrition)
cfHRARF1


Raw.rf.prd <- predict(randomforestHRA2, newdata = testHRA)
confusionMatrix(testHRA$Attrition, Raw.rf.prd)
Raw.rf.plot<- plot.roc(as.numeric(testHRA$Attrition), as.numeric(Raw.rf.prd),lwd=2, type="b",print.auc=TRUE,col ="blue")


varImpPlot(randomforestHRA2)

#_________________________________LOGISTIC_______________________________________


modelHRAL12<-glm(Attrition~.,family=binomial(link="logit"),data=trainHRA)
summary(modelHRAL12)

modelHRAL13<-glm(Attrition~BusinessTravel+DistanceFromHome+JobRole+MaritalStatus+NumCompaniesWorked+OverTime+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+TotlaSatisfaction,family=binomial(link="logit"),data=trainHRA)
summary(modelHRAL13)

#testing

#validation of our model using validation set
# if type = response is not mentioned it will take log(odd(probability)), its for backtransforming it to categorical variable
fitted.resultsHRAL13 <- predict(modelHRAL12,newdata=testHRA[,-1],type='response')
#Thresholding
fitted.resultsHRAL13<- ifelse(fitted.resultsHRAL13 > 0.5,1,0)

#plotting auc curve

HRALP13 <- predict(modelHRAL10, newdata=testHRA[,-1], type="response")
HRALpr13 <- prediction(fitted.resultsHRAL13, testHRA[,1])
HRALprf13 <- performance(HRALpr13, measure = "tpr", x.measure = "fpr")
plot(HRALprf13)

HRALaucp13 <- performance(HRALpr13, measure = "auc")
HRALaucp13 <- HRALaucp13@y.values[[1]]
HRALaucp13



#___________________________SVM______________________________________


library(e1071)

#model building
HRAclassifier10<-svm(formula =Attrition~.,data=trainHRA,type = 'C-classification')
summary(HRAclassifier10)
HRAclassifier11<-svm(formula =Attrition~.,data=trainHRA,type = 'C-classification',gamma=0.5,cost=4)
summary(HRAclassifier11)


# SVM based based on grid scearch
tunesvmHRA=tune(svm,Attrition~.,
                data=trainHRA,
                ranges = list(gamma=2^(-1:1),cost=2^(2:9)))
summary(tunesvmHRA)


HRAclassifier12<-svm(formula =Attrition~.,data=trainHRA,type = 'C-classification',kernel="linear")
summary(HRAclassifier12)
HRAclassifier13<-svm(formula =Attrition~.,data=trainHRA,type = 'C-classification',kernel="sigmoid")
summary(HRAclassifier13)
HRAclassifier14<-svm(formula =Attrition~.,data=trainHRA,type = 'C-classification',kernel="polynomial")
summary(HRAclassifier14)

View(HRA)
#validation data
#validation of our model using validation set
# if type = response is not mentioned it will take log(odd(probability)), its for backtransforming it to categorical variable
fitted.resultssvmHRA10 <- predict(HRAclassifier10,newdata=testHRA[,-1])
fitted.resultssvmHRA11 <- predict(HRAclassifier11,newdata=testHRA[,-1])
fitted.resultssvmHRA12 <- predict(HRAclassifier12,newdata=testHRA[,-1])
fitted.resultssvmHRA13 <- predict(HRAclassifier13,newdata=testHRA[,-1])
fitted.resultssvmHRA14 <- predict(HRAclassifier14,newdata=testHRA[,-1])


#Confusion matrix
svmcfHRA10<-table(fitted.resultssvmHRA10 , testHRA[,1])
svmcfHRA11<-table(fitted.resultssvmHRA11 , testHRA[,1])
svmcfHRA12<-table(fitted.resultssvmHRA12 , testHRA[,1])
svmcfHRA13<-table(fitted.resultssvmHRA13 , testHRA[,1])
svmcfHRA14<-table(fitted.resultssvmHRA14 , testHRA[,1])


#function for accuracy for logistic radial without cost and gamma
acc<-function(svmcfHRA10){
  Totp<-svmcfHRA10[2,1]+svmcfHRA10[2,2]
  TP<-svmcfHRA10[2,2]
  c<-TP/Totp
  c
}
acc(svmcfHRA10)


#function for accuracy for logistic radial wit cost and gamma
acc<-function(svmcfHRA11){
  Totp<-svmcfHRA11[2,1]+svmcfHRA11[2,2]
  TP<-svmcfHRA11[2,2]
  c<-TP/Totp
  c
}
acc(svmcfHRA11)


#function for accuracy for logistic linear
acc<-function(svmcfHRA12){
  Totp<-svmcfHRA12[2,1]+svmcfHRA12[2,2]
  TP<-svmcfHRA12[2,2]
  c<-TP/Totp
  c
}
acc(svmcfHRA12)


#function for accuracy for logistic sigmoid
acc<-function(svmcfHRA13){
  Totp<-svmcfHRA13[2,1]+svmcfHRA13[2,2]
  TP<-svmcfHRA13[2,2]
  c<-TP/Totp
  c
}
acc(svmcfHRA13)


#function for accuracy for logistic polynomial
acc<-function(svmcfHRA14){
  Totp<-svmcfHRA14[2,1]+svmcfHRA14[2,2]
  TP<-svmcfHRA14[2,2]
  c<-TP/Totp
  c
}
acc(svmcfHRA14)


#plotting auc curve for linear
HRAsvmp11 <- predict(HRAclassifier10, newdata=testHRA[,-1])
HRAsvmp11 <- as.numeric(HRAsvmp11)
testHRA$Attrition <-as.numeric(testHRA$Attrition)
HRAsvmpr11 <- prediction(HRAsvmp11, testHRA[,1])
HRAsvmprf11 <- performance(HRAsvmpr11, measure = "tpr", x.measure = "fpr")
plot(HRAsvmprf11)

HRAaucsvmp11 <- performance(HRAsvmpr11, measure = "auc")
HRAaucsvmp11 <- HRAaucsvmp11@y.values[[1]]
HRAaucsvmp11


#_______________________NAIVE BAYES_____________________________________
library(e1071)

#model building on train data
library(e1071)
HRAnaivem11 <- naiveBayes(Attrition~., data=trainHRA)
dim(HRAnaivem11)
summary(HRAnaivem11)


#validation data

HRANaive_pred11 = predict(HRAnaivem11, newdata = testHRA)
cmHRANaive11 = table(HRANaive_pred11, testHRA$Attrition)
library(caret)
cfNaiveHRA11<-confusionMatrix(HRANaive_pred11,testHRA$Attrition)
cfNaiveHRA11


class(HRANaive_pred11)
class(testHRA$Attrition)
testHRA$Attrition <- as.numeric(testHRA$Attrition)

#__________________________XGB__________________________________
library(xgboost)
library(plyr)
library(DMwR)

fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 20,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)


HRAXGBmodel <- train(Attrition~., data = trainHRA,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)

HRAXGBprd <- predict(HRAXGBmodel,testHRA)
confusionMatrix(HRAXGBprd, testHRA$Attrition)
XGB.plot <- plot.roc (as.numeric(testHRA$Attrition), as.numeric(HRAXGBprd),lwd=2, type="b", print.auc=TRUE,col ="blue")


