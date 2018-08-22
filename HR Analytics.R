
#############################################################
# Logistic Regression Assignment: Employee attrition Model
# Submitted By: Rakesh Reddy, Ganesh Varanasi
#               Krishnasurekha, Radhe Thakur
#############################################################

#Set Working Directory
    setwd("C:/Users/RadheThakur/Desktop/HR-Analytics")

#Load Libraries
    library(dplyr)
    library(reshape2)
    library(ggplot2)
    library(cowplot)
    library(MASS)


#############################################################
##      Load Data Files                                    ##
#############################################################
    Employee.Survey.Data<-read.csv("employee_survey_data.csv",stringsAsFactors = TRUE)
    General.Data<-        read.csv("general_data.csv",stringsAsFactors = TRUE)
    Manager.Survey.Data<- read.csv("manager_survey_data.csv",stringsAsFactors = TRUE)
    In_time.Data<-        read.csv("in_time.csv",stringsAsFactors = TRUE)
    Out_time.Data<-       read.csv("out_time.csv",stringsAsFactors = TRUE)

#############################################################
##      Calculate Avg Working Hours                        ##
#############################################################

#As dataset is not sorted checking the emp id in intime an out time 
    length(unique(In_time.Data$X))          #4410
    length(unique(Out_time.Data$X))         #4410  
    emp.id.in<-In_time.Data[,1]
    emp.id.out<-Out_time.Data[,1]
    sum(emp.id.in-emp.id.out) #0 Same EMP ID sorting, Can perform Vector Operation 
  
  
#Removing the Emp ID column from In and OUT Time dataframe
    In_time.Data<-In_time.Data[-1]
    Out_time.Data<-Out_time.Data[-1]

#Convert the format in Time
    in_time <- as.data.frame(sapply(In_time.Data, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
    out_time <- as.data.frame(sapply(Out_time.Data, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
    Office.Time<-out_time-in_time
#COnvert all the columns to numeric for Mean Calculation
    Office.Time<-as.data.frame(sapply(Office.Time,function(x) as.numeric(x)))
    Office.Time$AvgWorkHrs<-round(apply(Office.Time,1,mean,na.rm=TRUE),digits = 2)
    AvgWorkHrs<-data.frame("EmployeeID"= emp.id.in,"AvgWorkHrs"= Office.Time$AvgWorkHrs)
    
    
#Check for the Unique Employee in each data sets
    length(unique(Employee.Survey.Data$EmployeeID))#4410
    length(unique(General.Data$EmployeeID))#4410
    length(unique(Manager.Survey.Data$EmployeeID))#4410
#Merge all the data sets in a single file
    Attrition.Master.Data<-merge(Employee.Survey.Data,General.Data,by="EmployeeID")
    Attrition.Master.Data<-merge(Attrition.Master.Data,Manager.Survey.Data,by="EmployeeID")
    Attrition.Master.Data<-merge(Attrition.Master.Data,AvgWorkHrs,by="EmployeeID")
    

#############################################################
##                Data Cleansing                           ##
#############################################################
# Remove Columns having all same row Values
    cols.to.Remove<-apply(Attrition.Master.Data,2,function(x) length(unique(x)))
    cols.to.Remove<-names(cols.to.Remove)[which(cols.to.Remove==1)]
    Attrition.Master.Data<-Attrition.Master.Data[ , !names(Attrition.Master.Data) %in% cols.to.Remove] 
    
# Check for NA ,Blank and Duplicate
    
    sum(is.na(Attrition.Master.Data)) #111 Values in Total ~ 2% of Data
                                      #Removing the NA Value Rows from the data
    Attrition.Master.Data<-na.omit(Attrition.Master.Data)
    colSums(Attrition.Master.Data =="") #No Blank Values 
    sum(duplicated(Attrition.Master.Data))#No Duplicates
    
    

#############################################################
##         Convert Variables to Factors                    ##
#############################################################    

#Remove the Employee ID Column 
    
    Attrition.Master.Data<- Attrition.Master.Data[,-1]
    
#Columns to be converted to Factors
    
    Fact.Cols<-c("Education","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","JobLevel")
       
#Converting Columns to Factor as per Data dictionary
    
    for(f in 1:length(Fact.Cols)){
      
      Attrition.Master.Data[,Fact.Cols[f]] <- as.factor(Attrition.Master.Data[,Fact.Cols[f]])
    }  
    
    levels(Attrition.Master.Data$Education) <- c("Below College","College","Bachelor","Master","Doctor")
    levels(Attrition.Master.Data$EnvironmentSatisfaction) <- c("Low","Medium","High","Very High")
    levels(Attrition.Master.Data$JobInvolvement) <- c("Low","Medium","High","Very High")
    levels(Attrition.Master.Data$JobSatisfaction) <- c ("Low","Medium","High","Very High")
    levels(Attrition.Master.Data$WorkLifeBalance) <- c("Bad","Good","Better","Best")
    levels(Attrition.Master.Data$PerformanceRating) <-c ("Low","Good","Excellent","Outstanding")
    
  

#############################################################
##               Exploratory Data Analysis                 ##
#############################################################        
 #Plots of Employee Behaviour
      
      plot_grid(ggplot(Attrition.Master.Data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
                ggplot(Attrition.Master.Data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(),
                align = "h")   
#Plots of Organizational Behaviour 
      plot_grid(ggplot(Attrition.Master.Data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(), 
                ggplot(Attrition.Master.Data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=Department,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=Education,fill=Attrition))+ geom_bar(),
                align = "h") 
#Plots of Facts    
      plot_grid(ggplot(Attrition.Master.Data, aes(x=EducationField,fill=Attrition))+ geom_bar(), 
                ggplot(Attrition.Master.Data, aes(x=Gender,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=JobRole,fill=Attrition))+ geom_bar(),
                ggplot(Attrition.Master.Data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
                align = "h") 


#Function to Extract Numeric COlumn Names
      
      Specific_Class_Columns <- function(x,y){
        Col.Class<- sapply(x,class)
        Col.List<-grep(y,Col.Class)
        return((Col.List))
      }
      
      Numeric.Cols<- c(Specific_Class_Columns(Attrition.Master.Data,"integer")
                       ,Specific_Class_Columns(Attrition.Master.Data,"numeric"))  
      
#Adding Attrition column for plotting
      Plot.Data<-c(Numeric.Cols,5)
      
      
      
      #plot all the numeric columns against Attrition for Outliers
      Attrition.Master.Data.Numeric<-Attrition.Master.Data[ ,Plot.Data ]
      Attrition.Master.Data.Numeric<-melt(Attrition.Master.Data.Numeric,id.vars = "Attrition")
      ggplot(Attrition.Master.Data.Numeric,aes(Attrition,value))+
        geom_boxplot(outlier.colour = "red")+
        facet_wrap(~variable)+
        coord_cartesian(ylim = c(0,40))#Plot 
      
#############################################################
##                Outlier Treatment                        ##
#############################################################
      
      
      Outlier_Treatment<- Numeric.Cols
      
      # Function to fix Outliers
      Append_outliers <- function(x,Min_Cap=.05,Max_Cap=.95){
        qnt <- quantile(x, probs=c(.25, .75))
        max_and_min <- quantile(x, probs=c(Min_Cap, Max_Cap))
        Inter_Q_Range <- 1.5 * IQR(x)
        x[x < (qnt[1] - Inter_Q_Range)] <- max_and_min[1]
        x[x > (qnt[2] + Inter_Q_Range)] <- max_and_min[2]
      }
      
      #Outlier Treatment for All the analysed Column
      for(z in 1:length(Outlier_Treatment)){
        
        Attrition.Master.Data[,Outlier_Treatment[z]] <- sapply(Attrition.Master.Data[,Outlier_Treatment[z]], Append_outliers)
      }
      

#############################################################
##               Scaling Continous Variables               ##
#############################################################    
      
      for(a in 1:length(Numeric.Cols)){
        
        Attrition.Master.Data[,Numeric.Cols[a]] <- scale(Attrition.Master.Data[,Numeric.Cols[a]])
      }
#############################################################
##               Dummy Variable Creation                   ##
#############################################################   
      
      Two_level_Treatment<-function(x){
        levels(x)<-c(1,0) 
        x <- as.numeric(levels(x))[x] 
        
      }
      
#Convert Factor with 2 levels to numerical variables
# Gender, Attrition 
      
      level_2<-c('Gender','Attrition')
      
      Attrition.Master.Data[,level_2]<-sapply(Attrition.Master.Data[,level_2],Two_level_Treatment)
      

      #Extract Character Columns
      
Char.Cols<- Specific_Class_Columns(Attrition.Master.Data,"character")
Fact.Cols<- Specific_Class_Columns(Attrition.Master.Data,"factor")
Multilevel_Data<-cbind(Attrition.Master.Data[,Char.Cols],Attrition.Master.Data[,Fact.Cols])

length.multilevel.data<-apply(Multilevel_Data,2,function(x) length(unique(x)))
Multilevel_Dummy<-names(length.multilevel.data)[which(length.multilevel.data > 2)]
      
      
Empty_DF <- data.frame(matrix(ncol = 0, nrow = 4300)) # To Store the Dummy Variables

      Dummy_Multilevel <- function(x) {
        Dummy_Variable <- data.frame(model.matrix(~x,data = Multilevel_Data))
        Dummy_Ready<-Dummy_Variable[,-1]
        Empty_DF<-cbind(Empty_DF,Dummy_Ready)
        return(Dummy_Ready) 
      }
 
      Dummy_Combined<-as.data.frame(sapply(Multilevel_Data[,Multilevel_Dummy],Dummy_Multilevel))
      
      Attrition.Model.Data<-Attrition.Master.Data[ , !names(Attrition.Master.Data) %in% Multilevel_Dummy] 
      
      
# Combine the dummy variables and the numeric columns
      Attrition.Model.Data<-cbind(Attrition.Model.Data,Dummy_Combined)
      names(Attrition.Model.Data)<-gsub("x","",names(Attrition.Model.Data))# handle extra x added due to custom funciton      
      
      
#############################################################
##               Modelling                                 ##
#############################################################   
      

      
      set.seed(123)
      Training_indice<- sample(1: nrow(Attrition.Model.Data),(.7*nrow(Attrition.Model.Data)))
      Training_data<-Attrition.Model.Data[Training_indice,]
      Test_data<-Attrition.Model.Data[-Training_indice,]      
      
      
model1<-glm(Attrition~.,data = Training_data,family = "binomial")
summary(model1)
  
setp<-stepAIC(model1,direction="both") 


model2<-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
              YearsWithCurrManager + AvgWorkHrs + EnvironmentSatisfaction.Medium + 
              EnvironmentSatisfaction.High + EnvironmentSatisfaction.Very.High + 
              JobSatisfaction.Medium + JobSatisfaction.High + JobSatisfaction.Very.High + 
              WorkLifeBalance.Good + WorkLifeBalance.Better + WorkLifeBalance.Best + 
              BusinessTravel.Travel_Frequently + BusinessTravel.Travel_Rarely + 
              Department.Research...Development + Department.Sales + EducationField.Other + 
              JobLevel.5 + JobRole.Manager + JobRole.Manufacturing.Director + 
              JobRole.Research.Director + JobRole.Research.Scientist + 
              JobRole.Sales.Eecutive + MaritalStatus.Single + JobInvolvement.High,data = Training_data,family = "binomial")
summary(model2)

model3<-update(model2,~.-JobRole.Manager )
summary(model3)
     
      