#Libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

requiredPackages <- c("tidyverse","knitr","lubridate","caret","gridExtra","Rborist")
lapply(requiredPackages, library, character.only = TRUE)


#Create Video Games set
videogames_data <- read.csv("https://raw.githubusercontent.com/eduardogdr/VideoGames/main/Video_Games_Sales_as_at_22_Dec_2016.csv")
class(videogames_data)
head(videogames_data)

#Data Cleanse 

#Dimensions of dataset
cat("The video games dataset has", nrow(videogames_data), "rows and", ncol(videogames_data), "columns.")
cat("There are", n_distinct(videogames_data$Name), "different video games and", n_distinct(videogames_data$Publisher), "different publishers in the video games dataset.")
cat("All sales are in millions of units sold.")


# overview of columns with NA
colSums(is.na(videogames_data))

videogames_data <- na.omit(videogames_data)
cat("After removing NA's the video games dataset now has", nrow(videogames_data), "rows.")

videogames_data <- videogames_data %>% filter( Year_of_Release != "N/A")

videogames_data$User_Score <- as.integer(videogames_data$User_Score)

#Overview of columns with blanks

colSums(videogames_data == '')

videogames_data <-videogames_data %>% filter(Developer != '' & Rating != '')
cat("After removing blanks the video games dataset now has", nrow(videogames_data), "rows.")

# Exploratory Data Analysis

summary(videogames_data)

#NA Sales per year of release
#North America is by far the most important market for the time period observed, although most recently there 
#is some convergence between North America and the European Union  from 2014-2016. North American Sales peaked 
#in 2008 and have exhibited a downward trend from there on

videogames_data %>% group_by(Year_of_Release) %>% 
  summarise(NA_Sales_=sum(NA_Sales), EU_Sales_=sum(EU_Sales),
            JP_Sales_=sum(JP_Sales), Other_Sales_=sum(Other_Sales)) %>%
  ggplot() +
  geom_line(aes(Year_of_Release, NA_Sales_, color = "NA", group=1))+
  geom_line(aes(Year_of_Release, EU_Sales_, color = "EU",group=1)) +
  geom_line(aes(Year_of_Release, JP_Sales_, color = "JP", group=1)) +
  geom_line(aes(Year_of_Release, Other_Sales_, color = "Other", group=1)) +
  labs(x = "Year of Release",
       y = "Sum of Sales (in millions of units)",
       color = "Legend") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_manual(values = c("NA" = "dark green", "EU" = "blue", 
                                "JP" = "red", "Other" = "yellow"))+
  ggtitle("Regional Sales per Year")


#NA Sales per platform 
# The X360 was the platform that had the most North American sales for the period observed(1985-2016).

videogames_data %>% group_by(Platform) %>% 
  summarise(NA_Sales = sum(NA_Sales)) %>% ggplot(aes(Platform, NA_Sales)) +
  geom_bar(stat="identity", fill="grey")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("North American Sales (million units)") +
  ggtitle("North American Sales per Platform 1985-2016")

#Additional data wrangling to consolidate platforms

Nintendo = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
Sony = c("PS","PS2","PSP","PS3","PS4","PSV")
Sega = c("GEN","SCD","DC","GG","SAT")
Microsoft = c("XB","X360", "XOne")
Other = c("2600","3DO","NG","PCFX","TG16","WS")
PC= c('PC')

videogames_data$Platform_Group[videogames_data$Platform %in% Nintendo] <- "Nintendo"
videogames_data$Platform_Group[videogames_data$Platform %in% Sony] <- "Sony"
videogames_data$Platform_Group[videogames_data$Platform %in% Microsoft] <- "Microsoft"
videogames_data$Platform_Group[videogames_data$Platform %in% Sega] <- "Sega"
videogames_data$Platform_Group[videogames_data$Platform %in% PC] <- "PC"
videogames_data$Platform_Group[videogames_data$Platform %in% Other] <- "Other"


#NA Sales per platform group
#Sony is the clear leader in Global Sales followed by Nintendo, and Microsoft

videogames_data %>% group_by(Platform_Group) %>% 
  summarise(NA_Sales = sum(NA_Sales)) %>% ggplot(aes(Platform_Group, NA_Sales)) +
  geom_bar(stat="identity", fill="grey")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("North American Sales (million units)") +
  ggtitle("North American Sales per Platform Group 1985-2016")

#NA Sales per Developer
#EA Canada an EA Sports have the most releases, However they do not have the most North american sales 
#(it's Nintendo) even when added together. 

videogames_data %>% group_by(Developer) %>%
  summarise(NA_Sales = sum(NA_Sales)) %>% slice_max(NA_Sales, n=5)


#NA Sales per publisher
#In terms of publisher, Electronic Arts has the most North American sales followed by Nintendo

videogames_data %>% group_by(Publisher) %>%
  summarise(NA_Sales = sum(NA_Sales)) %>% slice_max(NA_Sales, n=5)

videogames_data %>% group_by(Publisher) %>%
  summarise(Releases = n()) %>% slice_max(Releases, n=5)

#NA Sales per game
#Wii Sports is the most sold game in North America in the data set

videogames_data %>% group_by(Name) %>%
  summarise(NA_Sales = sum(NA_Sales)) %>% slice_max(NA_Sales, n=5)

#NA Sales per genre
#Action genre by far had the most North American sales  

videogames_data %>% group_by(Genre) %>% 
  summarise(NA_Sales = sum(NA_Sales)) %>% ggplot(aes(Genre, NA_Sales)) +
  geom_bar(stat="identity", fill="steel blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("North American Sales (million units)") +
  ggtitle("North American Sales per Genre 1985-2016")

#NA Sales per critic score
#North American sales have a positive relationship with critic score

NA_Sales_Critic_Score <-videogames_data %>% group_by(Critic_Score) %>%
ggplot(aes(Critic_Score, NA_Sales))+
geom_point()+
scale_y_log10()+
geom_smooth(method = "lm")+
ggtitle("NA Sales per Critic Score")  

#NA Sales per user score
#North American sales have a positive relationship with user score, although it is 
#less strong than with critic score as the smoothing line is flatter

NA_Sales_User_Score <-videogames_data %>% group_by(User_Score) %>%
ggplot(aes(User_Score, NA_Sales))+
geom_point()+
scale_y_log10()+
geom_smooth(method = "lm")+
ggtitle("NA Sales per User Score")

#Grid arrange for NA Sales per critic score and NA Sales per user score

grid.arrange(NA_Sales_Critic_Score,NA_Sales_User_Score)

#Critic count per release year
#Critic count per year increases sharply in the 2000's and peaks in 2009 then trends downwards
#User count trends upward in the 2000's peaking in 2011 then trends downward

Critic_Count_year <- videogames_data %>% group_by(Year_of_Release) %>% 
  summarise(Critic_Count = sum(Critic_Count)) %>% ggplot(aes(Year_of_Release, Critic_Count)) +
  geom_bar(stat="identity", fill="steel blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("User Count") +
  ggtitle("Critic Count per Year")

#NA Sales per User Count

User_Count_year <- videogames_data %>% group_by(Year_of_Release) %>% 
  summarise(User_Count = sum(User_Count)) %>% ggplot(aes(Year_of_Release, User_Count)) +
  geom_bar(stat="identity", fill="grey")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("User Count") +
  ggtitle("User Count per Year")

#Grid arrange for Critic count and User count per release year

grid.arrange(Critic_Count_year,User_Count_year)


#NA Sales per critic count
# Sales by Critic count are more spread out than by User Count. User count also displays outliers  

  NA_Sales_Critic_Count <- videogames_data %>% group_by(Critic_Count) %>%
  ggplot(aes(Critic_Count, NA_Sales))+
  geom_point()+
  scale_y_log10()+
  geom_smooth(method = "lm")+
  ggtitle("NA Sales per Critic Count")  
  
  
  #NA Sales per user count
  
  NA_Sales_User_Count <- videogames_data %>% group_by(User_Count) %>%
    ggplot(aes(User_Count, NA_Sales))+
    geom_point()+
    scale_y_log10()+
    scale_x_log10()+
    geom_smooth(method = "lm")+
    ggtitle("NA Sales per User Count")
  
  #grid arrange for NA_Sales per critic count and user count
  
  grid.arrange(NA_Sales_Critic_Count,NA_Sales_User_Count)


# Model to predict North American sales, as they are the most important in terms of the proportion of Global sales
#using Genres, Critic score & count, user score & count.

set.seed(527)
test_index <- createDataPartition(y = videogames_data$NA_Sales, times = 1, p = 0.5, list = FALSE)
train_set <- videogames_data[-test_index,]
test_set <- videogames_data[test_index,]

#Naive Bayes Model

mu <- mean(train_set$NA_Sales)
mu

NB_Model <- RMSE(test_set$NA_Sales, mu)
cat("The resulting RMSE is", NB_Model)


#LM Model

fit_GLM <- train(NA_Sales ~ Critic_Score + User_Score + Platform + Genre + Critic_Count + User_Count,
       data=train_set,
       method="lm")

Results_2 <- predict(fit_GLM, test_set)

GLM_Model <- RMSE(Results_2, test_set$NA_Sales)
cat("The resulting RMSE is", GLM_Model)

#User and Critic count are the most important features, followed by critic score and platform Wii

GLM_Feature_Importance <- varImp(fit_GLM, scale = FALSE)
plot(GLM_Feature_Importance, main= "GLM")

#KNN

fit_knn <- knn3(NA_Sales ~ Critic_Score + User_Score + Platform + Genre + Critic_Count + User_Count, 
                data=train_set, k=10)
Results_3 <- predict(fit_knn, test_set)
Knn_Model <- RMSE(Results_3, test_set$NA_Sales)
cat("The resulting RMSE is", Knn_Model)

#Note: varImp does not support feature importance for knn model


#Random Forest
 
 fit_rf <- train(NA_Sales ~ Critic_Score + User_Score + Platform + Genre + Critic_Count + User_Count, 
                  data=train_set, method="Rborist", tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),)
 
 Results_4<- predict(fit_rf, test_set)
 RF_Model<- RMSE(Results_4, test_set$NA_Sales)
 cat("The resulting RMSE is", RF_Model)
 
#Critic count and user count are the most important feature, followed by Platform Wii and critic score
 
 rf_Feature_Importance <- varImp(fit_rf, scale = FALSE)
 plot(rf_Feature_Importance, main= "RF")
 
 #Classification Trees
 
 fit_rp <- train(NA_Sales ~ Critic_Score + User_Score + Platform + Genre + Critic_Count + User_Count, 
                 data=train_set, method="rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),)
 
 Results_5<- predict(fit_rp, test_set)
 RP_Model<- RMSE(Results_5, test_set$NA_Sales)
 cat("The resulting RMSE is", RP_Model)
 
 #User count and score, and Critic count and Score are the most important features in the rp model
 
 rp_Feature_Importance <- varImp(fit_rp, scale = FALSE)
 plot(rp_Feature_Importance, main= "RP")
 
 #Results Table
 
 results <- data.frame(Model="Naive Mean-Baseline Model", RMSE=NB_Model)
 results <- results %>% add_row(Model="GLM Model", RMSE=GLM_Model)
 results <- results %>% add_row(Model="knn Model", RMSE=Knn_Model)
 results <- results %>% add_row(Model="Random Forest Model", RMSE=RF_Model)
 results <- results %>% add_row(Model="Classification Trees", RMSE=RP_Model)
 
 results[order(results$RMSE),]
 
 
 