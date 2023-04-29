library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plspm)
library(cluster)
library(caTools)
library(factoextra)
library(Rtsne)
library(NbClust)
library(ROSE)


#Loading the data set
df = read_csv("dataset.csv")
View(df)

glimpse(df)
colnames(df)

#Checking for missing values 
missing_values = sapply(df,function(x) sum(is.na(x)))
missing_values #no missing values

#Checking for duplicates 
df = df[!duplicated(df), ] #
View(df) #no duplicates

str(df)


#removing variables
df = df[,-c(2,3,7,8,9,10,11,20,21,22,23,24,25,26,27,28,29,30,31)]




#unique values
unique_values = list()
for (col in names(df)) {
  unique_values[[col]] <- unique(df[[col]])
}

unique_values

#renaming
setnames(df, old = c('Marital status', "Previous qualification",'Daytime/evening attendance','Course','Displaced','Educational special needs', 'Debtor', 'Tuition fees up to date', 'Gender','Scholarship holder','Age at enrollment', 'International', 'Unemployment rate', 'Inflation rate'), 
         new = c('marital_status', 'prev_qualification','attendance','course','displaced','edu_special_needs', 'debtor','tuition','gender','scholarship', 'age', 'international', 'UR', 'IR'))



###Response Variable###

#getting counts of target variable
target_counts = count(df, Target)
target_counts

# Create a subset of the data with "graduate" and "dropout" categories
academic_df = filter(df, Target %in% c("Graduate", "Dropout"))

# Create a subset of the data with "enrolled" category
enrolled_df = filter(df, Target == "Enrolled")

View(academic_df)






###Predictor Variables###


###Marital Status

marital_counts = count(academic_df, marital_status)
marital_counts

academic_df$marital_status = ifelse(academic_df$marital_status %in% c(1), "Single",
                      ifelse(academic_df$marital_status %in% c(2), "Married", "Complicated"))

academic_df$marital_status = factor(academic_df$marital_status)


                                            

###Nationality
nat_counts = count(academic_df, nationality)
nat_counts   

academic_df$nationality = ifelse(academic_df$nationality %in% c("1"), "Portuguese", "Other")

academic_df$nationality = factor(academic_df$nationality)   



###Daytime/Evening
academic_df$attendance = ifelse(academic_df$attendance %in% c(1), "Daytime", "Evening")

att_counts = count(academic_df, attendance)
att_counts  




###Previous Qualification

academic_df$prev_qualification = ifelse(academic_df$prev_qualification %in% c(1), "Secondary Education",
                                    ifelse(academic_df$prev_qualification %in% c(2,3,4,5,6,15,17), "Higher Education",
                                           ifelse(academic_df$prev_qualification %in% c(7,8,9,10,11), "High School", "Other")))


academic_df$prev_qualification = factor(academic_df$prev_qualification)   

aca_counts = count(academic_df, prev_qualification)
aca_counts  


###course

course_counts = count(academic_df, course)
course_counts


# Recode the "course" variable
academic_df$course <- ifelse(academic_df$course %in% c("1", "2", "5"), "Technology and Design",
                          ifelse(academic_df$course %in% c("4", "6", "8"), "Agriculture and Veterinary",
                                 ifelse(academic_df$course %in% c("9", "11", "14"), "Business and Management",
                                        ifelse(academic_df$course %in% c("15", "16", "13"), "Communication and Education",
                                               ifelse(academic_df$course %in% c("10", "12", "17", "3", "7"), "Other", "Unknown")))))

table(academic_df$course)

academic_df$course = factor(academic_df$course)


###Factoring 0,1 variables
academic_df$displaced = factor(academic_df$displaced, labels = c("No", "Yes"))
academic_df$edu_special_needs = factor(academic_df$edu_special_needs, labels = c("No", "Yes"))
academic_df$debtor = factor(academic_df$debtor, labels = c("No", "Yes"))
academic_df$tuition = factor(academic_df$tuition, labels = c("No", "Yes"))
academic_df$scholarship = factor(academic_df$scholarship, labels = c("No", "Yes"))
academic_df$international = factor(academic_df$international, labels = c("No", "Yes"))
academic_df$gender = factor(academic_df$gender, labels = c("Female", "Male"))



#write.csv(academic_df, file = "academic_cleaned.csv", row.names = TRUE)



#############Descriptive Analysis####################

summary(academic_df)

#univariate

ggplot(academic_df, aes(x = Target)) +
  geom_bar() +
  labs(title = "Counts of Target Variable", x = "Target", y = "Count") +
  scale_x_discrete(labels = c("Dropout", "Graduate"))

target_counts = count(academic_df, Target)
target_counts


ggplot(academic_df, aes(x = "", fill = Target)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Target Categories", fill = "Target") +
  theme_void()


boxplot(academic_df$age, main = "Boxplot of Age")



#bivariate


boxplot(GDP ~ Target, data = academic_df, 
        main = "Boxplot of GDP by Target", 
        xlab = "Target", ylab = "GDP")

boxplot(IR ~ Target, data = academic_df, 
        main = "Boxplot of Inflation Rate by Target", 
        xlab = "Target", ylab = "Inflation Rate")

boxplot(UR ~ Target, data = academic_df, 
        main = "Boxplot of Unemployement by Target", 
        xlab = "Target", ylab = "Unemployment Rate")


boxplot(age ~ Target, data = academic_df, 
        main = "Boxplot of Age by Target", 
        xlab = "Target", ylab = "Age")



#
prop_gender = prop.table(table(academic_df$gender, academic_df$Target), margin = 1)

ggplot(as.data.frame(prop_gender), aes(fill = Var2, y = Freq, x = Var1)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Gender", y = "Proportion", fill = "Target")


#
prop_debtor = prop.table(table(academic_df$debtor, academic_df$Target), margin = 1)

ggplot(as.data.frame(prop_debtor), aes(fill = Var2, y = Freq, x = Var1)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Debtor", y = "Proportion", fill = "Target")

#
prop_schol = prop.table(table(academic_df$scholarship, academic_df$Target), margin = 1)

ggplot(as.data.frame(prop_schol), aes(fill = Var2, y = Freq, x = Var1)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Scholarship", y = "Proportion", fill = "Target")





#correlation matrix
vars = c("age", "IR", "UR", "GDP")
df_subset = academic_df[, vars]

corr_matrix = cor(df_subset)

print(corr_matrix)


# Create heatmap plot
corr_melted <- melt(corr_matrix)
names(corr_melted) <- c("Variable1", "Variable2", "Correlation")

ggplot(corr_melted, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#132B43", high = "#56B1F7", mid = "white", midpoint = 0, 
                       limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####
data = academic_df

#relationship between categirical variables
chisq.test(data$marital_status,data$course,correct =FALSE)
chisq.test(data$marital_status,data$attendance,correct =FALSE)
chisq.test(data$marital_status,data$prev_qualification,correct =FALSE)
chisq.test(data$marital_status,data$displaced,correct =FALSE)
chisq.test(data$marital_status,data$debtor,correct =FALSE)
chisq.test(data$marital_status,data$tuition,correct =FALSE)
chisq.test(data$marital_status,data$gender,correct =FALSE)
chisq.test(data$marital_status,data$scholarship,correct =FALSE)
chisq.test(data$marital_status,data$edu_special_needs,correct =FALSE)
chisq.test(data$marital_status,data$international,correct =FALSE)
chisq.test(data$course,data$displaced,correct =FALSE)
chisq.test(data$course,data$attendance,correct =FALSE)
chisq.test(data$attendance,data$displaced,correct =FALSE)
chisq.test(data$prev_qualification,data$displaced,correct =FALSE)
chisq.test(data$debtor,data$displaced,correct =FALSE)
chisq.test(data$tuition,data$displaced,correct =FALSE)
chisq.test(data$international,data$displaced,correct =FALSE)
chisq.test(data$scholarship,data$displaced,correct =FALSE)
chisq.test(data$debtor,data$international,correct =FALSE)
chisq.test(data$edu_special_needs,data$tuition,correct =FALSE)
fisher.test(data$prev_qualification,data$attendance)


#relationship between continuous and categorical variables
shapiro.test(resid(lm(data$UR~data$international)))
kruskal.test(data$GDP,data$international)





#########
#########Cluster Analysis


# Select the columns of interest
data_selected <- data %>% select(age, UR, IR, GDP, marital_status, course, attendance, prev_qualification, displaced, edu_special_needs, debtor, tuition, gender, scholarship, international, Target)

# Convert categorical variables to factors and reorder the levels
data_selected <- data_selected %>%
  mutate_if(is.character, factor) %>%
  mutate_at(vars(marital_status, course, attendance, prev_qualification, displaced, edu_special_needs, debtor, tuition, gender, scholarship, international, Target), factor)

# Calculate Gower distance matrix
dist_matrix <- daisy(data_selected, metric = "gower")

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Determine optimal number of clusters using elbow method
elbow=fviz_nbclust(data_selected, hcut, method = "wss")

# Cut the dendrogram to get cluster assignments
clusters <- cutree(hc, k = 3)

# Cut the dendrogram to get cluster assignments
clusters <- cutree(hc, k = 4)


# Compute t-SNE
tsne_obj <- Rtsne(dist_matrix, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Extract t-SNE coordinates
tsne_coord <- tsne_obj$Y

# Combine the coordinates with the cluster assignments
tsne_df <- data.frame(tsne_coord, cluster = as.factor(clusters))

# Plot the clusters using ggplot2
ggplot(tsne_df, aes(x = X1, y = X2, color = cluster)) + 
  geom_point(size = 2) +
  ggtitle("Clustering Visualization with t-SNE")



# Add cluster assignments to the original dataset
data$cluster <- as.factor(clusters)

# Get data for each cluster
cluster1_data <- filter(data, cluster == 1)
cluster2_data <- filter(data, cluster == 2)
cluster3_data <- filter(data, cluster == 3)




######
#Balancing

#over sampling
data_balanced <- ovun.sample(Target ~ ., data = academic_df, method = "over",N = 4418)$data
table(data_balanced$Target)


#Cluster1
table(cluster1_data$Target)
cluster1_balanced <- ovun.sample(Target ~ ., data = cluster1_data, method = "over",N = 1680)$data
table(cluster1_balanced$Target)

#Cluster2
table(cluster2_data$Target)
cluster2_balanced <- ovun.sample(Target ~ ., data = cluster2_data, method = "over",N = 3872)$data
table(cluster2_balanced$Target)

#Cluster3
table(cluster3_data$Target)
cluster3_balanced <- ovun.sample(Target ~ ., data = cluster3_data, method = "over",N = 398)$data
table(cluster3_balanced$Target)

write.csv(data_balanced, file = "balanced.csv", row.names = FALSE)
write.csv(cluster1_balanced, file = "c1_balanced.csv", row.names = FALSE)
write.csv(cluster2_balanced, file = "c2_balanced.csv", row.names = FALSE)
write.csv(cluster3_balanced, file = "c3_balanced.csv", row.names = FALSE)

