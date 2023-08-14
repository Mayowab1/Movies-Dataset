# Step 1 Import data and library:
# Load library:

install.packages("tidyverse")

# Import library

library(tidyverse)

# Step 2 Check data types:

str(Hollywood)

dim(Hollywood) 

# Step 3 Check for missing values:

colSums(is.na(Hollywood))


# Step 4 Remove missing values in rows and check for duplicates
Hollywood <- na.omit(Hollywood)

#Check for duplicates and
# Remove duplicated rows based on a given column
Hollywood <- Hollywood[!duplicated(Hollywood$Film), ]

# Step 5 round off values to 2 places

Hollywood$Profitability <- round(Hollywood$Profitability ,digit=2)

head(Hollywood)

#Step 2.1: Outlier removal:

#Check for outliers using a boxplot
install.packages("ggplot")
library(ggplot2)

#remove outlier

#Create a boxplot that labels the outliers  
ggplot(Hollywood, aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'growth'
Q1 <- quantile(Hollywood$Profitability, .25)
Q3 <- quantile(Hollywood$Profitability, .75)
IQR <- IQR(Hollywood$Profitability)


no_outliers <- subset(Hollywood, Hollywood$Profitability> (Q1 - 1.5*IQR) & Hollywood$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers) 

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)

Q3 <- quantile(no_outliers$Worldwide.Gross, .75)

IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1)

#Step 3: Exploratory Data Analysis 

#Summary Statistics:
summary(df1)

#bivariate analysis

#scatter plot:
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart:
ggplot(df1, aes(x=Year)) + geom_bar()

#additional chart:

#piechart:
counts <- table(df1$Genre)
pie(counts, main="Pie Chart - Genres",)

#Histogram:
ggplot(df1,aes(x=Year)) + geom_histogram(binwidth=1, color= "darkblue", fill="lightblue", linetype="dashed") +ggtitle("Movies produced per year")

#barchart:
ggplot(df1, aes(x=Genre, y=Profitability , fill=Genre)) + geom_bar(stat = "identity", fill="lightpink") + theme_dark() +ggtitle("Profitability of Genres")

#Write a line of code to export the clean data.
write.csv(df1, "clean_df1.csv")

