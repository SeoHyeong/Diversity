## Load the required library
library(DataComputing)
library(tidyr)
library(rpart)
library(statisticalModeling)
## install.packages("rpart.plot")
library(rpart.plot)
## install.packages("xlsx")
library(xlsx)

## Input the data into R
data_00_13 <- read.csv("C:\\Users\\QiMing\\Downloads\\Delta_database_87_13_CSV\\data_clean_1.csv")
data_87_99 <- read.csv("C:\\Users\\QiMing\\Downloads\\Delta_database_87_13_CSV\\data_clean_2.csv")

## Clean up and summarise the data table
data_0013_clean <- data_00_13 %>%
  group_by(academicyear) %>%
  summarise(Operating_Income = sum(stable_operating_rev, na.rm = TRUE),
            Total_Enrollment = sum(total_enrollment, na.rm = TRUE),
            Total_Intl_Enrollment = sum(total_enrollment_nonres_tot, na.rm = TRUE))

data_8799_clean <- data_87_99 %>%
  group_by(academicyear) %>%
  summarise(Operating_Income = sum(stable_operating_rev, na.rm = TRUE),
            Total_Enrollment = sum(total_enrollment, na.rm = TRUE),
            Total_Intl_Enrollment = sum(total_enrollment_nonres_tot, na.rm = TRUE))

## education_budget <- read.xlsx("C:\\Users\\QiMing\\Downloads\\edhistory.xls")

Government_funding_clean <- education_budget[43,] %>%
  gather(key, value, -NA.) %>%
  filter(grepl("X[0-9]{4}", key)) %>%
  select(-NA.) %>%
  mutate(Year = as.numeric(gsub("X", "", key)),
         Government_funding = (as.numeric(value) * 1000)) %>%
  select(Year, Government_funding)
         

data_clean <- rbind(data_0013_clean, data_8799_clean)
data_clean <- data_clean %>%
  arrange(academicyear)

## Input other data into R
gdp_clean <- read.csv("C:\\Users\\QiMing\\Downloads\\Diversity-master\\Diversity-master\\cleangdp.csv")
president_clean <- read.csv("C:\\Users\\QiMing\\Downloads\\Diversity-master\\Diversity-master\\Presidents\\Presidents.csv")
senate_houses_clean <- read.csv("C:\\Users\\QiMing\\Downloads\\Diversity-master\\Diversity-master\\CongressParty\\DominantCongressParty.csv")
visa <- read.csv("C:\\Users\\QiMing\\Downloads\\Diversity-master\\Diversity-master\\visa_all.csv")
total_visa <- read.csv("C:\\Users\\QiMing\\Downloads\\Diversity-master\\Diversity-master\\Visas\\Visas.Year.csv")

visa_clean <- visa %>%
  group_by(Year) %>%
  summarise(total_J1 = sum(J.1, na.rm = TRUE),
            total_M1 = sum(M.1, na.rm = TRUE),
            total_F1 = sum(F.1, na.rm = TRUE)) %>%
  mutate(Visa_for_Intl_Students = (total_J1 + total_M1 + total_F1)) %>%
  select(Visa_for_Intl_Students,Year)


## Join up other variables into data_clean
data_clean <- data_clean %>%
  left_join(Government_funding_clean, by = c(academicyear = "Year")) %>%
  left_join(gdp_clean, by = c(academicyear = "year")) %>%
  left_join(president_clean, by = c(academicyear = "Year")) %>%
  left_join(visa_clean, by = c(academicyear = "Year")) %>%
  left_join(total_visa, by = c(academicyear = "Year"))

names(data_clean)[names(data_clean) == "gdp.billion."] <- "GDP.billion"
names(data_clean)[names(data_clean) == "academicyear"] <- "Academic_Year"
names(data_clean)[names(data_clean) == "Grand.Total"] <- "Total_Visa"

data_clean <- data_clean %>%
  select(-X.x, -X.y) 

data_clean <- data_clean %>%
  mutate(Operating_Income.billion = (Operating_Income / 1000000000),
         Income_per_GDP = (Operating_Income.billion / GDP.billion * 100),
         Prop_Intl_Student = (Total_Intl_Enrollment / Total_Enrollment * 100),
         Government_funding_per_GDP = (Government_funding / (GDP.billion * 1000000000) * 100))

data_clean <- data_clean %>%
  left_join(senate_houses_clean, by = c(Academic_Year = "Year"))


## ggplot
p1 <- data_clean %>%
  ggplot(aes(x = Academic_Year, y = Income_per_GDP, color = Party)) +
  geom_line() 

p2 <- data_clean %>%
  ggplot(aes(x = Academic_Year, y = Income_per_GDP)) + 
  geom_line() + 
  facet_grid(.~Party)

p3 <- data_clean %>%
  ggplot(aes(x = Prop_Intl_Student, y = Income_per_GDP)) +
  geom_line() +
  scale_x_continuous(limits = c(2.5,4))

p4 <- data_clean %>%
  ggplot(aes(x = Government_funding_per_GDP, y = Income_per_GDP)) +
  geom_point()

## Regression
## Linear Regression

## Prop_Intl_Student ~ Party + House + Senate

linear_model <- lm(Prop_Intl_Student ~ Party + House + Senate, data_clean)
print(linear_model)
summary(linear_model)

## Result:

p5 <- fmodel(linear_model)

linear_model %>%
  evaluate_model(at=list(Party="Republican", House = "Republican", Senate = "Republican")) 

## Predicted Output = 3.125402

## Income_per_GDP ~ Prop_Intl_Student
linear_model_1 <- lm(Income_per_GDP ~ Prop_Intl_Student, data_clean)
print(linear_model_1)
summary(linear_model_1)

## Result: Income_per_GDP = 0.2202(Prop_Intl_Student) + 0.4127

p6 <- fmodel(linear_model_1)

linear_model_1 %>% 
  evaluate_model(at=list(Prop_Intl_Student = 3.125402))

## Predicted Output = 1.100947

## Income_per_GDP ~ Prop_Intl_student + Government_funding_per_GDP
linear_model_2 <- lm(Income_per_GDP ~ Prop_Intl_Student + Government_funding_per_GDP, data_clean)
print(linear_model_2)
summary(linear_model_2)

## Result: Income_per_GDP = 0.2098(Prop_Intl_Student) + -1.3663(Government_funding_per_GDP)

p7 <- fmodel(linear_model_2)
linear_model_2 %>%
  evaluate_model(at=list(Prop_Intl_Student = 3.125402, Government_funding_per_GDP = (324.1/19284.99)*100))
                 
## Budget for FY2017 - 324.1billion on tertiary education, GDP for FY2017 = 19287.99billion
## Predicted_output = -0.977233

## Recursive Partition
rpart(Income_per_GDP ~ Prop_Intl_Student + Government_funding_per_GDP, data_clean) %>%
  rpart.plot::prp(type=3)

rpart(Operating_Income ~  Party + Visa_for_Intl_Students, data_clean) %>%
  rpart.plot::prp(type=3)


