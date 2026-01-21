# Intro-to-GitHub-Assignment
CHEP ML898 Data Wrangling Assignment 


# Loading necessary libraries

```{r setup, include=TRUE}

library(rstatix)
library(tidyverse)
library(pastecs)
library(knitr)
library(epitools)
library(Epi)
library(janitor)
library(tableone)
library(ggplot2)
library(ggridges)
library(harrypotter)
library(skimr)

options(scipen=999) 
```

# Question 1 Data Cleaning:

## • Load the provided datasets and examine their structure (e.g., columns, data types, unique values). • Identify and correct issues such as outliers, duplicates, and inconsistent formatting.


### Loading the dataset

```{r}
data1 <-read_csv("C:/Users/Sohana/OneDrive - University of Saskatchewan/ML 898/assignment_one/assignment1_can_path_demographics.csv")

head(data1)

data2 <-read_csv("C:/Users/Sohana/OneDrive - University of Saskatchewan/ML 898/assignment_one/assignment1_can_path_health.csv")

head(data2)

```

### Examining the structure of the datasets

```{r}
glimpse(data1)
glimpse(data2)

```

Based on the dataset one (assignment1_can_path_demographics), there are only three variables (ID, SDC_GENDER and, SDC_INCOME). SDC_GENDER and SDC_INCOME are numerical (dbl) except for ID which is a character variable, which is incorrcet. 
According to CANPATH data dictionary both SDC_GENDER and, SDC_INCOME are categorical variables.

Based on the dataset two (assignment1_can_path_health), there are six variables (ID, HS_GEN_HEALTH, NUT_VEG_QTY, NUT_FRUITS_QTY, PA_TOTAL_SHORT, and DIS_ASTHMA_EVER). All variables except ID are numerical (dbl), which is incorrect.
According to CANPATH data dictionary:
The categorical variables are: HS_GEN_HEALTH ,DIS_ASTHMA_EVER.
The count or continuous variables are: NUT_VEG_QTY, NUT_FRUITS_QTY, PA_TOTAL_SHORT


### Recording of SDC_GENDER

```{r}
    data1 <- data1 %>%
    	mutate(sdc_gender_recode = case_when(
    		SDC_GENDER == 1 ~ "Male",
        SDC_GENDER == 2 ~ "Female"
    	))

table(data1$SDC_GENDER, data1$sdc_gender_recode)
```


### Recording of SDC_INCOME

```{r}
   data1 <- data1 %>%
    	mutate(sdc_income_recode = case_when(
    		SDC_INCOME == 1 ~ "1_Less than 10 000 $",
        SDC_INCOME == 2 ~ "2_10 000 $ - 24 999 $",
    		SDC_INCOME == 3 ~ "3_25 000 $ - 49 999 $",
        SDC_INCOME == 4 ~ "4_50 000 $ - 74 999 $",
    		SDC_INCOME == 5 ~ "5_75 000 $ - 99 999 $",
        SDC_INCOME == 6 ~ "6_100 000 $ - 149 999 $",
    		SDC_INCOME == 7 ~ "7_150 000 $ - 199 999 $",
        SDC_INCOME == 8 ~ "8_200 000 $ or more",
    	))

table(data1$SDC_INCOME, data1$sdc_income_recode)

```


### Recording of HS_GEN_HEALTH

```{r}
    data2 <- data2 %>%
    	mutate(hs_gen_health_recode = case_when(
    		HS_GEN_HEALTH == 1 ~ "Poor",
        HS_GEN_HEALTH == 2 ~ "Fair",
    		HS_GEN_HEALTH == 3 ~ "Good",
        HS_GEN_HEALTH == 4 ~ "Very good",
    		HS_GEN_HEALTH == 5 ~ "Excellent"
    	))

table(data2$HS_GEN_HEALTH, data2$hs_gen_health_recode)

```

### Recording of DIS_ASTHMA_EVER

```{r}
    data2 <- data2 %>%
    	mutate(dis_asthma_ever_recode = case_when(
    		DIS_ASTHMA_EVER == 0 ~ "Never had asthma",
        DIS_ASTHMA_EVER == 1 ~ "Ever had asthma",
    		DIS_ASTHMA_EVER == 2 ~ "Presumed - Never had asthma"
    	))


table(data2$DIS_ASTHMA_EVER, data2$dis_asthma_ever_recode)
```

## Identify and correct issues such as outliers, duplicates, and inconsistent formatting.

### Missing values

```{r} 

skimr::skim(data1)
skimr::skim(data2)

``` 

SDC_INCOME from dataset one (assignment1_can_path_demographics) has  5934 (14.4%) missing values. And, HS_GEN_HEALTH (n= 672, 1.6%), NUT_VEG_QTY (n= 2549, 6.6%), NUT_FRUITS_QTY (n= 2426, 5.9%), PA_TOTAL_SHORT (n= 6763, 16.4%) and DIS_ASTHMA_EVER (n=1228, 3%) from dataset two (assignment1_can_path_health) have missing values.

### Outliers

We will use boxplots to identify outliers in the continuous variables of both datasets. Since dataset one (assignment1_can_path_demographics) does not have any continuous variables, we will only check for outliers in dataset two (assignment1_can_path_health).

```{r}
# Checking for outliers in continuous variables of data2
data2 %>%
  select(NUT_VEG_QTY, NUT_FRUITS_QTY, PA_TOTAL_SHORT) %>%
  gather() %>%
  ggplot(aes(x = key, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")
```

The boxplots show several values above the upper whisker, indicating outlier is present in all three variables (NUT_VEG_QTY, NUT_FRUITS_QTY, PA_TOTAL_SHORT) from dataset two (assignment1_can_path_health).



```{r}
summary(data2$NUT_VEG_QTY)

data2 <- data2 %>%
          mutate(new_veg = case_when(
            NUT_VEG_QTY <= 2 ~ 2,
            NUT_VEG_QTY >= 4 ~ 4,
            TRUE ~ NUT_VEG_QTY
          ))
summary(data2$new_veg)

summary(data2$NUT_FRUITS_QTY)

data2 <- data2 %>%
          mutate(new_fruit = case_when(
            NUT_FRUITS_QTY >= 5 ~ 5,
            TRUE ~ NUT_FRUITS_QTY
          ))

summary(data2$new_fruit)

summary(data2$PA_TOTAL_SHORT)
data2 <- data2 %>%
          mutate(new_pa = case_when(
            PA_TOTAL_SHORT > 1300 ~ 1300,
            TRUE ~ PA_TOTAL_SHORT
          ))

summary(data2$new_pa)


data2 %>%
  select(new_veg, new_fruit, new_pa) %>%
  gather() %>%
  ggplot(aes(x = key, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")

```

The outliers have been addressed by capping the extreme values in the continuous variables of the dataset as follows:
- NUT_VEG_QTY: Values above 4 were capped at 4, and values below 2 were capped at 2.
- NUT_FRUITS_QTY: Values above 5 were capped at 5.
- PA_TOTAL_SHORT: Values above 1300 were capped at 1300.



### Duplicates

```{r}
# Check for duplicates in data1
data1 %>%
  get_dupes(ID)
# Check for duplicates in data2
data2 %>%
  get_dupes(ID)
```

No duplicates were found in either dataset.

### Inconsistent formatting and correcting data types

```{r}
# Correcting data types in data1
data1 <- data1 %>%
  mutate( 
    SDC_GENDER = as.factor(SDC_GENDER),
    SDC_INCOME = as.factor(SDC_INCOME),
    sdc_gender_recode = as.factor(sdc_gender_recode),
    sdc_income_recode = as.factor(sdc_income_recode)
  )

# Correcting data types in data2
data2 <- data2 %>%
  mutate(
    HS_GEN_HEALTH = as.factor(HS_GEN_HEALTH),
    DIS_ASTHMA_EVER = as.factor(DIS_ASTHMA_EVER),
    NUT_VEG_QTY = as.numeric(NUT_VEG_QTY),
    NUT_FRUITS_QTY = as.numeric(NUT_FRUITS_QTY),
    PA_TOTAL_SHORT = as.numeric(PA_TOTAL_SHORT),
    hs_gen_health_recode = as.factor(hs_gen_health_recode),
    dis_asthma_ever_recode = as.factor(dis_asthma_ever_recode),
    new_veg = as.numeric(new_veg),
    new_fruit = as.numeric(new_fruit),
    new_pa = as.numeric(new_pa)
  )

```


# 2. Joining Datasets:

## • Combine tne two related datasets by performing appropriate join operations (e.g., inner join, left join). • Ensure the resulting dataset maintains consistency and completeness.


### Joining the datasets

```{r}
final_data <- data1 %>%
  inner_join(data2, by = "ID")

glimpse(final_data)
```

The datasets have been successfully joined using an inner join on the "ID" column. The resulting dataset, final_data, contains only the records that have matching IDs in both datasets, total observations and variables remained same ensuring consistency and completeness.


# 3. Descriptive Statistics

## • Calculate key statistics (e.g., mean, median, mode, variance, and standard deviation) for relevant variables. • Identify trends or anomalies based on these statistics.


### Calculating descriptive statistics

```{r}

cat("\nTable 1. The Mean and Standard Deviation\n")

CreateTableOne(data = final_data,
               vars = c("NUT_VEG_QTY", 
                        "NUT_FRUITS_QTY", 
                        "PA_TOTAL_SHORT", 
                        "new_veg", 
                        "new_fruit", 
                        "new_pa"),
               includeNA = TRUE)


```
Table 1 represents the mean and standard deviation of vegetable and fruit consumption in a day, total fruit and vegetable consumption in a day, and total physical activity in a week. 

On average, participants consumed `r round(mean(final_data$NUT_VEG_QTY, na.rm = TRUE), 2)` vegetables with variability `r round(sd(final_data$NUT_VEG_QTY, na.rm = TRUE), 2)` (SD), `r round(mean(final_data$NUT_FRUITS_QTY, na.rm = TRUE), 2)` fruits with variability `r round(sd(final_data$NUT_FRUITS_QTY, na.rm = TRUE), 2)` (SD), and the average physical activity was `r round(mean(final_data$PA_TOTAL_SHORT, na.rm = TRUE), 2)` with variability `r round(sd(final_data$PA_TOTAL_SHORT, na.rm = TRUE), 2)` (SD).

After addressing outliers, the average vegetable consumption is `r round(mean(final_data$new_veg, na.rm = TRUE), 2)` with variability `r round(sd(final_data$new_veg, na.rm = TRUE), 2)` (SD), average fruit consumption is `r round(mean(final_data$new_fruit, na.rm = TRUE), 2)` with variability `r round(sd(final_data$new_fruit, na.rm = TRUE), 2)` (SD), and average physical activity is `r round(mean(final_data$new_pa, na.rm = TRUE), 2)` with variability `r round(sd(final_data$new_pa, na.rm = TRUE), 2)` (SD).

### Median, mode and variance

```{r}
# median
summary(final_data$NUT_VEG_QTY)
summary(final_data$NUT_FRUITS_QTY)
summary(final_data$PA_TOTAL_SHORT)

#new median after addressing outliers
summary(final_data$new_veg)
summary(final_data$new_fruit)
summary(final_data$new_pa)


# mode and variance

table(final_data$NUT_VEG_QTY)
var(final_data$NUT_VEG_QTY, na.rm = TRUE)
table(final_data$NUT_FRUITS_QTY)
var(final_data$NUT_FRUITS_QTY, na.rm = TRUE)
#table(final_data$PA_TOTAL_SHORT)
#var(final_data$PA_TOTAL_SHORT, na.rm = TRUE)

# new mode and variance after addressing outliers
table(final_data$new_veg)
var(final_data$new_veg, na.rm = TRUE)
table(final_data$new_fruit)
var(final_data$new_fruit, na.rm = TRUE)
#table(final_data$new_pa)
#var(final_data$new_pa, na.rm = TRUE)

```

Interpretation: The median vegetable consumption is `r median(final_data$NUT_VEG_QTY, na.rm = TRUE)`, median fruit consumption is `r median(final_data$NUT_FRUITS_QTY, na.rm = TRUE)`, and median physical activity is `r median(final_data$PA_TOTAL_SHORT, na.rm = TRUE)`. 

The mode of vegetable consumption is `r as.numeric(names(sort(table(final_data$NUT_VEG_QTY), decreasing=TRUE)[1]))` and, the mode of fruit consumption is `r as.numeric(names(sort(table(final_data$NUT_FRUITS_QTY), decreasing=TRUE)[1]))`. 
I intentionally avoided mode of Physical activity as the result was 0, which is not meaningful in this context.

The variance of vegetable consumption is `r round(var(final_data$NUT_VEG_QTY, na.rm = TRUE), 2)`and, the variance of fruit consumption is `r round(var(final_data$NUT_FRUITS_QTY, na.rm = TRUE), 2)`.

After addressing outliers, the median vegetable consumption is `r median(final_data$new_veg, na.rm = TRUE)`, median fruit consumption is `r median(final_data$new_fruit, na.rm = TRUE)`, and median physical activity is `r median(final_data$new_pa, na.rm = TRUE)`.

The mode of vegetable consumption is `r as.numeric(names(sort(table(final_data$new_veg), decreasing=TRUE)[1]))` and, the mode of fruit consumption is `r as.numeric(names(sort(table(final_data$new_fruit), decreasing=TRUE)[1]))`.

The variance of vegetable consumption is `r round(var(final_data$new_veg, na.rm = TRUE), 2)`and, the variance of fruit consumption is `r round(var(final_data$new_fruit, na.rm = TRUE), 2)`.


### Calculating the percents and frequencies * For the variables were it is appropriate to do so

```{r}

cat("\nTable 2. The Frequency and percentage\n")

CreateTableOne(
  data = final_data,
  vars = c(
    "sdc_gender_recode",
    "sdc_income_recode",
    "hs_gen_health_recode",
    "dis_asthma_ever_recode"
  ),
  factorVars = c(
    "sdc_gender_recode",
    "sdc_income_recode",
    "hs_gen_health_recode",
    "dis_asthma_ever_recode"
  ),
  includeNA = TRUE
)

```

Interpretation: Table 2 repreesnts the frequency and percentages of categorical variables. About 36.9% of participants were male. The largest groups earning between $50,000–$74,999 (16.6%), $75,000–$99,999 (16.0%), and $100,000–$149,999 (18.5%), while 14.4% did not report their income. Most participants reported general health “very good” (41.1%) or “good” (30.5%), while only 2% reported “poor” health. Around 12.7% reported ever having asthma, while the majority (80.8%) had never had asthma.


### Identifying trends or anomalies of continuous variables


#### Histogram of daily consumption of vegetable quantity

```{r}
veg_qty_histo <- ggplot(final_data, aes(NUT_VEG_QTY)) + 
              geom_histogram()+
              labs(title="Figure 1: Histogram of daily consumption of vegetable quantity", face="bold",
                   x="Daily consumption of Vegetable", y="Frequency")
plot(veg_qty_histo)

```

Figure 1 is the histogram of daily consumption of vegetable quantity. The distribution is somewhat bell shaped. Thus we can assume that data is normally distributed. 


#### Histogram of daily consumption of fruit quantity

``` {r}
fruit_qty_histo <- ggplot(final_data , aes(NUT_FRUITS_QTY)) + 
              geom_histogram()+
              labs(title="Figure 2: Histogram of daily consumption of fruit quantity",
                   face="bold",
                   x="Daily consumption of fruit", y ="Frequency")
plot(fruit_qty_histo)
```

Figure 2 is the histogram of daily consumption of fruit quantity. The distribution is somewhat bell shaped. Thus we can assume that data is normally distributed. 


#### Histogram of total physical activity MET-minutes/week

``` {r}
pa_total_short_histo <- ggplot(final_data , aes(PA_TOTAL_SHORT)) + 
              geom_histogram()+
              labs(title="Figure 3: Histogram of total physical activity MET-minutes/week",
                   face="bold",
                   x="Total physical activity MET-minutes/week", y ="Frequency")
plot(pa_total_short_histo)
```

Figure 3 is the histogram of total physical activity MET-minutes/week. The distribution is skewed to the right. Thus we can assume that data is not normally distributed. 


# 4. Data Visualization

## • Create at least three visualizations to highlight key findings. • Use appropriate chart types (e.g., bar charts, scatterplots, heatmaps) and ensure clarity in labels, legends, and formatting.


### Heatmap of mean physical activity by asthma status and general health

```{r}
final_data %>%
  group_by(DIS_ASTHMA_EVER, HS_GEN_HEALTH) %>%
  summarise(
    mean_pa = mean(PA_TOTAL_SHORT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = HS_GEN_HEALTH, y = DIS_ASTHMA_EVER, fill = mean_pa)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Mean Physial Activity") +
  labs(
    title = "Mean Physical Activity by Asthma Status and General Health",
    x = "General health status",
    y = "Asthma status"
  ) +
  theme_minimal()

```

Interpretation: Mean physical activity increases steadily with better general health across all asthma groups. Differences by asthma status are small, suggesting overall health is more strongly related to physical activity than asthma alone.


### Scatterplot of physical activity vs fruit intake

```{r}

# creating a new variable of total fruit and vegetable intake

final_data <- final_data %>%
  mutate(fruit_veg_tot = NUT_VEG_QTY + NUT_FRUITS_QTY)

summary(final_data$fruit_veg_tot)


scatter_plot_gender <- ggplot(final_data, aes(x = fruit_veg_tot, y = PA_TOTAL_SHORT, color =sdc_gender_recode)) + 
                  geom_point(alpha = .2) + 
                  labs(x = "Total fruit and vegetable intake", y = "Physical Activity", colour = "Gender") +
  theme_classic() +
                  facet_wrap(~ sdc_gender_recode)

plot(scatter_plot_gender)

```

Interpretation: The scatterplots show variability in physical activity across total fruit and vegetable intake for both females and males. Physical activity shows similar pattern across genders, suggesting no strong gender specific differences in the relationship.


### Density ridges of physical activity by income category

```{r}

ggplot(final_data, aes(x = PA_TOTAL_SHORT, y = sdc_income_recode,  fill = sdc_income_recode)) +
  geom_density_ridges2(rel_min_height = 0.01) +
  labs(x = "Total physical activity", y = "Income group", colour = "Income") +
  theme_classic() +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = 2) +
  scale_fill_hp(discrete = TRUE, option = "LunaLovegood", name = "Income")

```

Interpretation: Physical activity is right skewed across all income groups. Most participants reported low to moderate physical activity.




