---
title: "ML_template"
author: "Korean Data Team"
date: "05 March, 2020"
output:
  html_document:
    theme: cerulean
    keep_md: true
    toc: true
    toc_width: 10
    toc_float: true
    code_folding: hide
---

# 예상

1. 데이터베이스에 뽑아올 가능성이 높다 <- Join 을 잘 써야한다.
2. 바이너리 문제가 나올 가능성이 높다 (A VS B)
3. 텍스트마이닝을 할 가능성이 높다.a
4. 금융정보에 대한 지식이 필요할 가능성이 높다 (feature selection)

5. Outlier Detection (KNN = K-NEIGHTBORHOOD-NEAREST)

# 1. Objectives

* Predict customer behaviors
(name, address, income, credit score, phone number, job, purchase item, lease term, lease amount, interest rate, number of late payments, account_id
given name, family name, birth data (age), gender, bank, demographic info, review(sentiment)) 

* 
*

## a. Load Packages

```r
# Load the Packages

# loading package
library(tidyverse)
library(data.table)

# clean columns
library(janitor)

# EDA
library(DataExplorer)
library(naniar)
library(skimr)

# Visualization 
library(plotly)
library(corrplot)
library(DT)
library(ggrepel)
library(corrplot)
library(gridExtra)

# Feature Engineering
library(stringr)
library(lubridate)

# Feature Selection
library(Boruta)


# Machine Learning
library(caret)
library(xgboost)
library(rattle)

# Instant load the packages
pacman::p_load(data.table, janitor, tidyverse, DataExplorer, naniar, skimr, plotly, corrplot, DT, gridExtra, ggrepel, stringi, lubridate, caret, xgboost, rattle, Boruta)
```

## b. Load the data

```r
# Clean the name of variables (lower.case & '_' connection) by using Janitor package

# Method 1
train <- fread(file = "url", stringsAsFactors = F, na.strings= c("NA","","NULL")) %>% clean_names()

# Method 2
test <- read_csv(file = "url", header = TRUE, na = c("NA", "","NULL"), n_max = Inf, guess_max = min(1000, n_max)) %>% clean_names()

test$target <- NA

dat <- rbind(train, test)
```

## c. Load Multiple files in one directory

```r
file_list <- sapply("directory",function(x){list.files(x,pattern = "csv")})
for (i in file_list){
  assign(str_extract(i, "[a-z|A-Z|_|0-9]+(?=\\.)"), fread(i, stringsAsFactors = F, na.strings= c("NA","","NULL")))
}
```

# 2. EDA (Expolatory Data Analysis)

## a. Scales of data 


```r
# ncol() | nrow()
cat("Train dataset' has", dim(train)[1], "rows and", dim(train)[2], "columns."
    "\n Test dataset has",dim(test)[1], "rows and", dim(test)[2], "columns.")
```

## b. Structure of data


```r
for (i in c(train, test)) {
  str(i)
  summary(i)
  glimpse(i)
  # skimr package
  skim(i)
}
```

## c. Type of variables 


```r
lapply(dat, typeof) %>% data.frame() %>%  t() %>%  as.data.frame() 
```


## d. missing value 

code below is needless when 

  1. $ na.strings= c("NA","")) $ is used for $fread$ function and 
  2. $ na = c("NA", "") $ is used for $read\_csv$ function.


```r
# missing_transform (optional)
# dat <- sapply(dat, function(x) {ifelse(is.character(x) & nchar(x) == 0, NA, x)}) %>% data.frame(stringsAsFactors = F) 
# Careful to not use as.data.frame()
```


```r
colSums(is.na(train)) %>% data.frame()
colSums(is.na(test)) %>% data.frame()
```


```r
# Show missing value - visualization
missing_train <- plot_missing(train, missing_only = T, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "Train missing") %>% ggplotGrob()
missing_test <- plot_missing(test, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "Test missing") %>% ggplotGrob()
```


```r
# Show missing result by using grid
grid.arrange(missing_train, missing_test, ncol = 1)
```

- The missing plot per variable can be created with $naniar$ Package as well below

- There is $gg\_miss\_var$ plot which indicates how much proportion of missing values in $nanair$ pacakge below. 


```r
# Show missing values by using naniar package
gmv1 <- naniar::gg_miss_var(train, show_pct = T)
gmv2 <- naniar::gg_miss_var(test, show_pct = T)
grid.arrange(gmv1, gmv2, nrow = 1)
```

- Also, it can be visualized with other way shown below. (Not recommended)


```r
# Show missing values by using naniar package (Not recommended)
m1 <- naniar::vis_miss(train, warn_large_data = F) + labs(title = "train missing")
m2 <- naniar::vis_miss(test, warn_large_data = F) + labs(title = "test missing")
grid.arrange(m1, m2, ncol = 1)
```

- Here is own created missing viz method


```r
missing_clock_plot <-  function(x){
  colSums(is.na(x)) %>% 
    data.frame() %>% 
    rownames_to_column() %>%
    mutate(pct = ./nrow(x)) %>% 
    ggplot(aes(x = rowname, y = .)) +
    geom_text(aes(label = ifelse(pct == 0, "", paste(round(pct,2) * 100,"%")), size = 5), show.legend = F) +
    geom_bar(stat = "identity", fill = "red", alpha = 0.5) +
    coord_polar(theta = "x") +
    labs(x = "Missing number", y = "Variables", title = "Missing Value per Variable") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 10, family = "sans"))
}

missing_clock_plot(train)
missing_clock_plot(test)
```

## d. Switching original features to any datatype(i.e. numeric, characater, date, ...) value with condition (mutate_at)


```r
# Using mutate_at
# train <- train %>% mutate_at(colnames(data1) %>% grep("gpa|act|credits", ., value =T), as.numeric)
# test <- test %>% mutate(total_meeting_duration = total_meeting_duration %>% as.numeric())

t1 <- train %>% select_if(is.numeric) %>% colnames() %>% data.frame() 
t2 <- test %>% select_if(is.numeric) %>% colnames() %>% data.frame()
rbind(t1, t2) %>% mutate(type = "Numeric") # Create 'type' column
```

## e. DataExplorer Package 


```r
# DataExplor package - numeric variable & categorical variable & row & column
t1 <- DataExplorer::introduce(data1) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")
t2 <- DataExplorer::introduce(data2) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")

# DataExplor package - Visualization
p1 <- DataExplorer::plot_intro(data1, title = "train", ggtheme = theme_bw())
p2 <- DataExplorer::plot_intro(data2, title = "test", ggtheme = theme_bw())
```


```r
# Combine tables and plots in a row.
grid.arrange(p1, tableGrob(t1, rows=NULL), tableGrob(t2, rows=NULL), p2,  ncol = 2, top = "Attributes of datasets")
```


# 3. Feature Engineering 

Sequences

1. Feature Exploation (Data Visulaization)
2. Wraglization
3. Feature selection 
4. Imputation

## a. Feature Exploration

- ggplot 
- highcharter

1. ggplot containing table


```r
# t1 <- advising_student_fs %>% 
#   group_by(enrollment_status) %>% 
#   summarise(mean = mean(sum_of_cumulative_missing_credit, na.rm = T)) 
# 
# advising_student_fs %>% 
#   ggplot(aes(x = enrollment_status, y = sum_of_cumulative_missing_credit)) +
#   geom_jitter(alpha = 0.3, color = "gray") +
#   geom_violin(aes(fill = enrollment_status)) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   coord_flip() +
#   annotation_custom(tableGrob(t1, rows = NULL), xmin= 0.5, xmax=  1 , ymin= 50 , ymax= 60)
```

2. alluvium plot


```r
# titanic %>% group_by(sex, age_group, survived) %>% tally() %>% 
#   ggplot(data= . , aes(axis1 = sex, axis2 = age_group, axis3 = survived, y = n)) +
#   geom_alluvium(aes(fill = survived), width = 0, knot.pos = 0) +
#   guides(fill = F) +
#   geom_stratum(width = 0.1, infer.label = T) +
#   geom_label(stat = "stratum", infer.label = T) +
#   scale_x_continuous(breaks = c(1,2,3) , labels = c("sex", "age group", "survived")) +
#   theme_minimal()
```

3. proportion table


```r
# prop.table(table(train$survived)) %>% data.frame()
# table(advising_student_fs$enrollment_status) %>% data.frame() %>% mutate(Pct = round(Freq / sum(Freq) * 100, 2))
```

## b. Data Wraglization

### 1. data1 & data2 Joining key


```r
# without function
joining_key <- data1[,which(colnames(data1) %in% colnames(data2))] %>% colnames()

join_data <- 
  left_join(data1, data2, by = joining_key)

# with function (natural join)
natural_join <- function(x,y){
  x %>% left_join(y, by = x[,which(colnames(x) %in% colnames(y))] %>% colnames())
}

join_data <-
  data1 %>% natural_join(data2)


# datatable(join_data %>% head(100), options = list(scrollX = T))
```

### 2. add_count (adding variable count to orignal data frame)


```r
dat <- dat %>% add_count(varible, name = "")
# titanic <- titanic %>% add_count(family, name = "fam_count")
```

### 3. Dimensional Reduction


```r
# Transform continous data to categorical factor
dat <- dat %>% mutate(name = variable, cut(breaks = c(), labels = c()))
# titanic <- titanic %>% mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))

# ifelse
dat <- dat %>% mutate(name = ifelse(condition, true_value, false_value))
# titanic <- titanic %>% 
#   mutate(fa = ifelse(fa == "alone", "alone", "family"))

# case_when
dat <- dat %>% mutate(name = case_when(
                      condition1 ~ "level1",
                      condition2 ~ "level2",
                      condition3 ~ "level3",
                      TRUE ~ "level4"
                      ))
# titanic <-
#   titanic %>% 
#   mutate(fa = case_when(
#     sib_sp + parch == 0 ~  "alone",
#     fam_count == sib_sp + parch + 1 ~ family,
#     related_number == sib_sp + parch + 1 ~ family,
#     TRUE ~ "Unknown"
#   ))
```

### 4. text mining


```r
# description, review, sentiment, address(city, zipcode)
# str_detect (return TRUE, FALSE)
# str_extract | str_extract_all
# str_replace | str_replace_all
# str_split
# grep | gsub
# regular expression
```

### 5. tidyverse


```r
# gather, spread
# separate, unite
# arrange
# distinct
# top_n
# rank
```

## c. Feature Selction

1. method 1 - Correlation plot


```r
cor <- cor(train %>% select_if(is.numeric))
corrplot(cor, method = c("number"), type = "upper", tl.col = "black", diag = T)
```

2. method 2 - Boruta method


```r
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(survived ~ ., data=na.omit(train), doTrace=2)  # perform Boruta search
# collect Confirmed and Tentative variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = T)
print(boruta_signif)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance
```

3. method 3 - Recursive Feature Elimination (RFE)


```r
set.seed(1000)
trainControl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE,
                   allowParallel = TRUE)

train_model <- train %>% na.omit()

lmProfile <- rfe(x= train_model, y= train_model$survived,
                 sizes = c(1,5,10,15,20),
                 rfeControl = trainControl)

lmProfile
```

## d. Imputation

Improving data quality 

1. Filtering missing value


```r
# Method 1
dat %>% na.omit() # delete entire missing value regardless of variables

# Method 2
missing_variables <- c() # Put selected variables for ML
dat %>% drop_na(mssing_variables)
```

2. Impute missing value

Median is the most frequent way to impute the missing data

  - a. Identification of data


```r
# Visualize missing portion of each variables
naniar::gg_miss_var(dat, show_pct = T)

# Example

# advising_student_final %>% ggplot() +
#   geom_histogram(aes(x = act_composite, fill = enrollment_status)) +
#   facet_wrap(~ enrollment_status) +
#   theme_minimal()
# 
# advising_student_final %>% ggplot() +
#   geom_histogram(aes(x = high_school_gpa, fill = enrollment_status)) +
#   facet_wrap(~ enrollment_status) +
#   theme_minimal()

# advising_student_final %>% group_by(enrollment_status) %>% summarise(act_median = median(act_composite, na.rm = T), hs_gpa_median = median(high_school_gpa, na.rm = T))
# 
# advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 22
# advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 22
# advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.46
# advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.24

# advising_student_final <- advising_student_final %>% mutate_if(is.character, as.factor) %>% mutate_if(is.logical, as.factor) %>% select(-c(identifier))
 
# sapply(advising_student_final, class)

naniar::gg_miss_var(advising_student_final, show_pct = T)
```

  - b. Imputation median per group


```r
# for (i in unique(titanic$title)){
#   titanic$age[titanic$title == i & is.na(titanic$age)] <- median(titanic$age[titanic$title == i ], na.rm = T)
# }
```

Checking before there is no NA values for each feaure we are going to use

3. NA validation


```r
colSums(is.na(dat))
```


# 4. Machine Learning


## a. Check Target Distribution

check if target variable is balanced or not


```r
proportion <- dat %>% group_by(target) %>% tally() %>%  mutate(pct = round(n/sum(n),2))
proportion
```

## b. Split between train vs test


```r
# Modeling
features <- c()
ml_df <- dat %>% filter(!is.na(target)) %>% select(features) 

set.seed(1000)
InTraining <- caret::createDataPartition(data$target, p = .7, list = F)
# createFolds(train$survived, 10, list = F)
train <- dat %>% dplyr::slice(InTraining)
test <- dat %>% dplyr::slice(-InTraining)
```

## c. xgboost setting


```r
# taget setting
tr_label <- train$survived 
ts_label <- test$survived 
total_label <- ml_df$survived

# model.matrix
new_tr <- model.matrix(~.+0, data = train %>% select(-target), with = F)
new_ts <- model.matrix(~.+0, data = test %>% select(-target), with = F)
new_total <- model.matrix(~.+0, data = ml_df %>% select(-target), with = F)
```



```r
#xgb.DMatrix
dtrain <- xgb.DMatrix(new_tr, label = tr_label)
dtest <- xgb.DMatrix(new_ts, label = ts_label)
dtotal <- xgb.DMatrix(new_total, label = total_label)
```

## d. default modeling


```r
# traincontrol : cross-validation (5 folds)
xgb_trcontrol <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# model without params
xgb_model<- xgboost::xgboost(data = new_tr, 
                    label = tr_label,
                    verbosity = 2, 
                    objective = "binary:logistic", 
                    nrounds = 500, 
                    early_stopping_rounds = 100, 
                    trControl = xgb_trcontrol)
```

## e. params modeling


```r
?xgb.train

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.2, # the less, preventing overfitting
  gamma = 0.1,
  max_depth = 6, # the less, prevent overfitiing
  min_child_weight = 1, # the more, preventing overfitting 
  subsample = 1, 
  colsample_bytree = 1,
  eval_metric = "error", # rmse, rmsle, error, mae, auc
  scale_pos_weight = 549/342 # case 0 number / case 1 number (imblanced)
)

watchlist <- list(train = dtrain, test = dtest, total = dtotal)

xgb_model2 <- xgb.train(params = params, data= dtrain, nrounds = 500, trControl = xgb_trcontrol, watchlist = watchlist, early_stopping_rounds = 100)
```


## f. validation of test 


```r
test <- test %>% mutate(pred_pct = predict(xgb_model, dtest), pred = ifelse(pred_pct > 0.5, 1, 0))
test <- test %>% mutate(pred_pct2 = predict(xgb_model2, dtest), pred2 = ifelse(pred_pct2 > 0.5, 1, 0))

confusionMatrix(factor(test$pred), factor(test$target))
confusionMatrix(factor(test$pred2), factor(test$target))
```

## g. validation of whole dataset


```r
ml_df <- ml_df %>% mutate(pred_pct = predict(xgb_model, dtotal), pred = ifelse(pred_pct > 0.5, 1, 0))
ml_df <- ml_df %>% mutate(pred_pct2 = predict(xgb_model2, dtotal), pred2 = ifelse(pred_pct2 > 0.5, 1, 0)))
```

## h. confusionmaxtrix


```r
confusionMatrix(factor(ml_df$pred), factor(ml_df$target)) 
confusionMatrix(factor(ml_df$pred2), factor(ml_df$target))
```

# 5. conclusion

## a. variable importance


```r
# variable importance
imp_matrix <- xgb.importance (feature_names = colnames(new_tr),model = xgb_model2)
xgb.plot.importance(imp_matrix[1:10])

# xgboost tree viz
xgb.plot.tree(model = xgb_model2, trees = 0, show_node_id = TRUE)
```

- 
- 
-
