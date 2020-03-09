pacman::p_load(tidyverse)

# Practice
df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6"),
  stringsAsFactors = FALSE
)
separate_rows(df, x:z, convert = TRUE)

d <- data.frame(a=c(1:3), 
                b=c("name1, name2, name3", "name4", "name5, name6"),
                c=c("name7","name8, name9", "name10" ))
separate_rows(d, a:c, sep=",") 

# 2/25 T
# 1 Read the data
dat <- read_csv("C:/Users/qlekr/Desktop/SCHOOL/Data Science Club/Group/Rent.csv") %>% clean_names()


# 2 separate_rows, (",")
separate_rows(dat, description, sep = ".") %>% view()
# sep = "[:punct:]"
# what is pinct? (, . ! / )


# 3 piviot_wider, (gender, cost)
dat_wider <- dat %>% 
  select(apartment, gender, cost) %>% 
  pivot_wider(names_from = gender, values_from = cost) %>% view()

###
dat_wider <- dat %>% 
  pivot_wider(names_from = gender, values_from = cost) %>% view()

# 4 piviot_longer
dat_wider %>% 
  pivot_longer(c('F', 'M'), names_to = "gender", values_to = "Cost", values_drop_na = TRUE) %>% view()


# 2/26
#library(data.table) (read data library)
# train <- fread("train.csv") %>% clean_names()

# setwd("") (working directory)

# c() this means vector, or list

# str(test), summary(i), glimpse(test), dim(i) this checks the structure of the data dim(i) 

# add_count (adds a column right next to the column and count)
## dat <- dat %>% add_count(family, name = "family_number") (family 는 기준점)

# colSums(is.na(dat)) (count and sum each column of number of NA)
## colSums(is.na(dat)) %>%  data.frame()
### data.frame() (piviots the table)

# library(naniar)
## naniar::gg_miss_var(dat)

# where_na(dat) 
# this fuctions tells us where the missing value is

library(tidyverse)
dat <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", 
                col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))

dat %>% 
  ggplot(aes(x = Semester_Date, y = Count, color = Department)) +
  geom_line() + 
  geom_point(color = "black") +
  theme_bw() + 
  scale_color_viridis_d() + #changes the color of line
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")


# Facet example

dat %>% 
  #  group_by(Year, Department) %>% 
  #  summarise(total = sum(Count), mean = mean(Count), sd = sd(Count)) %>% 
  #  ungroup() %>% 
  ggplot(aes(x = Department, y = Count, fill = Semester)) + 
  geom_col() + 
  facet_grid(Semester~Year)

dat %>% 
  group_by(Year, Department) %>% 
  summarise(total = sum(Count), mean = mean(Count), sd = sd(Count)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Department, y = total)) + 
  geom_col() + 
  facet_wrap(~Year)

# Graph, change y, angle, change x
dat %>% 
  ggplot(aes(x = nameGiven, y = salary)) + 
  geom_boxplot(fill = NA) + 
  geom_jitter(height = 0) +
  facet_wrap(~byu, scales = "free_x") + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  labs(x = "Player", y = "Salary (non-adjusted", title = "BYU Wins!")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency", 
       subtitle = "Two seaters ", 
       caption = "Data from abc", 
       x = "Engine displacement", 
       y = "Highway fuel economy", 
       color = "Car type"
  )

geom_label
use nudge_y = 2 to move the label

#ggrepel labels don't overlap
ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

#change is to number
prase_number(money)

geom_boxplot
geom_jitter

## change y, just set the plotting window. coord_cartesian
fl_bp +
  geom_boxplot() +
  coord_cartesian(ylim = c(-10, 50)) +
  facet_wrap(~origin)

## geom_col and geom_bar
Use geom_col() instead of geom_bar(stat = "identity")  

# clean data
https://rpubs.com/bradleyboehmke/data_wrangling

# Data import
https://r4ds.had.co.nz/data-import.html

#case_when
mutate(
  season = case_when(
    month %in% c("12", "01", "02") ~ "Winter", 
    month %in% c("03", "04", "05") ~ "Spring", 
    month %in% c("06", "07", "08") ~ "Summer", 
    month %in% c("09", "10", "11") ~ "Fall", 
    TRUE ~ "Other")
)

# Read_lines see task_11
dat_random <- readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt")
dat_number<- readr::read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")
#Remove all the a and e and count
dat_random %>% str_remove_all("a|e") %>% str_count()

# change angle, theme, facet, wrap, grid, x or y change
ggplot(aes(x = nameGiven, y = salary)) + 
  geom_boxplot(fill = NA) + 
  geom_jitter(height = 0) +
  facet_wrap(~byu, scales = "free_x") + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(axis.f.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  labs(x = "Player", y = "Salary (non-adjusted", title = "BYU Wins!")

# Generate Date
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
discount_data_df %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))

# fill the colunm
discount_data_df %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  group_by(Product) %>%
  fill(`Discount Rate`)

# ifelse
train <- train %>% 
  mutate(cabin = ifelse(cabin %>%  nchar( == 0, NA, cabin))
         
         # cut fuction
         p + cut(breaks = c(0, 20, 40, 60, 80), labels = c("A", "B", "c", "D"))
         
         # table
         ## - means desc order
         table(titanic$title) %>% data.frame() %>% arrange(-Freq)
         
         # Find where the NA is
         titanic[is.na(titanic),]
         ## or
         
         # library(janitor)
         ## Changes the title to lower_case
         read_csv("asdsa") %>% clean_names()
         
         # transmute , just keep the mutated col
         
         # cut fuction
         data1 %>% 
           mutate(
             fare_group = cut(as.numeric(data1$Fare), breaks = quantile(Fare, na.rm = T), labels = c("1", "2", "3", "4")))
         
         #2. Per pclass, get the top 5 highest fare and make table, library(dplyr)
         Pclass1 <- data1 %>% 
           group_by(Pclass) %>% 
           arrange(-Fare) %>% 
           ungroup() %>% 
           group_by(Pclass) %>% 
           slice(1:5)
         
         # for loop
         df <- tibble(
           a = rnorm(10),
           b = rnorm(10),
           c = rnorm(10),
           d = rnorm(10)
         )
         
         output <- vector("double", ncol(df))
         for(i in seq_along(df)) {
           output[[i]] <- median(df[[i]])
         }
         output
         
         # separate
         table3 %>% 
           separate(rate, into = c("cases", "population"), sep = "/")
         
         # Piviot_wider
         table2 %>%
           pivot_wider(names_from = type, values_from = count)
         
         # piviot_longer
         tidy4a <- table4a %>% 
           pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
         tidy4b <- table4b %>% 
           pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
         left_join(tidy4a, tidy4b)
         
         ## NA drop
         stocks %>% 
           pivot_wider(names_from = year, values_from = return) %>% 
           pivot_longer(
             cols = c(`2015`, `2016`), 
             names_to = "year", 
             values_to = "return", 
             values_drop_na = TRUE
           )
         
         # Make table
         knitr::kable()
         
         # shows the columns of the data
         glimpse()
         
         # change the graph order (use inside the ggplot())
         ## fct_reorder (order by the median), (data, which, what)
         dat_bmnt %>% 
           filter(volume_short_title == "BoM") %>% 
           ggplot(aes(x = fct_reorder(book_title, count, median), y = count)) + 
           geom_boxplot()
         
         ## fct_inorder (order by the data input?) 
         dat_bmnt %>% 
           filter(volume_short_title == "BoM") %>% 
           ggplot(aes(x = fct_inorder(book_title), y = count)) + 
           geom_boxplot()
         
         ## fct_inorder and fct_rev, reverse the order
         dat_bmnt %>% 
           filter(volume_short_title == "BoM") %>% 
           ggplot(aes(x = fct_inorder(book_title) %>% fct_rev(), y = count)) + 
           geom_boxplot()
         
         # cut x axis
         p + coord_cartesian(xlim = c(0, 90))
         
         # x axis scale
         p +   scale_x_continuous(breaks = seq(0,150, by = 20))
         
         # Erase the outliers 
         geom_boxplot(outlier.colour = NA, fill = NA)
         
         
         #library(data.table) (read data library)
         train <- fread("train.csv") %>% clean_names()
         
         # setwd("") (working directory)
         
         # c() this means vector, or list
         
         # str(test), summary(i), glimpse(test), dim(i) this checks the structure of the data dim(i) 
         
         # add_count (adds a column right next to the column and count)
         ## dat <- dat %>% add_count(family, name = "family_number") (family 는 기준점)
         
         # colSums(is.na(dat)) (count and sum each column of number of NA)
         ## colSums(is.na(dat)) %>%  data.frame()
         ### data.frame() (piviots the table)
         
         # library(naniar)
         ## naniar::gg_miss_var(dat)
         
         # where_na(dat) 
         # this fuctions tells us where the missing value is
         
         # library(highcharter)
         
         # library(lubridate)
         * ymd(), mdy(), dmy(), ydm()
         * “January 21st, 2017”
         * “2012-01-01”
         * “March 2016 - 23”
         
         # The creators
         # ymd(), mdy(), dmy(), ydm()
         
         "January 21st, 2017"
         “2012-01-01”
         “March 2016 - 23”
         
         mdy('January 21st, 2017')
         ymd('2012-01-01')
         myd('March 2016 - 23')
         
         str(mdy('January 21st, 2017'))
         as.numeric(mdy('January 21st, 2017'))
         
         # The Formaters
         # year(), month(), mday() (day of the month), yday() (day of the year), wday() (day of the week), hour(), minute(), and second()
         
         “March 23, 2016”
         # 
         'March 23, 2016' %>% mdy() %>% wday(label = T)
         'March 23, 2016' %>% mdy() %>% yday()
         'March 23, 2017' %>% mdy() %>% yday()
         
         # THE CHANGERS (1)
         # floor_date(), round_date(), and ceiling_date()
         
         jan31 <- ymd('2013-01-31')
         # add one month to "January 31st
         jan31 + months(1)
         
         # celing_date
         # Add 27 hours and 15 minutes to “January 31st, 2013 06:35:27”
         mtime <- 'January 31st, 2013 06:35:27' #%>% mdy_hms() + hours(27) + minutes(15)
         mtimef <- mtime %>% mdy_hms()
         
         # change time zone to amrica/denver
         force_tz(mtimef, tzone = "America/Denver")
         
         # change the time zone 원래 있던 시간을 america/denver에서의 시간이 언제인지 바꿈
         with_tz(mtimef, tzone = "America/Denver")
         
         # floor_date()
         mtimef %>% floor_date(unit = "hour")
         mtimef %>% floor_date(unit = "day")
         mtimef %>% floor_date(unit = "15 minutes")
         mtimef %>% floor_date(unit = "quater")
         # ceiling_date()
         
         # narniar
         # missing Value
         naniar::gg_miss_var(dat)