########################
# Set up
########################

# install.packages("properties")
library(properties)
library(DBI)
library(odbc)
library(tidyverse)
library(wesanderson)
library(plotly)

# rename template.config.properties to config.properties and fill out login data
# NEVER commit this file (it is present in .gitignore)
config <- read.properties('config.properties')

con <- dbConnect(odbc(),
                 driver = "SQL Server",
                 uid = config$user,
                 pwd = config$pass,
                 server = 'mssql-2017.labs.wmi.amu.edu.pl',
                 database = 'iliagil')

# Create 'Calendar' data frame
Calendar <- dbGetQuery(con,
                       'SELECT	calendar_year,
                       calendar_month,
                       calendar_year_month,
                       MonthName AS month_name
                       
                       FROM dim_calendar
                       
                       GROUP BY calendar_year,
                       calendar_month,
                       calendar_year_month,
                       MonthName')

Calendar$short_date <- paste0(substr(Calendar$month_name,1,3),"'",substr(Calendar$calendar_year,3,4))

# Remove spaces from months names
Calendar <- as.data.frame(
  apply(Calendar,2, function(x) gsub("\\s+", "", x)))

# Change column types
for (i in c(1:3)){
  Calendar[i] <- lapply(Calendar[i], function(x) as.integer(x))
}


# Create 'Service' data frame
Service <- dbGetQuery(con,
                  'SELECT stype.service_type_name,      
                  fs.service_cost,      
                  cal.calendar_month,     
                  cal.calendar_year,      
                  cal.calendar_year_month,      
                  cal.MonthName month_name,     
                  car.fuel_type,      
                  car.producer,     
                  car.model,      
                  car.production_year
                  
                  FROM fact_service fs       
                  
                  JOIN dim_calendar cal ON fs.service_date_id = cal.DATE_id       
                  JOIN dim_service_type stype ON fs.service_type_id = stype.service_type_id       
                  JOIN dim_car car ON car.car_id = fs.car_id')

# Adjust service type name
Service$service_type_name[Service$service_type_name == 'tire change'] <- 'Tire change'
Service$service_type_name[Service$service_type_name == 'oil service'] <- 'Oil service'

# Unify petrol type
Service$fuel_type[Service$fuel_type == 'diesle'] <- 'Diesel'
Service$fuel_type[Service$fuel_type == 'Petol'] <- 'Petrol'

# Remove spaces from data frame
Service[6:10] <- as.data.frame(
  apply(Service[6:10], 2, function(x) gsub("\\s+", "", x)))


# Change character column types to factors
for (i in 1:ncol(Service)){
  if(sapply(Service[i], class) == 'character'){
    Service[i] <- lapply(Service[i], function(x) as.factor(x))
  }
}

levels(Service$service_type_name)
levels(Service$fuel_type)
levels(Service$model)
levels(Service$producer)


# Create 'Income' data frame
Income <- dbGetQuery(con,
                      'SELECT fr.rental_amount,
                      fr.payment_deadline,
                      car.producer,
                      car.model,
                      car.production_year
                      
                      FROM fact_rental fr
                      
                      JOIN dim_calendar cal ON cal.DATE_id = fr.rental_DATE_id
                      JOIN dim_car car ON car.car_id = fr.car_id')

Income <- Income %>% mutate(payment_year = as.integer(substr(payment_deadline,1,4)),
                  payment_month = as.integer(substr(payment_deadline,6,7)),
                  payment_year_month = as.integer(paste0(substr(payment_deadline,1,4),substr(payment_deadline,6,7)))) %>%
  select(1,3:8)

# Change character column types to factors
for (i in 1:ncol(Income)){
  if(sapply(Income[i], class) == 'character'){
    Income[i] <- lapply(Income[i], function(x) as.factor(x))
  }
}

levels(Income$model)
levels(Income$producer)


# Data frames summary
as.tibble(Service)
as.tibble(Income)
as.tibble(Calendar)



# Create palette
library(scales)

wes_palettes
mp <- c(wes_palette('Moonrise1'),
        wes_palette('Moonrise2'),
        wes_palette('IsleofDogs1'))

show_col(mp)

#my_pal = c('#798E87','#CCC591','#D5D5D3','#F3DF6C','#C27D38','#79402E','#29211F','#8D8680')
my_pal = c('#798BA0','#9FACBD','#798E87','#9BB0A5','#9F818C','#BCA5AE','#8D8680','#C3BBB5','#CCC591','#29211F')
my_pal_2 = c('#CC8C00','#F0F0F0','#402E32')
show_col(my_pal)
show_col(my_pal_2)



########################
# Service costs
########################
# 01 - current year vs previous year
## 1st option - total service
df01_a <- Service %>% group_by(calendar_year) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  filter(calendar_year == 2021 | calendar_year == 2022) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%
  mutate(across(c('calendar_year'),factor))

(g01_a <- ggplot(df01_a, aes(x = calendar_year,
                         y = service_cost_k,
                         # tooltip edition:
                         text = paste(
                           '<b>Service cost:</b>', service_cost_k,'k PLN',
                           '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
    geom_bar(stat = 'identity', fill = my_pal[1]) +
    labs(x = 'Year', y = 'Service cost (k PLN)'))
  
ggplotly(g01_a, tooltip = c('text'))
  

## 2nd option - split for service type
df01_b <- Service %>% group_by(service_type_name, calendar_year) %>%
  filter(calendar_year == 2022 | calendar_year == 2021) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%
  mutate(across(c('calendar_year'),factor))

g01_b <- ggplot(df01_b, aes(x = calendar_year,
                            y = service_cost_k,
                            fill = service_type_name,
                            label = service_cost_k,
                            # tooltip edition:
                            text = paste('<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_fill_manual(name = 'Service type', values = c(my_pal)) +
  
  # "bold" labels 
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=4) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=4.01) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=4.02) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=4.03)

ggplotly(g01_b, tooltip = c('text'))


# 02 - general trend
## 1st option - total service
df02_a <- Service %>% group_by(calendar_year) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%
  mutate(across(c('calendar_year'),factor))

(g02_a <- ggplot(df02_a, aes(x = calendar_year,
                        y = service_cost_k,
                        # tooltip edition:
                        text = paste(
                          '<b>Service cost:</b>', service_cost_k,'k PLN',
                          '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity', fill = my_pal[1]) +
  labs(x = 'Year', y = 'Service cost (k PLN)'))
  
ggplotly(g02_a, tooltip = c('text'))

## 2nd option - split for service type
df02_b <- Service %>% group_by(service_type_name, calendar_year) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%
  mutate(across(c('calendar_year'),factor))

(g02_b <- ggplot(df02_b, aes(x = calendar_year,
                        y = service_cost_k,
                        fill = service_type_name,
                        # tooltip edition:
                        text = paste(
                          '<b>Service cost:</b>', service_cost_k,'k PLN',
                          '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_fill_manual(name = 'Service type', values = c(my_pal)))

ggplotly(g02_b, tooltip = c('text'))


# 03 - general trend with income comparison
# Yearly_income
yearly_income <- Income %>% group_by(payment_year) %>%
  summarise(income_k = (sum(rental_amount))/1000)

df03 <- Service %>% group_by(calendar_year) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  left_join(yearly_income, by=c('calendar_year' = 'payment_year')) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100,
         cost_of_rev = (service_cost_k/income_k)*100) %>%
  replace_na(list(yoy_change = 0))

(g03 <- ggplot(df03) + 
    geom_bar(aes(x = calendar_year,
                 y = service_cost_k,
                 text = paste(
                   '<b>Service cost:</b>', service_cost_k,'k PLN',
                   '\n<b>YOY change:</b>', round(yoy_change,2), '%',
                   '\n----------------------------',
                   '\n<b>Part of income:</b>', round(cost_of_rev,2),'%')),
             stat = 'identity',
             fill = my_pal[1]) +
    geom_line(aes(x = calendar_year,
                  y = 100*cost_of_rev,
                  color = '% of income'),
              stat = 'identity',
              size = 1.25) +
    labs(x = 'Year', y = 'Service cost (k PLN)') +
    scale_color_manual(name = '', values = my_pal_2[1])+
    scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = '% of income')))


ggplotly(g03, tooltip = c('text'))

# no tooltip, just labels
Service %>% group_by(calendar_year, service_type_name) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  ggplot(aes(x = fct_infreq(factor(calendar_year)),
             y = service_cost_k,
             fill = service_type_name,
             label = service_cost_k)) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2]) +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_fill_manual(name = 'Service type', values = my_pal)


########################
# Service activities
########################
# 01 - Service type
(df04 <- Service %>% group_by(calendar_year_month, service_type_name) %>%
  summarise(service_cost_k = round(sum(service_cost/1000),1),
            service_cnt = n()) %>%
  right_join(Calendar, by='calendar_year_month') %>%
  replace_na(list(service_cnt = 0)) %>%
  arrange(calendar_year_month) %>%
  mutate(date = paste0(substr(month_name,1,3),"'",substr(calendar_year,3,4)),
         calendar_year_month = as.character(calendar_year_month)) %>%
  filter(calendar_year == 2021 | calendar_year == 2022))

# Vector with data labels (for labels not to overlap, some values must be empty)
d <- c(df04$date)
odd <- seq(2,26,2)
d <- replace(d,odd,'')

(g04 <- ggplot(df04, aes(x = calendar_year_month,
             y = service_cnt,
             fill = service_type_name,
             label = service_cnt,
             text = paste(
               '<b>Service cost:</b>', service_cost_k,'k PLN'))) +
  geom_col() +
  scale_fill_manual(name = 'Service type', values = my_pal, limits = c('Oil service', 'Tire change')) +
  scale_x_discrete(labels = c(d)) +
  
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3)+
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.01)+
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.02)+
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.03) +
  
  labs(x = '', y = 'Number of serivce activities'))

p <- ggplotly(g04, tooltip = c('text'))

p %>% style(p, showlegend = FALSE, traces = 3)


