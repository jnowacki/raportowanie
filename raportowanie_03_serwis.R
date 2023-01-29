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

dbListTables(con)



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

Service <- as_tibble(Service)

# Data frame summary
head(Service)
skimr::skim(Service)

# Change character column types to factors
for (i in 1:ncol(Service)){
  if(sapply(Service[i], class) == 'character'){
    Service[i] <- lapply(Service[i], function(x) as.factor(x))
  }
}

skimr::skim(Service)


# Create palette
library(scales)

wes_palettes
mp <- c(wes_palette('Moonrise1'),
        wes_palette('Moonrise2'),
        wes_palette('Moonrise3'),
        wes_palette('IsleofDogs1'))

show_col(mp)

my_pal = c('#798E87','#CCC591','#D5D5D3','#F3DF6C','#C27D38','#79402E','#29211F','#8D8680')
show_col(my_pal)

# Service costs
# 01 - current year vs previous year
# 1st option
g01_a <- Service %>% group_by(calendar_year) %>%
  filter(calendar_year == 2022 | calendar_year == 2021) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%

  ggplot(aes(x = fct_infreq(factor(calendar_year)),
             y = service_cost_k,
             # tooltip edition:
             text = paste(
               '<b>Service cost:</b>', service_cost_k,'k PLN',
               '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity', fill = my_pal[1]) +
  labs(x = 'Year', y = 'Service cost (k PLN)')
  
ggplotly(g01_a, tooltip = c('text'))
  
# 2nd option
# YOY calculation not working :-(
g01_b <- Service %>% group_by(calendar_year, service_type_name) %>%
  filter(calendar_year == 2022 | calendar_year == 2021) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  arrange(service_type_name) %>%
  mutate(yoy_change = (service_cost_k-lag(service_cost_k))/service_cost_k) %>%
  replace_na(list(yoy_change = 0)) %>%

  ggplot(aes(x = fct_infreq(factor(calendar_year)),
             y = service_cost_k,
             fill = service_type_name,
             # tooltip edition:
             text = paste(
               '<b>Service cost:</b>', service_cost_k,'k PLN',
               '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_fill_manual(name = 'Service type', values = c(my_pal))

ggplotly(g01_b, tooltip = c('text'))


# 02 - general trend
g02 <- Service %>% group_by(calendar_year) %>%
    summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
    mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
    replace_na(list(yoy_change = 0)) %>%
    
    ggplot(aes(x = fct_infreq(factor(calendar_year)),
               y = service_cost_k,
               # tooltip edition:
               text = paste(
                 '<b>Service cost:</b>', service_cost_k,'k PLN',
                 '\n<b>YOY change:</b>', round(yoy_change,2), '%'))) +
    geom_bar(stat = 'identity', fill = my_pal[1]) +
    labs(x = 'Year', y = 'Service cost (k PLN)')
  
ggplotly(g02, tooltip = c('text'))
  
# no tooltip, just labels
Service %>% group_by(calendar_year, service_type_name) %>%
  summarise(service_cost = sum(service_cost),
            service_cost_k = round(sum(service_cost)/1000,1)) %>%
  ggplot(aes(x = fct_infreq(factor(calendar_year)),
             y = service_cost,
             fill = service_type_name,
             label = service_cost_k)) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.9)) +
  labs(x = 'Year', y = 'Service cost') +
  scale_fill_manual(name = 'Service type', values = my_pal)
























