#install.packages("properties")

library(properties)
library(DBI)
library(odbc)
library(tidyverse)

# rename template.config.properties to config.properties and fill out login data
# NEVER commit this file (it is present in .gitignore)

config <- read.properties("config.properties")

con <- dbConnect(odbc(),
                 driver = "SQL Server",
                 uid = config.user,
                 pwd = config.pass,
                 server = 'mssql-2017.labs.wmi.amu.edu.pl',
                 database = 'iliagil')

dbListTables(con)

Service <- dbGetQuery(con,
                      "SELECT stype.service_type_name,      
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
                  JOIN dim_car car ON car.car_id = fs.car_id")

head(Service)

Service %>% as_tibble()

Service %>% group_by(calendar_year) %>%
  summarise(serivce_cost = sum(service_cost)) +
  ggplot(aes(x=calendar_year,y=service_cost)) +
  geom_bar(stat = 'identity', fill = )