########################
# Set up
########################

# install.packages("properties")
# install.packages("kableExtra")
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

as.tibble(Calendar)

# Create 'Service' data frame
Service <- dbGetQuery(con,
                      'SELECT stype.service_type_name,      
                  fs.service_cost,
                  fs.car_id,
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

# Change column types
for (i in 1:ncol(Service)){
  if(sapply(Service[i], class) == 'character'){
    Service[i] <- lapply(Service[i], function(x) as.factor(x))
  }
}

Service$calendar_year_month <- as.integer(Service$calendar_year_month)

as.tibble(Service)

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

# Remove spaces from data frame
Income[2:3] <- as.data.frame(
  apply(Income[2:3], 2, function(x) gsub("\\s+", "", x)))

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
library(wesanderson)
wes_palettes
mp <- c(wes_palette('Moonrise2'),
        wes_palette('Moonrise1'),
        wes_palette('IsleofDogs1'))

#my_pal = c('#798E87','#CCC591','#D5D5D3','#F3DF6C','#C27D38','#79402E','#29211F','#8D8680')
my_pal = c("#798E87",'#9BB0A5',"#C27D38",'#b39f57',"#CCC591",'#798BA0','#9FACBD','#635e68','#a9a5b0', "#78726d","#9e9995",'#c7c7c1')
my_pal_2 = c('#cf9d47','#e8e8e8','#454141')
show_col(my_pal)
show_col(my_pal_2)


palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
for (palname in names(palettes)) {
  pal <- tableau_color_pal(palname)
  max_n <- attr(pal, "max_n")
  show_col(pal(max_n))
  title(main = palname)
}


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


df01_b <- Service %>% group_by(service_type_name, calendar_year) %>%
  filter(calendar_year == 2022 | calendar_year == 2021) %>%
  summarise(service_cost_k = round(sum(service_cost)/1000,1)) %>%
  mutate(yoy_change = ((service_cost_k - lag(service_cost_k))/service_cost_k)*100) %>%
  replace_na(list(yoy_change = 0)) %>%
  mutate(across(c('calendar_year'),factor))

sc_2021 <- df01_b[1,3]+df01_b[3,3]
sc_2022 <- df01_b[2,3]+df01_b[4,3]
diff <- round(((sc_2022-sc_2021)/sc_2022)*100,2)


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

g02_b <- ggplot(df02_b, aes(x = calendar_year,
                            y = service_cost_k,
                            fill = service_type_name,
                            label = service_cost_k,
                            # tooltip edition:
                            text = paste('<b>YOY change:</b>', round(yoy_change,2), '%'))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_fill_manual(name = 'Service type', values = c(my_pal)) +
  
  # "bold" labels
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=3.5) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=3.51) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=3.52) +
  geom_text(position = position_stack(vjust = 0.9), color = my_pal_2[2], size=3.53)

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

g03 <- ggplot(df03, aes(label = service_cost_k)) + 
  geom_bar(aes(x = as.integer(calendar_year),
               y = service_cost_k,
               text = paste(
                 '<b>YOY change:</b>', round(yoy_change,2), '%',
                 '\n----------------------------',
                 '\n<b>Part of income:</b>', round(cost_of_rev,2),'%')),
           stat = 'identity',
           fill = my_pal[1]) +
  geom_line(aes(x = as.integer(calendar_year),
                y = 100*cost_of_rev,
                color = '% of income'),
            stat = 'identity',
            size = 1.25) +
  labs(x = 'Year', y = 'Service cost (k PLN)') +
  scale_color_manual(name = '', values = my_pal_2[1])+
  scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = '% of income')) +
  
  # "bold" labels
  geom_text(aes(x = as.integer(calendar_year), y = service_cost_k),
            position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.5) +
  geom_text(aes(x = as.integer(calendar_year), y = service_cost_k),
            position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.51) +
  geom_text(aes(x = as.integer(calendar_year), y = service_cost_k),
            position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.52) +
  geom_text(aes(x = as.integer(calendar_year), y = service_cost_k),
            position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.53)


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
df04 <- Service %>% group_by(calendar_year_month, service_type_name) %>%
  summarise(service_cost_k = round(sum(service_cost/1000),1),
            service_cnt = n(),
            avg_cost = round((service_cost_k*1000)/service_cnt,1)) %>%
  right_join(Calendar, by='calendar_year_month') %>%
  replace_na(list(service_cnt = 0)) %>%
  arrange(calendar_year_month) %>%
  filter(calendar_year == 2021 | calendar_year == 2022)

# Vector with data labels (for labels not to overlap, some values must be empty)
d <- c(distinct(df04, short_date)$short_date)
d <- replace(d,seq(2,26,2),'')

(g04 <- ggplot(df04, aes(x = as.character(calendar_year_month),
                        y = service_cnt,
                        fill = service_type_name,
                        label = service_cnt,
                        text = paste(
                          '<b>Service cost:</b>', service_cost_k,'k PLN'))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Service type', values = my_pal, limits = c('Oil service', 'Tire change')) +
  scale_x_discrete(labels = c(d)) +
  
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3) +
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.01) +
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.02) +
  geom_text(position = position_stack(vjust = 0.95), color = my_pal_2[2], size=3.03) +
  
  labs(x = '', y = 'Number of serivce activities'))

# Remove "NA" from legend in ggplotly
p <- ggplotly(g04, tooltip = c('text'))
p %>% style(p, showlegend = FALSE, traces = 3)




# 02 - Car model

df05 <- Service %>% group_by(calendar_year_month, producer) %>%
  summarise(service_cost_k = round(sum(service_cost/1000),1),
            service_cnt = n(),
            avg_cost = round((service_cost_k*1000)/service_cnt,1)) %>%
  right_join(Calendar, by='calendar_year_month') %>%
  replace_na(list(service_cnt = 0)) %>%
  arrange(calendar_year_month) %>%
  filter(calendar_year == 2021 | calendar_year == 2022)

# Vector with data labels (for labels not to overlap in ggplotly some values must be empty)
d <- c(distinct(df05, short_date)$short_date)
d <- replace(d,seq(2,26,2),'')

(g05 <- ggplot(df05, aes(x = as.character(calendar_year_month),
                        y = service_cnt,
                        fill = producer,
                        label = service_cnt,
                        text = paste(
                          '<b>Service cost:</b>', service_cost_k,'k PLN'))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Car type', values = my_pal,
                    limits = c(levels(Service$producer))) +
  scale_x_discrete(labels = c(d)) +
  
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.01) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.02) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.03) +
  
  labs(x = '', y = 'Number of serivce activities') +
    facet_wrap(~producer))

# Remove "NA" from legend in ggplotly
p <- ggplotly(g05, tooltip = c('text'))
p %>% style(p, showlegend = FALSE, traces = 3)




df05 <- Service %>% group_by(calendar_year_month, producer) %>%
  summarise(service_cost_k = round(sum(service_cost/1000),1),
            service_cnt = n()) %>%
  right_join(Calendar, by='calendar_year_month') %>%
  replace_na(list(service_cnt = 0)) %>%
  arrange(calendar_year_month) %>%
  filter(calendar_year == 2021 | calendar_year == 2022)

# Vector with data labels (for labels not to overlap in ggplotly some values must be empty)
d <- c(distinct(df05, short_date)$short_date)
d <- replace(d,seq(2,26,2),'')

(g05 <- ggplot(df05, aes(x = as.character(calendar_year_month),
                         y = service_cnt,
                         fill = reorder(producer, service_cnt),
                         label = service_cnt,
                         text = paste(
                           '<b>Service cost:</b>', service_cost_k,'k PLN'))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(name = 'Car type', values = my_pal,
                      limits = c(levels(Service$producer))) +
    scale_x_discrete(labels = c(d)) +
    
    geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3) +
    geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.01) +
    geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.02) +
    geom_text(position = position_stack(vjust = 0.5), color = my_pal_2[2], size=3.03) +
    
    labs(x = '', y = 'Number of serivce activities'))

# Remove "NA" from legend in ggplotly
p <- ggplotly(g05, tooltip = c('text'))
p %>% style(p, showlegend = FALSE, traces = 9)


########################
# Car maintenance costs
########################

as.tibble(Service)

df06 <- Service %>% group_by(producer, model, calendar_year) %>%
  summarise(service_cost_model_k = round(sum(service_cost/1000),1),
            mean_service_cost_model = round(mean(service_cost,1))) %>%
  group_by(producer, calendar_year) %>%
  mutate(service_cost_producer_k = sum(service_cost_model_k),
         mean_service_cost_producer = round(mean(mean_service_cost_model),1))

(df06_a <- df06 %>% filter(calendar_year == 2022) %>%
    select(!c(calendar_year,model,mean_service_cost_model,service_cost_model_k)) %>%
    distinct(producer, .keep_all = TRUE) %>%
    arrange(desc(mean_service_cost_producer)))

df06%>%View()

g06 <- ggplot((df06 %>% filter(calendar_year==2022)),
              aes(x = reorder(producer, service_cost_producer_k),
                  y = service_cost_model_k,
                  fill = reorder(model, service_cost_producer_k),
                  label = paste(service_cost_model_k, model, sep="\n"),
                  text = paste(
                    '<b>Mean service cost for:</b>',
                    '\n<b> Model:</b>', mean_service_cost_model,'PLN',
                    '\n<b> Producer:</b>',mean_service_cost_producer,'PLN'))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = 'Car model', values = c(my_pal_l)) +
  
  geom_text(position = position_stack(vjust = 0.8), color = my_pal_h[2], size=3.5) +
  geom_text(position = position_stack(vjust = 0.8), color = my_pal_h[2], size=3.51) +
  geom_text(position = position_stack(vjust = 0.8), color = my_pal_h[2], size=3.52) +
  geom_text(position = position_stack(vjust = 0.8), color = my_pal_h[2], size=3.53) +
  
  
  theme(legend.position = "none")

ggplotly(g06, tooltip = c('text'))


(df07 <- Service %>% group_by(calendar_year,production_year, fuel_type, car_id, producer, model) %>%
    summarise(mean_service_cost_car = mean(service_cost),
              total_service_cost_car = sum(service_cost),
              cnt = n()) %>% 
    mutate(age = calendar_year-production_year,
           producer_model = paste(producer,model,sep=' ')) %>%
    arrange(desc(total_service_cost_car)))

(g07 <- ggplot(df07 %>% filter(calendar_year== 2022) %>% head(),
               aes(x = reorder(as.character(car_id),total_service_cost_car),
                   y = total_service_cost_car,
                   fill = producer_model,
                   label = total_service_cost_car,
                   text = paste(
                     '<b>Car ID:</b>', car_id,
                     '\n<b>Service events:</b>',cnt,
                     '\n<b>Car age:</b>',age,'years',
                     '\n<b>Fuel type:</b>',fuel_type)))+
    geom_bar(stat = 'identity') + 
    scale_fill_manual(name = 'Car model', values = c(my_pal_l)) +
    labs(y = 'Total service cost in 2022', x = '') +
    geom_text(position = position_stack(vjust = 0.95), color = my_pal_h[2], size=3.0) +
    geom_text(position = position_stack(vjust = 0.95), color = my_pal_h[2], size=3.01) +
    geom_text(position = position_stack(vjust = 0.95), color = my_pal_h[2], size=3.02) +
    geom_text(position = position_stack(vjust = 0.95), color = my_pal_h[2], size=3.03) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x=element_blank())) 
  


ggplotly(g07, tooltip = c('text'))






ggplotly(g07, tooltip = c('text'))




Service %>% filter(car_id==670) %>% View()

df08 <- Service %>% group_by(producer, fuel_type, calendar_year) %>%
  summarise(service_cost_fuel_type_k = round(sum(service_cost/1000),1),
            mean_service_cost_fuel_type = round(mean(service_cost,1))) %>%
  group_by(producer, calendar_year) %>%
  mutate(service_cost_producer_k = sum(service_cost_fuel_type_k),
         mean_service_cost_producer = round(mean(mean_service_cost_fuel_type),1))

g08 <- ggplot((df08 %>% filter(calendar_year==2022)),
              aes(x = reorder(producer, service_cost_producer_k),
                  y = service_cost_fuel_type_k,
                  fill = reorder(fuel_type, service_cost_producer_k),
                  label = paste(service_cost_fuel_type_k, fuel_type, sep="\n"),
                  text = paste(
                    '<b>Mean service cost for:</b>',
                    '\n<b> Model:</b>', mean_service_cost_fuel_type,'PLN',
                    '\n<b> Producer:</b>',mean_service_cost_producer,'PLN'))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = 'Car model', values = c(my_pal_l)) +
  
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_h[2], size=3.5) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_h[2], size=3.51) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_h[2], size=3.52) +
  geom_text(position = position_stack(vjust = 0.5), color = my_pal_h[2], size=3.53) +
  
  labs(x = '', y = 'Service costs (k PLN)') +
  theme(legend.position = "none")

ggplotly(g08, tooltip = c('text'))


