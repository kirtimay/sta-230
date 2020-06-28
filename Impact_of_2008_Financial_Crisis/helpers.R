library (readr)
library (knitr)
library(plyr)
library(dplyr)
library (tidyr)
library(readxl)
library(cowplot)

#global_debt
global_debt <- read_excel("global debt.xlsx", 
                              na = "no data")
global_debt <- global_debt[,c(1,57:63)]
global_debt <- global_debt[-c(1,176:177), ]
global_debt <- gather(global_debt, key= year, value="debt", 2:8)
colnames(global_debt) <- c("country", "year", "debt")
global_debt$year <- as.numeric(global_debt$year)
global_debt$country<-revalue(global_debt$country, c("Bahamas, The"="Bahamas", "Gambia, The" ="Gambia", "Hong Kong SAR"="Hong Kong", "Slovak Republic"="Slovakia", "Taiwan Province of China"="Taiwan", "West Bank and Gaza"="Palestine")) 

#unemployment
unemployment <- read_excel("Unemployment.xlsx", col_names = FALSE)
unemployment <- unemployment[-c(1:5), ]
unemployment <- filter(unemployment, unemployment[,3] == "15+")
unemployment <- unemployment[-c(1:5), ]
unemployment <- unemployment[,c(1,4:5)]
colnames(unemployment) <- unemployment[1,] 
unemployment <- unemployment[-c(1:1974), ]
unemployment$year <- as.numeric(unemployment$"2005")
unemployment$unemp <- as.numeric(unemployment$"5.61")
unemployment$country <- unemployment$World
unemployment <- select(unemployment, country, year, unemp)
unemployment <-filter(unemployment, year > 2004)
unemployment <-filter(unemployment, year < 2012)
unemployment$country<-revalue(unemployment$country, c("Cape Verde"="Cabo Verde", "Congo" ="Congo, Republic of", "Hong Kong, China"="Hong Kong", "Iran, Islamic Republic of"="Iran", "Lao People's Democratic Republic"="Lao P.D.R", "Moldova, Republic of"="Moldova", "Occupied Palestinian Territory"="Palestine", "Taiwan, China"="Taiwan", "Viet Nam"="Vietnam", "Venezuela, Bolivarian Republic of"="Venezuela")) 

data <- inner_join(x = global_debt, y = unemployment, by = c("country", "year"))

#savings
savings <- read_excel("savings.xlsx", col_names = FALSE)
savings <- savings[-c(1:3), ]
savings <- savings[,-c(2:49, 57:63)]
colnames(savings) <- savings[1,]
savings <- gather(savings, key= year, value="savings", 2:8)
colnames(savings) <- c("country", "year", "savings")
savings$year <- as.numeric(savings$year)
savings$country<-revalue(savings$country, c("Bahamas, The" = "Bahamas", "Gambia, The" ="Gambia", "Congo, Rep." = "Congo, Republic of", "Hong Kong SAR, China"="Hong Kong", "Iran, Islamic Rep." = "Iran", "Korea, Rep." = "Korea, Republic of", "Slovak Republic"="Slovakia", "West Bank and Gaza"="Palestine" , "Yemen, Rep." = "Yemen" , "Slovak Republic" = "Slovakia" , "Lao PDR" = "Lao P.D.R" , "Cote d'Ivoire" = "Cote d'Ivoire" , "Egypt, Arab Rep." = "Egypt" , "St. Lucia" = "Saint Lucia" , "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines" , "Venezuela, RB" = "Venezuela")) 

data <- inner_join(x = data, y = savings, by = c("country", "year"))

#FDI
fdi <- read_excel("fdi.xls", col_names = FALSE)
fdi <- fdi[, -c(2:49, 57:63)]
colnames(fdi) <- fdi[4,]
fdi <- fdi[-c(1:4),]
fdi <- gather(fdi, key= year, value="fdi", 2:8)
colnames(fdi) <- c("country", "year", "fdi")
fdi$year <- as.numeric(fdi$year)
fdi$country<-revalue(fdi$country, c("Bahamas, The" = "Bahamas", "Gambia, The" ="Gambia", "Congo, Rep." = "Congo, Republic of", "Hong Kong SAR, China"="Hong Kong", "Iran, Islamic Rep." = "Iran", "Korea, Rep." = "Korea, Republic of", "Slovak Republic"="Slovakia", "West Bank and Gaza"="Palestine" , "Yemen, Rep." = "Yemen" , "Slovak Republic" = "Slovakia" , "Lao PDR" = "Lao P.D.R" , "Cote d'Ivoire" = "C?te d'Ivoire" , "Egypt, Arab Rep." = "Egypt" , "St. Lucia" = "Saint Lucia" , "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines" , "Venezuela, RB" = "Venezuela")) 

data <- inner_join(x = data, y = fdi, by = c("country", "year"))

#inflation
inflation <- read_excel("inflation.xlsx", col_names = FALSE)
inflation <- inflation[,-c(2:49, 57:63)]
inflation <- inflation[-c(1:4),]
colnames(inflation) <- inflation[1,]
inflation <- inflation[-c(1), ]
inflation <- gather(inflation, key= year, value="inflation", 2:8)
colnames(inflation) <- c("country", "year", "inflation")
inflation$year <- as.numeric(inflation$year)
inflation$country<-revalue(inflation$country, c("Bahamas, The" = "Bahamas", "Cote d'Ivoire" = "C?te d'Ivoire" , 
                                                "Egypt, Arab Rep." = "Egypt", "Gambia, The" ="Gambia", "Congo, Rep." = "Congo, Republic of", 
                                                "Hong Kong SAR, China"="Hong Kong", "Iran, Islamic Rep." = "Iran", "Korea, Rep." = "Korea, Republic of", 
                                                "Slovak Republic"="Slovakia", "West Bank and Gaza"="Palestine" , "Yemen, Rep." = "Yemen" , 
                                                "Slovak Republic" = "Slovakia" , "Lao PDR" = "Lao P.D.R" , "St. Lucia" = "Saint Lucia" , "St. Vincent and the Grenadines" = 
                                                  "Saint Vincent and the Grenadines" , "Venezuela, RB" = "Venezuela")) 

data <- inner_join(x = data, y = inflation, by = c("country", "year"))

gdp_growth <- read_excel("gdpgrowth.xlsx", col_names = FALSE)
gdp_growth <- gdp_growth[,-c(2:49, 57:63)]
gdp_growth <- gdp_growth[-c(1:4),]
colnames(gdp_growth) <- gdp_growth[1,]
gdp_growth <- gdp_growth[-c(1), ]
gdp_growth <- gather(gdp_growth, key= year, value="gdp_growth", 2:8)
colnames(gdp_growth) <- c("country", "year", "gdp_growth")
gdp_growth$year <- as.numeric(gdp_growth$year)

gdp_growth$country<-revalue(gdp_growth$country, c("Bahamas, The" = "Bahamas", "Cote d'Ivoire" = "C?te d'Ivoire" , 
                                                "Egypt, Arab Rep." = "Egypt", "Gambia, The" ="Gambia", "Congo, Rep." = "Congo, Republic of", 
                                                "Hong Kong SAR, China"="Hong Kong", "Iran, Islamic Rep." = "Iran", "Korea, Rep." = "Korea, Republic of", 
                                                "Slovak Republic"="Slovakia", "West Bank and Gaza"="Palestine" , "Yemen, Rep." = "Yemen" , 
                                                 "Lao PDR" = "Lao P.D.R" , "St. Lucia" = "Saint Lucia" , "St. Vincent and the Grenadines" = 
                                                  "Saint Vincent and the Grenadines" , "Venezuela, RB" = "Venezuela"))
data <- inner_join(x = data, y = gdp_growth, by = c("country", "year"))

#trade balance
tradebalance2005 <- read_excel("tradebalance2005.xlsx")
tradebalance2005 <- tradebalance2005[,-c(1, 3:9, 12:38)]
tradebalance2005$tb2005 <- (tradebalance2005$`Export (US$ Thousand)` - tradebalance2005$`Import (US$ Thousand)`)
tradebalance2005 <- tradebalance2005[,-c(3:4)]
colnames(tradebalance2005) <- c("country", "2005")

tradebalance2006 <- read_excel("tradebalance2006.xlsx", 
                               sheet = "Partner")
tradebalance2006 <- tradebalance2006[,-c(1, 3:9, 12:38)]
tradebalance2006$tb2006 <- (tradebalance2006$`Export (US$ Thousand)` - tradebalance2006$`Import (US$ Thousand)`)
tradebalance2006 <- tradebalance2006[,-c(3:4)]
colnames(tradebalance2006) <- c("country", "2006")

tb <- inner_join(x = tradebalance2005, y = tradebalance2006, by = c("country"))

tradebalance2007 <- read_excel("tradebalance2007.xlsx", 
                               sheet = "Partner")
tradebalance2007 <- tradebalance2007[,-c(1, 3:9, 12:38)]
tradebalance2007$tb2007 <- (tradebalance2007$`Export (US$ Thousand)` - tradebalance2007$`Import (US$ Thousand)`)
tradebalance2007 <- tradebalance2007[,-c(3:4)]
colnames(tradebalance2007) <- c("country", "2007")

tb <- inner_join(x = tb, y = tradebalance2007, by = c("country"))

tradebalance2008 <- read_excel("tradebalance2008.xlsx", 
                               sheet = "Partner")
tradebalance2008 <- tradebalance2008[,-c(1, 3:9, 12:38)]
tradebalance2008$tb2008 <- (tradebalance2008$`Export (US$ Thousand)` - tradebalance2008$`Import (US$ Thousand)`)
tradebalance2008 <- tradebalance2008[,-c(3:4)]
colnames(tradebalance2008) <- c("country", "2008")
tb <- inner_join(x = tb, y = tradebalance2008, by = c("country"))

tradebalance2009 <- read_excel("tradebalance2009.xlsx", 
                               sheet = "Partner")
tradebalance2009 <- tradebalance2009[,-c(1, 3:9, 12:38)]
tradebalance2009$tb2009 <- (tradebalance2009$`Export (US$ Thousand)` - tradebalance2009$`Import (US$ Thousand)`)
tradebalance2009 <- tradebalance2009[,-c(3:4)]
colnames(tradebalance2009) <- c("country", "2009")
tb <- inner_join(x = tb, y = tradebalance2009, by = c("country"))

tradebalance2010 <- read_excel("tradebalance2010.xlsx", 
                               sheet = "Partner")
tradebalance2010 <- tradebalance2010[,-c(1, 3:9, 12:38)]
tradebalance2010$tb2010 <- (tradebalance2010$`Export (US$ Thousand)` - tradebalance2010$`Import (US$ Thousand)`)
tradebalance2010 <- tradebalance2010[,-c(3:4)]
colnames(tradebalance2010) <- c("country", "2010")
tb <- inner_join(x = tb, y = tradebalance2010, by = c("country"))

tradebalance2011 <- read_excel("tradebalance2011.xlsx", 
                               sheet = "Partner")
tradebalance2011 <- tradebalance2011[,-c(1, 3:9, 12:38)]
tradebalance2011$tb2011 <- (tradebalance2011$`Export (US$ Thousand)` - tradebalance2011$`Import (US$ Thousand)`)
tradebalance2011 <- tradebalance2011[,-c(3:4)]
colnames(tradebalance2011) <- c("country", "2011")
tb <- inner_join(x = tb, y = tradebalance2011, by = c("country"))
tb <- gather(tb, key = year, value = trade_balance, 2:8)
tb$year <- as.numeric(tb$year)

tb$country<-revalue(tb$country, c("Bahamas, The" = "Bahamas", "Brunei" = "Brunei Darussalam", "Cape Verde" = "Cabo Verde", 
                                  "Congo, Rep." = "Congo, Republic of", "Cote d'Ivoire" = "C?te d'Ivoire" , "Egypt, Arab Rep." = "Egypt",
                                  "Swaziland" = "Es watini", "Gambia, The" ="Gambia", "Hong Kong, China"="Hong Kong", "Iran, Islamic Rep." 
                                  = "Iran", "Korea, Rep." = "Korea, Republic of", "Macedonia, FYR" = "North Macedonia",
                                  "Slovak Republic"="Slovakia", "Occ.Pal.Terr"="Palestine" , "Serbia, FR(Serbia/Montenegro)" = "Serbia",
                                  "Fm Sudan" = "Sudan", "East Timor" = "Timor Leste", 
                                  "Lao PDR" = "Lao P.D.R" , "St. Lucia" = "Saint Lucia" , "St. Vincent and the Grenadines" = 
                                  "Saint Vincent and the Grenadines"))

data <- inner_join(x = data, y = tb, by = c("country", "year"))
data$savings <- as.numeric(data$savings)
data$fdi <- as.numeric(data$fdi)

data$debt[data$country == "Vietnam" & data$year == 2009] = 51.36
data <- na.omit(data)
data$real_gdp <- (data$gdp_growth / data$inflation)
data <- select(data, -7, -8)
data_06 <- filter(data, year==2006)
data_09 <- filter(data, year==2009)

#difference between 06 and 09

data_cl <- inner_join(x=data_06, y =data_09, by="country")
data_cl$year = (data_cl$year.y-data_cl$year.x)
data_cl$debt = (data_cl$debt.y - data_cl$debt.x)
data_cl$unemp = (data_cl$unemp.y - data_cl$unemp.x)
data_cl$savings = (data_cl$savings.y - data_cl$savings.x)
data_cl$fdi = (data_cl$fdi.y - data_cl$fdi.x)
data_cl$trade_balance = (data_cl$trade_balance.y - data_cl$trade_balance.x)
data_cl$real_gdp = (data_cl$real_gdp.y - data_cl$real_gdp.x)

data_cl <- data_cl[,-c(2:16)]
data_cl <- as.data.frame(data_cl)
rownames(data_cl) <- data_cl$country
data_cl <- data_cl[,-1]

p <- prcomp(data_cl, scale = TRUE)
n <- fviz_contrib(p, choice = "var", axes = 1, top = 6)
c <- fviz_contrib(p, choice = "var", axes = 2, top = 6)
e <- fviz_contrib(p, choice = "var", axes = 3, top = 6)
pca <- plot_grid(n,c,e, nrow = 2)

clust <- kmeans(scale(data_cl), centers = 5)
clust_info <- as.data.frame(clust$cluster)
clust_info$country <- rownames(clust_info)
data_df <- inner_join(x = data, y = clust_info, by = "country")
data_df$time = ifelse(data_df$year >= 2008, 1, 0)
data_df1 <- filter(data_df, data_df$`clust$cluster` == 1 | data_df$`clust$cluster` == 2)
data_df1$treated <- ifelse(data_df1$`clust$cluster` == 1, 1, 0)
data_df1$did = data_df1$time * data_df1$treated
data_df2 <- filter(data_df, data_df$`clust$cluster` == 3 | data_df$`clust$cluster` == 2)
data_df2$treated <- ifelse(data_df2$`clust$cluster` == 3, 1, 0)
data_df2$did = data_df2$time * data_df2$treated
data_df3 <- filter(data_df, data_df$`clust$cluster` == 4 | data_df$`clust$cluster` == 2)
data_df3$treated <- ifelse(data_df3$`clust$cluster` == 4, 1, 0)
data_df3$did = data_df3$time * data_df3$treated
data_df4 <- filter(data_df, data_df$`clust$cluster` == 5 | data_df$`clust$cluster` == 2)
data_df4$treated <- ifelse(data_df4$`clust$cluster` == 5, 1, 0)
data_df4$did = data_df4$time * data_df4$treated
region <- read.csv("all.csv")
region <- region[,c(1,6)]
colnames(region) <- c("country", "region")
region$country <- revalue(region$country, c("Bolivia (Plurinational State of)" = "Bolivia", "Congo, Democratic Republic of the" = "Congo, Republic of", "Congo" = "Congo, Republic of", 
                                            "Côte d'Ivoire" = "C?te d'Ivoire", "Czechia" = "Czech Republic", "Moldova, Republic of" = "Moldova", "Palestine, State of" = "Palestine",
                                            "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom", "United States of America" = "United States")) 
data <- inner_join(x=data, y=region, by= "country")