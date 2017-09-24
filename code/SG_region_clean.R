library(dplyr)

df <- readRDS("data/SG_region_data.rds")

### Remove subregions for year 2000
df <- df[(df$Year==2000 & stringr::str_count(df$Level_3, " ")<10) |
          df$Year!=2000 | df$content== "resident_hh_dwelling" ,]

df$Level_3 <- trimws(df$Level_3) 
df$Level_3 <- gsub("- Total", "", df$Level_3)
df$Level_1 <- gsub("-", " ", df$Level_1)
df$Level_1 <- gsub("\\.", " ", df$Level_1)
df$Level_1 <- gsub("X ", "", df$Level_1)
df$Level_1 <- gsub(" +", " ", df$Level_1)
df$Level_1 <- trimws(df$Level_1) 
df$Level_2 <- trimws(df$Level_2) 

region2000 <- unique(df[df$Year==2000,]$Level_3)
region2010 <- unique(df[df$Year==2010,]$Level_3)
region2015 <- unique(df[df$Year==2015,]$Level_3)

### Find common region across all 3 years
common_region <- region2010[region2010 %in% region2000]
common_region <- common_region[common_region %in% region2015]

### Recode income for 2000
df[df$Year==2000 & df$content=="resident_hh_income",]$Level_1 <- 
  gsub("([0-9]) ([0-9][0-9][0-9])", "\\$\\1,\\2", 
       df[df$Year==2000 & df$content=="resident_hh_income",]$Level_1)

df$Level_1 <- ifelse(df$Level_1=="$8,000 Over", "$8,000 & Over", df$Level_1)

df[df$Year==2000 & df$content=="resident_pop_GMI",]$Level_1 <- 
  gsub("([0-9]) ([0-9][0-9][0-9])", "\\$\\1,\\2", 
       df[df$Year==2000 & df$content=="resident_pop_GMI",]$Level_1)

df$Level_1 <- ifelse(df$Level_1=="$6,000 Over", "$6,000 & Over", df$Level_1)

### Recode industries for 2000
df$Level_1 <- ifelse(df$Level_1=="Goods Producing Industries Manufac turing", "Manufacturing", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Goods Producing Industries Construction", "Construction", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Total", "Services Total", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Wholesale Retail Trade", "Services Wholesale & Retail Trade", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Hotels Restaurants", "Services Accommodation & Food Services", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Transport Communi cations", "Services Information & Communications", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Financial Services", "Services Financial & Insurance Services", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Other Services Industries"  , "Other Services Industries", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Services Producing Industries Business Services"  , "Business Services", df$Level_1)

### Recode occupations 
df$Level_1 <- ifelse(df$Level_1=="Legislators, Senior Officials & Managers", "Senior Officials & Managers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Clerical Support Workers", "Clerical Workers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Craftsmen & Related Trades Workers", "Production Craftsmen & Related Workers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Senior Officials Managers", "Senior Officials & Managers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Associate Professionals Technicians", "Associate Professionals & Technicians", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Service Sales Workers", "Service & Sales Workers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Production Craftsmen Related Workers", "Production Craftsmen & Related Workers" , df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Plant Machine Operators Assemblers", "Plant & Machine Operators & Assemblers", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Cleaners Labourers Related Workers", "Cleaners, Labourers & Related Workers"  , df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="Agricultural Fishery Workers" , "Agricultural & Fishery Workers", df$Level_1)

### Harmonise age (2000, 2010) and age-sex (2015) data
temp <- df[df$content=="resident_pop_sex_age",]$Level_1
df[df$content=="resident_pop_sex_age",]$Level_1 <- df[df$content=="resident_pop_sex_age",]$Level_2
df[df$content=="resident_pop_sex_age",]$Level_2 <- temp
df$Level_1 <- gsub("X([0-9]+)", "\\1", df$Level_1)
df$Level_1 <- gsub("Age Group \\(Years\\) ([0-9]+)", "\\1", df$Level_1)
df$Level_1 <- gsub("([0-9]+) ([0-9]+)", "\\1 - \\2", df$Level_1)
df$Level_1 <- ifelse(df$Level_1=="65 Over", "65 & Over", df$Level_1)
df$content <- ifelse(df$content=="resident_pop_sex_age", "resident_pop_age", df$content)

df$Level_1 <- gsub("Condo miniums","Condominiums", df$Level_1)

### Multiply values in 2010 by 1000 so they reflect actual numbers
df$Value <- ifelse(df$Year==2015 & !(df$content=="resident_pop_age" |
                      df$content=="resident_pop_ethnic_sex" | df$content=="resident_pop_dwelling"),
                   df$Value*1000, df$Value)

df <- df[,c("Year", "survey", "content", "Level_1", "Level_2", "Level_3", "Value")]
saveRDS(df, "data/SG_region_data_clean.rds")

### Use to check consistency of Level_1 and Level_2 across years
# group <- unique(df$content)[7]
# df2000 <- df[df$Year==2000 & df$content==group,]
# df2010 <- df[df$Year==2010 & df$content==group,]
# df2015 <- df[df$Year==2015 & df$content==group,]
# 
# unique(df[df$content==group,]$Level_1)
# unique(df[df$content==group,]$Level_2)
# unique(df[df$content==group,]$Level_3)
# 
# unique(df2000$Level_1)
# unique(df2000$Level_2)
# unique(df2010$Level_1)
# unique(df2010$Level_2)
# unique(df2015$Level_1)
# unique(df2015$Level_2)
