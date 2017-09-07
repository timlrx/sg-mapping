library(dplyr)
library(reshape2)

### Read in data to plot
survey <- c("GHS2015", "Census2010")
main_file <- data.frame()

### Compile region level data for GHS2015 and Census2010
for (i in 1:length(survey)){
  survey_path <- paste0("data/",survey[i],"/")
  content <- list.files(paste0("data/",survey[i],"/"))
  content2 <- c("resident_pop_ethnic_sex", "resident_pop_sex_age", "resident_pop_dwelling")
  content <- content[!content %in% content2]
  
  ## test
# survey_path <- "data/GHS2015/"
# content <- "resident_hh_dwelling"
  
  for (j in 1:length(content)){
    file_lvl1 <- read.csv(paste0(survey_path, content[j], "/Level1.csv"),
                     stringsAsFactors=FALSE)
    file_lvl2 <- read.csv(paste0(survey_path, content[j], "/Level2.csv"),
                          stringsAsFactors=FALSE)
    file_lvl1$Value <- as.numeric(file_lvl1$Value)
    file_lvl2$Value <- as.numeric(file_lvl2$Value)
    file_lvl1$Level_3 <- "Total"
    file <- bind_rows(file_lvl1, file_lvl2)
    file$content <- content[j]
    file$survey <- survey[i]
    main_file <- bind_rows(main_file, file)
  }
  
}

### Manually add in for the special cases in content2

file <- read.csv("data/GHS2015/resident_pop_ethnic_sex/Level1.csv",
   stringsAsFactors=FALSE)
file$content <- "resident_pop_ethnic_sex"
file$survey <- "GHS2015"
file$Value <- as.numeric(file$Value)
main_file <- bind_rows(main_file, file)
  
file <- read.csv("data/GHS2015/resident_pop_dwelling/Level1.csv",
                 stringsAsFactors=FALSE)
file$Level_3 <- file$Level_2
file$Level_2 <- "Total"
file$content <- "resident_pop_dwelling"
file$survey <- "GHS2015"
file$Value <- as.numeric(file$Value)
main_file <- bind_rows(main_file, file)



file <- read.csv("data/GHS2015/resident_pop_sex_age/Level1.csv",
    stringsAsFactors=FALSE)
file$content <- "resident_pop_sex_age"
file$survey <- "GHS2015"
file$Value <- as.numeric(file$Value)
main_file <- bind_rows(main_file, file)

file_lvl1 <- read.csv("data/Census2010/resident_pop_ethnic_sex/Level1.csv",
                      stringsAsFactors=FALSE)
file_lvl2 <- read.csv("data/Census2010/resident_pop_ethnic_sex/Level2.csv",
                      stringsAsFactors=FALSE)
file_lvl2 <- file_lvl2 %>%
  select(-Level_3) %>%
  rename(Level_3 = Level_4)
file <- bind_rows(file_lvl1, file_lvl2)
file$content <- "resident_pop_ethnic_sex"
file$survey <- "Census2010"
main_file <- bind_rows(main_file, file)

### Census2000

content <- list.files("data/Census2000/")
content2 <- "resident_pop_ethnic_sex"
content <- content[!content %in% content2]


for (j in 1:length(content)){
  file <- read.csv(paste0("data/Census2000/", content[j],"/OutputFile.csv"), 
                   stringsAsFactors=FALSE, skip=4)
  print(content[j])
  file <- file[!file$Total=="",]
  file[,2:length(file)] <- as.numeric(gsub(",", "", as.matrix(file[,2:length(file)])))
  colnames(file) <- c("Level_3" ,colnames(file)[-1])
  file <- melt(file, id.vars = "Level_3", value.name="Value")
  file <- file %>%
    rename(Level_1 = variable)
  file$Level_2 <- "Total"
  file$Year <- 2000
  file$content <- content[j]
  file$survey <- "Census2000"
  main_file <- bind_rows(main_file, file)
}

### Seperate treatment for resident_pop_ethnic_sex

file <- read.csv("data/Census2000/resident_pop_ethnic_sex/OutputFile.csv", 
                 stringsAsFactors=FALSE, skip=5)
file <- file[!file$Total=="",]
file[,2:length(file)] <- as.numeric(gsub(",", "", as.matrix(file[,2:length(file)])))
colnames(file) <- c("Level_3" ,colnames(file)[-1])
file <- melt(file, id.vars = "Level_3", value.name="Value")

file$Level_1 <- ifelse(grepl("1", file$variable), "Chinese",
                  ifelse(grepl("2", file$variable), "Malays",
                    ifelse(grepl("3", file$variable), "Indians",
                      ifelse(grepl("4", file$variable),"Others", "Total"))))
file$variable <- gsub("\\.[0-9]", "", file$variable)

file <- file %>%
  rename(Level_2 = variable)
file$Year <- 2000
file$content <- "resident_pop_ethnic_sex"
file$survey <- "Census2000"
main_file <- bind_rows(main_file, file)

saveRDS(main_file, "data/SG_region_data.rds")