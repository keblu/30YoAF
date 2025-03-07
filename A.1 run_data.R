rm(list = ls())

library("tidyverse")
library("readxl")
library("DataExplorer")
library("dplyr")

######################################

# Merge the excel files

#####################################

########## Web of Science data

mergefiles <- function(path = ""){
temp = paste(path,list.files(path = path, pattern="*.xls"), sep = "/", collapse = NULL)
myfiles = lapply(temp, read_excel)
merged = do.call(rbind, myfiles)

merged$abbrev = sapply(stringr::str_split(path,pattern = "/"),FUN = function(x) x[2])
merged$origin = "WOS"
merged$path = path
return(merged)
}


listf = list.dirs("data", recursive = FALSE)


mergecomplete = lapply(listf, mergefiles)

# Cleaning
fulldata1 = data.table::rbindlist(mergecomplete)
fulldata1 = as.data.frame(fulldata1)
fulldata1 = fulldata1[fulldata1$`Document Type` == "Article" | is.na(fulldata1$`Document Type`),]


####### Scopus Data

mergefiles_scopus <- function(path = ""){
  temp = paste(path,list.files(path = path, pattern="*.csv"), sep = "/", collapse = NULL)
  if(!stringr::str_detect(temp,pattern = "csv")){
    return(NULL)
  }
  myfiles = lapply(temp, read_csv)
  merged = data.table::rbindlist(myfiles,fill = TRUE)
  
  merged$abbrev = sapply(stringr::str_split(path,pattern = "/"),FUN = function(x) x[2])
  merged$origin = "scopus"

  return(merged)
}

mergecomplete = lapply(listf, mergefiles_scopus)
fulldata2 = data.table::rbindlist(mergecomplete,fill = TRUE)

# Cleaning

fulldata2 = fulldata2[fulldata2$`Document Type` == "Article" | fulldata2$`Document Type` == "Review",]
fulldata2$Authors = stringr::str_replace_all(fulldata2$Authors,pattern = "\\.,",";")
fulldata2$Authors = stringr::str_replace_all(fulldata2$Authors,pattern = "\\.","")
fulldata2$Authors = stringr::str_replace_all(fulldata2$Authors,pattern = " ",", ")
fulldata2$Authors = stringr::str_replace_all(fulldata2$Authors,pattern = ";,",";")
fulldata2 = data.frame(fulldata2)
fulldata2$funding = apply(fulldata2,MARGIN = 1,FUN = function(x) paste0(x[20:30],collapse = "."))
fulldata2$funding[fulldata2$funding == "NA.NA.NA.NA.NA.NA.NA.NA.NA.NA.NA"] = NA
fulldata2$funding[fulldata2$funding == ".NA.NA.NA.NA.NA.NA.NA.NA.NA.NA"] = NA
fulldata2$funding[fulldata2$funding == "..NA.NA.NA.NA.NA.NA.NA.NA.NA"] = NA
fulldata2$funding[fulldata2$funding == "...NA.NA.NA.NA.NA.NA.NA.NA"] = NA
fulldata2$funding[fulldata2$funding == "....NA.NA.NA.NA.NA.NA.NA"] = NA
fulldata2 = fulldata2[fulldata2$Abstract != "[No abstract available]",]

########## import data built manually directly from the journal's website

fulldata3 = rio::import("data/DataManual.xlsx")

#Cleaning 

aut1 = sapply(stringr::str_split(fulldata3$Author1,pattern = "\\("),FUN = function(x) x[1])
aut1[is.na(aut1)] = ""
aut1 = sapply(stringr::str_split(aut1,n = 2,pattern = " "),FUN = function(x) paste0(x[2],", ",x[1]))
aut1[aut1 == "NA, "] = ""
aut2 = sapply(stringr::str_split(fulldata3$Author2,pattern = "\\("),FUN = function(x) x[1])
aut2[is.na(aut2)]= ""
aut2 = sapply(stringr::str_split(aut2,n = 2,pattern = " "),FUN = function(x) paste0(x[2],", ",x[1]))
aut2[aut2 == "NA, "] = ""
aut3 = sapply(stringr::str_split(fulldata3$Author3,pattern = "\\("),FUN = function(x) x[1])
aut3[is.na(aut3)]  = ""
aut3 = sapply(stringr::str_split(aut3,n = 2,pattern = " "),FUN = function(x) paste0(x[2],", ",x[1]))
aut3[aut3 == "NA, "] = ""
aut4 = sapply(stringr::str_split(fulldata3$Author4,pattern = "\\("),FUN = function(x) x[1])
aut4[is.na(aut4)] = ""
aut4 = sapply(stringr::str_split(aut4,n = 2,pattern = " "),FUN = function(x) paste0(x[2],", ",x[1]))
aut4[aut4 == "NA, "] = ""
aut  =cbind(aut1,aut2,aut3,aut4)
fulldata3$authors = apply(aut,MARGIN = 1,FUN = function(x) paste0(x,collapse = "; "))
fulldata3$authors = stringr::str_remove_all(fulldata3$authors,pattern = "; ; ")
fulldata3$authors = stringr::str_remove_all(fulldata3$authors,pattern = "; $")
fulldata3$authors = stringr::str_squish(fulldata3$authors )
fulldata3$authors = stringr::str_replace_all(fulldata3$authors,pattern = " ; ","; ")
fulldata3$authors = stringr::str_replace_all(fulldata3$authors,pattern = " , ",", ")
fulldata3$authors[fulldata3$authors == ""] = NA

stringr::str_split(fulldata3$authors,pattern = " ")
add1 =  sapply(stringr::str_split(fulldata3$Author1,pattern = "\\("),FUN = function(x) x[2])
add1[is.na(add1)] = ""
add2 =  sapply(stringr::str_split(fulldata3$Author2,pattern = "\\("),FUN = function(x) x[2])
add2[is.na(add2)]= ""
add3 =  sapply(stringr::str_split(fulldata3$Author3,pattern = "\\("),FUN = function(x) x[2])
add3[is.na(add3)]  = ""
add4 =  sapply(stringr::str_split(fulldata3$Author4,pattern = "\\("),FUN = function(x) x[2])
add4[is.na(add4)] = ""

add  =cbind(add1,add2,add3,add4)
fulldata3$address = apply(add,MARGIN = 1,FUN = function(x) paste0(x,collapse = ";"))
fulldata3$origin = "MANUAL"

# Merge all sources

fulldata = data.frame("Source Title" = c(fulldata1$`Source Title`,fulldata2$Source.title,fulldata3$Journal),
                      "Authors" = c(fulldata1$Authors,fulldata2$Authors,rep(NA,nrow(fulldata3))),
                      "Scopus Authors ID" = c(rep(NA,nrow(fulldata1)),fulldata2$Author.s..ID,rep(NA,nrow(fulldata3))),
                      "Full Name" = c(fulldata1$`Author Full Names`,rep(NA,nrow(fulldata2)),fulldata3$authors),
                      "Article Title" = c(fulldata1$`Article Title`,fulldata2$Title,fulldata3$Title),
                      "Abstract" = c(fulldata1$Abstract,fulldata2$Abstract,fulldata3$Abstract),
                      "Publication Year" = c(fulldata1$`Publication Year`,fulldata2$Year,fulldata3$Year),
                      "Funding Orgs" = c(fulldata1$`Funding Orgs`,fulldata2$funding,fulldata3$Funding),
                      "Funding Orgs2" = c(rep(NA,nrow(fulldata1)),rep(NA,nrow(fulldata2)),fulldata3$Funding),
                      "Issue" = c(fulldata1$Issue,fulldata2$Issue,fulldata3$Issue),
                      "Volume" = c(fulldata1$Volume,fulldata2$Volume,fulldata3$Volume),
                      "DOI" = c(fulldata1$DOI,fulldata2$DOI,fulldata3$DOI),
                      "abbrev" = c(fulldata1$abbrev,fulldata2$abbrev,fulldata3$abbrev),
                      "Addresses" = c(fulldata1$Addresses,fulldata2$Affiliations,fulldata3$address),
                      "origin" = c(fulldata1$origin,fulldata2$origin,fulldata3$origin))

# Fix for some Author 

fulldata$Authors = stringr::str_remove_all(fulldata$Authors,pattern = "; Jr")
fulldata$Authors = stringr::str_remove_all(fulldata$Authors,pattern = "; JR")
fulldata$Authors = stringr::str_remove_all(fulldata$Authors,pattern = "; III")
fulldata$Authors = stringr::str_remove_all(fulldata$Authors,pattern = "; IV")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = ",,","; ")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Campbell, TL; II;","Campbell, TL;")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Campbell, TL; II","Campbell, TL")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Smith, RL; II","Smith, RL")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = ", BHJ","; BHJ")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Depken, CA; II;","Depken, CA;")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Depken, CA; II","Depken, CA")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Barnett, W; II;","Barnett, W;")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Knotek, ES; II;","Knotek, ES;")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "Thompson, DJ; II","Thompson, DJ")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = ", GBM","; GBM")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "; Jwintoki@kuedu","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "; Jwintoki@kuedu","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = " Prof;","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = " Prof;","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "C&cedil;o&die;teliog&caron;","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = " \\(Ben\\);","")
fulldata$Authors = stringr::str_replace_all(fulldata$Authors,pattern = "; \\(Jessica\\)$","")

# Remove copyright symbol
fulldata$Abstract = gsub(" \\(C).*","",fulldata$Abstract)
fulldata$Abstract = gsub(" \\(c).*","",fulldata$Abstract)
fulldata$Source.Title <- toupper(fulldata$Source.Title)

###############################################################################

### Process author names for gender extraction

split_abv_author = unlist(stringr::str_split(fulldata$Authors[!is.na(fulldata$Full.Name) & fulldata$origin != "MANUAL"],";"))
split_abv_fullname = unlist(stringr::str_split(fulldata$Full.Name[!is.na(fulldata$Full.Name) & fulldata$origin != "MANUAL"],";"))

xx = cbind(split_abv_author,split_abv_fullname)
xx[,1] = stringr::str_trim(xx[,1])
xx[,2] = stringr::str_trim(xx[,2])
xx = xx[gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(xx[,1]))) !=  gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(xx[,2]))),]
xx = xx[order(nchar(xx[,2]),decreasing = TRUE),]
xx = xx[!duplicated(xx[,1]),]
fulldata$Full.Name[which(fulldata$Authors == fulldata$Full.Name)] = NA

missing_id = which(is.na(fulldata$Full.Name))
missing_FN = fulldata$Authors[missing_id]
FN_fill = ""
for(i in 1:length(missing_FN)){
  target = gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(stringr::str_split(missing_FN[i],pattern = ";")[[1]])))
  potential = gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim( xx[,1])))
 
  FN_fill[i] = paste0(xx[match(target,potential),2],collapse = "; ")
}
fulldata$Full.Name[missing_id] = FN_fill


### if full name is missing, go search full name on scopus by scopys ID

#indicator = lapply(stringr::str_split(fulldata$Full.Name[fulldata$origin == "scopus"],pattern = ";"),FUN = function(x) stringr::str_detect(x, pattern = "NA"))
#aut.id = lapply(stringr::str_split(fulldata$Scopus.Authors.ID[fulldata$origin == "scopus"][],pattern = ";"),FUN = function(x) x[1:(length(x)-1)])

#fulldata$Authors[fulldata$origin == "scopus"][which(sapply(indicator,FUN = length) != sapply(aut.id,FUN = length))]
#fulldata$Scopus.Authors.ID[fulldata$origin == "scopus"][which(sapply(indicator,FUN = length) != sapply(aut.id,FUN = length))]

#missing_abv = unlist(stringr::str_split(fulldata$Authors[fulldata$origin == "scopus"],pattern = ";"))[unlist(indicator) == TRUE]
#missing_code =unlist(aut.id)[unlist(indicator) == TRUE]
#missing = cbind(stringr::str_trim(missing_abv),missing_code)

# require(reticulate)
# 
# elsapy.elsclient = reticulate::import('elsapy.elsclient')
# elsapy.elsprofile  = reticulate::import('elsapy.elsprofile')
# elsapy.elsdoc = reticulate::import('elsapy.elsdoc')
# elsapy.elssearch = reticulate::import('elsapy.elssearch')
# 
# client = elsapy.elsclient$ElsClient(api_key = "Enter ID here")

#missing = as.data.frame(missing)
#missing = missing[!duplicated(missing[,2]),]
#missing$fullname = NA
#for(i in 4006:4006){
#  cat(i)
#  test = elsapy.elsprofile$ElsAuthor(author_id = missing$missing_code[i])
#  if(test$read(els_client = client)){
#    missing$fullname[i] = paste0(test$last_name,", ",test$first_name)
#  }
#}

#rio::export(missing,file = "outputs/missing_FN.xlsx",overwrite = TRUE)

# Procee Scopus FUll name including Missing name from article

missing = rio::import("outputs/missing_FN.xlsx")
test = stringr::str_split(fulldata$Scopus.Authors.ID[fulldata$origin == "scopus"][],pattern = ";")
test_FN = stringr::str_split(fulldata$Full.Name[fulldata$origin == "scopus"][],pattern = ";")
FN_fill = NA
for(i in 1:length(test)){
  match_code = match(test[[i]],missing$missing_code)
  match_code = match_code[1:(length(match_code)-1)]
  FN_tmp = match_code
  FN_tmp[is.na(match_code)] = test_FN[[i]][is.na(match_code)]
  FN_tmp[!is.na(match_code)] = missing$fullname[as.numeric(match_code[!is.na(match_code)])]
  FN_fill[i] =  paste0(FN_tmp,collapse = "; ")
}

fulldata$Full.Name[fulldata$origin == "scopus"] = stringr::str_squish(FN_fill)
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#252;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#769;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#225;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#233;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#250;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#243;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#243;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#231;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#199;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "&#237;",replacement = "?")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "Allen;, David Edmund;",replacement = "Allen, David Edmund;")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "Shin&apos;",replacement = "Shin'")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,pattern = "C?o&die;teliog&caron;",replacement = "")
split_abv_author = unlist(stringr::str_split(fulldata$Authors[!stringr::str_detect(fulldata$Full.Name,pattern = "NA") & fulldata$origin != "MANUAL"],";"))
split_abv_fullname = unlist(stringr::str_split(fulldata$Full.Name[!stringr::str_detect(fulldata$Full.Name,pattern = "NA") & fulldata$origin != "MANUAL"],";"))

xx = cbind(split_abv_author,split_abv_fullname)
xx[,1] = stringr::str_trim(xx[,1])
xx[,2] = stringr::str_trim(xx[,2])
xx = xx[gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(xx[,1]))) !=  gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(xx[,2]))),]
xx = xx[order(nchar(xx[,2]),decreasing = TRUE),]
xx = xx[!duplicated(xx[,1]),]
fulldata$Full.Name[stringr::str_detect(fulldata$Full.Name,pattern = "NA")] = NA

missing_id = which(is.na(fulldata$Full.Name))
missing_FN = fulldata$Authors[missing_id]
FN_fill = ""
for(i in 1:length(missing_FN)){
  target = gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim(stringr::str_split(missing_FN[i],pattern = ";")[[1]])))
  potential = gsub("[^A-Za-z0-9 ]","",stringr::str_to_lower(stringr::str_trim( xx[,1])))
  
  FN_fill[i] = paste0(xx[match(target,potential),2],collapse = "; ")
}
fulldata$Full.Name[missing_id] = FN_fill
fulldata = fulldata[fulldata$Publication.Year >= 1992 & fulldata$Publication.Year <= 2021,]

total_missing = cbind(stringr::str_trim(unlist(stringr::str_split(fulldata$Full.Name[(stringr::str_detect(fulldata$Full.Name,pattern = "NA"))],";"))),
      stringr::str_trim(unlist(stringr::str_split(fulldata$Authors[(stringr::str_detect(fulldata$Full.Name,pattern = "NA"))],";"))))
total_missing = total_missing[total_missing[,1] == "NA",]
total_missing = total_missing[!duplicated(total_missing),]

########################## Extract First name and associate to gender

###############################################################################
#f.name = unique(sapply(stringr::str_split(unlist(stringr::str_split(fulldata$Full.Name,pattern = "; ")),", "),FUN = function(x) x[2]))
#data.frame( "First Name" = f.name)
#rio::export(data.frame(first_name = u.name),file = "outputs/first_name.xlsx",overwrite = TRUE)
######################################

# at this point the first name extracted are processed into genderAPI 

first_name_annotated = rio::import("outputs/first_name_annotated.xlsx")
colnames(first_name_annotated)[1] = "target"
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,"Vu Tran;"," Tran, Vu;")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name," Vu Tran"," Tran, Vu")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name," Vu Tran"," Tran, Vu")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,"Tung Lam Dang","Dang, Tung Lam")
fulldata$Full.Name = stringr::str_replace_all(fulldata$Full.Name,"Hoang Luong","Luong, Hoang")

######################### Associate to Gender

gender_count = matrix(0, nrow = nrow(fulldata), ncol = 2)
gender_count_accurate = matrix(0, nrow = nrow(fulldata), ncol = 2)
missing_gender = NA
id = NA
abbrev = NA
for(i in 1:nrow(fulldata)){
  
  first.name_art = unique(sapply(stringr::str_split(unlist(stringr::str_split(fulldata$Full.Name[i],pattern = "; ")),", "),FUN = function(x) x[2]))
  first.name_abv = unique(sapply(unlist(stringr::str_split(fulldata$Authors[i],pattern = "; ")),FUN = function(x) x[1]))
  if(any(is.na(match(first.name_art,first_name_annotated$target)))){
    abbrev = c(abbrev, first.name_abv[is.na(match(first.name_art,first_name_annotated$target))])
    id = c(id, i)
    missing_gender = c(missing_gender,first.name_art[is.na(match(first.name_art,first_name_annotated$target))])
  }
  gender = first_name_annotated$ga_gender[match(first.name_art,first_name_annotated$target)]
  accuracy = first_name_annotated$ga_accuracy[match(first.name_art,first_name_annotated$target)]
  gender_accurate = gender[accuracy >= 75]
  
  gender = table(gender)
  gender_count[i,1] = gender["male"]
  gender_count[i,2] = gender["female"]
  
  gender_accurate = table(gender_accurate)
  gender_count_accurate[i,1] = gender_accurate["male"]
  gender_count_accurate[i,2] = gender_accurate["female"]
  
}
gender_count[is.na(gender_count)] = 0
gender_count_accurate[is.na(gender_count_accurate)] = 0

#####################################
colnames(gender_count) = colnames(gender_count_accurate) =  c("male","female")
gender_count = as.data.frame(gender_count)
gender_count[,3] = NA
gender_count[,3] = rowSums(gender_count[,1:2]) == 0
colnames(gender_count) = c("male","female","no_idenfication")

gender_count_accurate = as.data.frame(gender_count_accurate)
gender_count_accurate[,3] = NA
gender_count_accurate[,3] = rowSums(gender_count_accurate[,1:2]) == 0
colnames(gender_count_accurate) = c("male_accurate","female_accurate","no_idenfication_accurate")

fulldata = cbind(fulldata,gender_count,gender_count_accurate)

write.csv(fulldata, "data/merged_data.csv")

########################  Processing articles #############################################

# Getting data
merged_data <- read.csv("data/merged_data.csv")

merged_data = merged_data[!is.na(merged_data$Publication.Year),]
merged_data = merged_data[merged_data$Publication.Year >= 1992,]
merged_data = merged_data[merged_data$Publication.Year <= 2021,]

nrow(merged_data)

# Removing duplicates and articles with missing abstract

sum(is.na(merged_data$Abstract))
merged_data = merged_data[!is.na(merged_data$Abstract),]
# No of Copies
length(merged_data$Abstract)-length(unique(merged_data$Abstract))

clean_title = stringr::str_to_lower(merged_data$Article.Title)
clean_title = stringr::str_remove(clean_title,"\\.")
clean_title = stringr::str_remove(clean_title,"\\.")
clean_title = stringr::str_replace_all(clean_title," - "," ")
clean_title = stringr::str_remove_all(clean_title,":")

merged_data = merged_data[-which(duplicated(clean_title,fromLast = TRUE)),]

clean_abstract = stringr::str_to_lower(merged_data$Abstract)
clean_abstract = stringr::str_remove(clean_abstract,"\\.")

merged_data = merged_data[-which(duplicated(clean_abstract,fromLast = TRUE)),]

DOI = stringr::str_remove_all(merged_data$DOI,pattern = "https://doi.org/")
DOI = stringr::str_to_lower(DOI)
DOI[which(is.na(DOI) | DOI == "")] = 1:sum(is.na(DOI) | DOI == "")

merged_data = merged_data[-which(duplicated(DOI,fromLast = TRUE)),]

# Fixing Journal name 
merged_data$Source.Title[merged_data$Source.Title == "JOURNAL OF INTERNATIONAL FINANCIAL MARKETS, INSTITUTIONS AND MONEY"] = "JOURNAL OF INTERNATIONAL FINANCIAL MARKETS INSTITUTIONS & MONEY"
merged_data$Source.Title[merged_data$Source.Title == "JOURNAL OF BANKING AND FINANCE"] = "JOURNAL OF BANKING & FINANCE"
merged_data$Source.Title[merged_data$Source.Title == "JOURNAL OF MONEY, CREDIT AND BANKING"] = "JOURNAL OF MONEY CREDIT AND BANKING"
merged_data$Source.Title[merged_data$Source.Title == "PACIFIC BASIN FINANCE JOURNAL"] = "PACIFIC-BASIN FINANCE JOURNAL"
merged_data$Source.Title[merged_data$Source.Title == "THE JOURNAL OF FINANCE"] = "JOURNAL OF FINANCE"
merged_data$Source.Title[merged_data$Source.Title == "THE JOURNAL OF REAL ESTATE FINANCE AND ECONOMICS"] = "JOURNAL OF REAL ESTATE FINANCE AND ECONOMICS"
merged_data$Source.Title[merged_data$Source.Title == "THE JOURNAL OF REAL ESTATE FINANCE AND ECONOMICS"] = "JOURNAL OF REAL ESTATE FINANCE AND ECONOMICS"
# Any na's ?
sapply(merged_data, function(x) sum(is.na(x)))


merged_data$Funding.Orgs_dummy = NA
# Create funding variable
merged_data$Funding.Orgs_dummy[!is.na(merged_data$Funding.Orgs) | !is.na(merged_data$Funding.Orgs2)] <- 1
merged_data$Funding.Orgs_dummy[is.na(merged_data$Funding.Orgs) & is.na(merged_data$Funding.Orgs2)] <- 0
merged_data = merged_data[!stringr::str_detect(merged_data$Article.Title,pattern = "Erratum"),]


# Keeping important columns
merged_data <- merged_data[,c("Abstract","Publication.Year", "Funding.Orgs_dummy", "Authors","Full.Name","Scopus.Authors.ID",
                       "Source.Title",
                       "Addresses","Article.Title","Issue","Volume","abbrev","male","female","no_idenfication", "DOI","origin","male_accurate","female_accurate","no_idenfication_accurate")]
names(merged_data)[names(merged_data) == "Publication.Year"] <- "Year"

names(merged_data)[names(merged_data) == "Source.Title"] <- "Journal"
names(merged_data)[names(merged_data) == "Funding.Orgs_dummy"] <- "Funding"


table(merged_data$Journal)

# Remove NA
sapply(merged_data, function(x) sum(is.na(x)))
merged_data <- merged_data[complete.cases(merged_data[, c("Year","Journal","Abstract")]), ]
nrow(merged_data)

# Get ID
merged_data$ID <- seq.int(nrow(merged_data))

# Keeping only data from 1992
merged_data <- merged_data[merged_data$Year>1991,]


write.csv(merged_data, "data/prepped_data.csv", row.names = FALSE)


######################################

# Add University Information

#####################################


# Getting data
uni_data <- read.csv("data/prepped_data.csv")

uni_data$Addresses <- as.character(uni_data$Addresses)

add = sapply(stringr::str_split(uni_data$Scopus.Authors.ID[is.na(uni_data$Addresses) & uni_data$origin == "scopus"],";"), FUN =function(x) x[1:(length(x)-1)])

# checking for missing address. If missing got get it on Scopus with author ID

#missing_add = unlist(add)

#  require(reticulate)
# 
#  elsapy.elsclient = reticulate::import('elsapy.elsclient')
#  elsapy.elsprofile  = reticulate::import('elsapy.elsprofile')
#  elsapy.elsdoc = reticulate::import('elsapy.elsdoc')
#  elsapy.elssearch = reticulate::import('elsapy.elssearch')
# 
#  client = elsapy.elsclient$ElsClient(api_key = "e0affa1a8fda912f18118a10a69ea731")
# 
#  missing_add = as.data.frame(missing_add)
# missing_add = missing_add[!duplicated(missing_add[,1]),,drop = FALSE]
# missing_add$addresse = NA
# missing_add$afname = NA
# for(i in 1:nrow(missing_add)){
#   cat(i)
#   test = elsapy.elsprofile$ElsAuthor(author_id = missing_add$missing_add[i])
#   if(test$read(els_client = client)){
#     
#   error = try({
#     missing_add$addresse[i] = paste0(sapply(test$data$`author-profile`$`affiliation-current`$affiliation,
#                                             FUN = function(x) x$`ip-doc`$`preferred-name`$`$`),collapse = "; ")})
#   }
#   error = try({
#     if(is.na(missing_add$addresse[i])) {
#       missing_add$addresse[i] = test$data$`author-profile`$`affiliation-current`$affiliation$`ip-doc`$`preferred-name`$`$`
#     }
#   })
#   
#   error = try({
#     missing_add$afname[i] = paste0(sapply(test$data$`author-profile`$`affiliation-current`$affiliation,
#                                             FUN = function(x) x$`ip-doc`$`parent-preferred-name`$`$`),collapse = "; ")})
#   error = try({
#     if(is.na(missing_add$addresse[i])) {
#       missing_add$afname[i] = test$data$`author-profile`$`affiliation-current`$affiliation$`ip-doc`$`parent-preferred-name`$`$`
#     }
#   })
#   
# }
# missing_add$good_adresss = missing_add$addresse
# missing_add$afname = stringr::str_replace_all(missing_add$afname,pattern = "NULL",replacement = "")
# missing_add$afname = stringr::str_replace_all(missing_add$afname,pattern = "^;",replacement = "")
# missing_add$afname =stringr::str_trim(missing_add$afname)
# missing_add$afname[missing_add$afname == ""] = NA
# missing_add$good_adresss[!is.na(missing_add$afname)] = missing_add$afname[!is.na(missing_add$afname)]
#rio::export(missing_add,file = "outputs/missing_adresses.xlsx")

missing_adresses = rio::import(file = "outputs/missing_adresses.xlsx")
fn_add = NA
for(i in 1:length(add)){
  
  fn_add[i] = paste0(missing_adresses$good_adresss[match(add[[i]],missing_adresses$missing_add)],collapse = "; ")
}
uni_data$Addresses[is.na(uni_data$Addresses) & uni_data$origin == "scopus"] = fn_add

uni_data$Addresses = stringr::str_replace_all(uni_data$Addresses,pattern = "\n",replacement = " ")
uni_data$Countries = uni_data$Addresses
uni_data$Countries <- gsub("[[:punct:]\n]","",uni_data$Countries)
raw2 <- strsplit(uni_data$Countries, " ")
country_list = countrycode::codelist[!is.na(countrycode::codelist$cowc),]
countries = matrix(0, nrow = length(raw2), ncol =  nrow(country_list))
for(i in 1:length(raw2)){
  if(length(raw2[[i]]) == 0){
    next()
  }
  if(is.na(raw2[[i]])){
    next()
  }
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "wales")] = "UKG"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "scotland")] = "UKG"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "england")] = "UKG"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "korea")] = "south korea"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "arabia")] = "saudi arabia"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "samoa")] = "american samoa"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "barbuda")] = "antigua & barbuda"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "bosnia")] = "bosnia & herzegovina"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "burkina")] = "burkina faso"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "zealand")] = "new zealand"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "fed")] = "USA"
  raw2[[i]][which(stringr::str_to_lower(raw2[[i]]) == "fed")] = "USA"
  id0 = which(stringr::str_detect(stringr::str_to_lower(uni_data$Countries[i]),stringr::str_to_lower(country_list$country.name.en)))
  id1 = match(raw2[[i]],table = country_list$cowc)

  id2 = match(stringr::str_to_lower(raw2[[i]]),table = stringr::str_to_lower(country_list$country.name.en))
  id = c(id0,id1,id2)
  countries[i, unique(id[!is.na(id)])] = 1
}
colnames(countries) = country_list$country.name.en
countries[,"Georgia"] = 0
countries[,"Vanuatu"] = 0
countries[,"Bavaria"] = 0
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "hong kong"),"China"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "czech"),"Czechia"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "arab emirates"),"United Arab Emirates"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "wharton"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "yorkny"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "calif"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cambridge"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "ohio"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "angelesca"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "univcoll"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "michigan"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "northwestern"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "duke"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "coll"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "arbormi"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "boston"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "hillma"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "london"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "texas"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "claraca"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "yale"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "new york"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "illinois"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cornell"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "chicago"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "dallas"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "carolina"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "philadelphia"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "purdue"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "rochester"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "washington"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "atlanta"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "oklahoma"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "new jersey"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "new brunswick"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "new brunswick"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cleveland"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "toronto"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "bowling green state"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "mcmaster"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "concordia"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "rutgers"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "fairfield"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "auburn"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "jacksonville"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "newark"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "kent"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "duquesne"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "houston"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "milwaukee"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "memphis"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "ucla"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "norwegian"),"Norway"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "limburg"),"Netherlands"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "queens"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "monash"),"Australia"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "florida"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "virginia"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "aarhus "),"Denmark"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "federal reserve"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "stockholm"),"Sweden"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "arkansas"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "mankato"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "villanova"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cowan"),"Australia"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "curtin"),"Australia"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "manitoba"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "suffolk"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "campbell"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "tennessee"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "standford"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "minnesota"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "clemson"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "louisianna"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "connecticut"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "scotland"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "brown"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "mississippi"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "utah"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "iowa"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "notre dame"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "alabama"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "landcaster"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "san luis"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "baltimore"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "paris"),"France"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "new orleans"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "kyoto"),"Japan"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "georgetown"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "lausanne"),"Switzerland"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "tilburg"),"Netherlands"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "italia"),"Italy"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "missouri"),"Italy"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "frobisher"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "simon fraser"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "harvard"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "haas"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "amsterdam"),"Netherlands"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "ontario"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "leuven"),"Belgium"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "louvain"),"Belgium"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "montr?al"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "helsinki"),"Finland"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "berlin"),"Germany"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "toulouse"),"France"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "schulich"),"Canada"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "goethe"),"Germany"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "belgrade"),"Russia"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "wayne"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "barcelona"),"Spain"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cologne"),"Germany"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "aucland"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "columbia"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "carnegie"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "penn state"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "stanford"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "cincinnati"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "louisiana"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "england"),"United Kingdom"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "buffalo"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "viet nam"),"Vietnam"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "pittsburgh"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "detroit"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "massachusetts"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "wisconsin"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "arizona"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "miami"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "wyoming"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "montana"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "dayton"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "san francisco"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "hofstra"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "colorado"),"United States"] = 1
countries[stringr::str_detect(stringr::str_to_lower(uni_data$Countries),pattern = "albany"),"United States"] = 1


countries_def = cbind(countries[,colSums(countries)>500],"other" = rowSums(countries[,colSums(countries)<=500])>1)


uni_data = cbind(uni_data,countries_def)
uni_data$Countries = NULL
# Remove []
uni_data$Addresses_sub <- gsub("\\[[^\\]]*\\]", "", uni_data$Addresses, perl=TRUE);
# Remove everything between ; and ,
uni_data$Addresses_sub <- gsub(",.*?;", ";", uni_data$Addresses_sub, perl = TRUE)
# Replace everything at and after , by ;
uni_data$Addresses_sub <- gsub(",.*$", ";", uni_data$Addresses_sub, perl = TRUE)
# Upper case
uni_data$Addresses_sub <- toupper(uni_data$Addresses_sub)

# Replace Address with Yes if uni name present
topuni <-function(top,s){
  temptopuni = top
  temptopuni[stringr::str_detect(temptopuni,s)] = "Yes"
  return(temptopuni)
}


# QS Uni Ranking Top 25 (incl variation)
qs_uni <- c( "Harvard University",
             "Standford University", "London School of Economics", "University of Oxford", "Cambridge University", "University of Pennsylvania",
             "University of California", "University of Chicago", "New York University", "London Business School", "Columbia University",
             "National University of Singapore", "Bocconi University",
             "Yale University", "University of California Los Angeles", "University of Manchester", "The University of Melbourne",
             "University of New South Wales", "HEC Paris", "University of Sydney", "Massachusetts Institute of Technology",
             'Hong Kong University of Science and Technology', 'Tsinghua', 'Peking', 'Nanyang',"CTR INT DEV HARVARD","University of Sydney",
             "Wharton","HARVARD UNIV", "Hong Kong University","Stern School of Business",
             "Harvard Business School","New York University", "MIT SLOAN", "MIT", "STANFORD UNIV", "STANFORD GRAD SCH BUSINESS", "STANFORD GSB",
            "LSE", "UNIV OXFORD LINCOLN COLL",  'OXFORD SAID BUSINESS SCH', "UNIV CAMBRIDGE", 'CAMBRIDGE ENDOWMENT RES FINANCE', 
            "UNIV PENN", "UNIV CALIF BERKELEY", "UNIV CHICAGO", "NYU", "NYU STERN", "LONDON BUSINESS SCH", "COLUMBIA BUSINESS SCH", 
            "COLUMBIA UNIV", "NATL UNIV SINGAPORE", "YALE UNIV", "YALE SCH MANAGEMENT", "UNIV NEW SOUTH WALES", "UNIV NEW S WALES", 
            "UNSW SYDNEY", "UNIV MELBOURNE", "MELBOURNE BUSINESS SCH", "BOCCONI UNIV", " SDA BOCCONI", "UNIV BOCCONI", "UCLA", 
            "UCLA ANDERSON", "UCLA ANDERSON SCH",  "HEC PARIS", "HEC SCH MANAGEMENT PARIS", "HONG KONG UNIV SCI & TECHNOL", 
            "HKUST", "UNIV MANCHESTER", "MANCHESTER SCH ACCOUNTING & FINANCE", "MANCHESTER BUSINESS SCH", "UNIV SYDNEY", "PEKING UNIV", 
            "TSINGHUA UNIV","NYU STERN SCHOOL OF BUSINESS IN NEW YORK.","NEW YORK UNIVERSITY","NYU'S STERN SCHOOL OF BUSINESS",
            "NYU STERN SCHOOL OF BUSINESS","HARVARD UNIVERSITY","STERN SCHOOL OF BUSINESS","MIT","SLOAN SCHOOL","STANFORD UNIVERSITY",
            "LONDON SCHOOL OF ECONOMICS","OXFORD UNIVERSITY","UNIVERSITY OF OXFORD","UNIVERSITY OF CAMBRIDGE",
            "UNIVERSITY OF PENNSYLVANIA","Univ. of California","UNIV. OF CALIFORNIA AT BERKELEY","UC BERKELEY-HAAS","BERKELEY HAAS SCHOOL OF BUSINESS",
            "HAAS SCHOOL OF BUSINESS","UNIVERSITY OF CHICAGO","Univ Oxford","Oxford University","BOOTH SCHOOL OF BUSINESS","LONDON BUSINESS SCHOOL","YALE SCHOOL OF MANAGEMENT",
            "COLUMBIA BUSINESS SCHOOL","COLUMBIA UNIVERSITY","NATIONAL UNIVERSITY OF SINGAPORE","YALE SCHOOL OF MANAGEMENT","YALE UNIVERSITY",
            "UNIVERSITY OF NEW SOUTH WALES","UNIVERSITY OF MELBOURNE","BOCCONI UNIVERSITY","BOCCONI SCHOOL OF MANAGEMENT",
            "UNIVERSITY OF CALIFORNIA/LOS ANGELES","UNIVERSITY OF CALIFORNIA LOS ANGELES","ANDERSON SCHOOL OF MANAGEMENT","HEC MANAGEMENT SCHOOL",
            "HEC SCHOOL OF MANAGEMENT","ECOLE DES HEC","HONG KONG UNIVERSITY OF SCIENCE AND TECHNOLOGY","MANCHESTER SCHOOL OF MANAGEMENT",
            "MANCHESTER BUSINESS SCHOOL","Univ Calif Los Angeles","UNIVERSITY OF MANCHESTER","UNIVERSITY OF SYDNEY","PEKING UNIVERSITY",
            "TSINGHUA UNIVERSITY")


uni_data$Addresses_sub[uni_data$origin == "MANUAL"] = uni_data$Addresses[uni_data$origin == "MANUAL"]
uni_data$Addresses_sub[uni_data$origin == "scopus"] = uni_data$Addresses[uni_data$origin == "scopus"]
uni_data$qsTop25 <- stringr::str_to_lower(uni_data$Addresses_sub)
for(s in  stringr::str_to_lower(qs_uni)){
  uni_data$qsTop25 <- topuni(uni_data$qsTop25, s)

  # print(length(s[grep("Yes",uni_data$qsTop25)]))
}


uni_data$qsTop25[uni_data$qsTop25 != "Yes"] <- "No"
uni_data$qsTop25[is.na(uni_data$qsTop25)] <- "No"


Full.name.missing.adresses = uni_data$Authors[which(is.na(uni_data$Addresses))]
FN_qs25 = FALSE
for(i in 1:length(Full.name.missing.adresses)){
  names = stringr::str_split(Full.name.missing.adresses[i],";")[[1]]
  isit = FALSE
  for(k in 1:length(names)){
    match.qs = uni_data$qsTop25[which(!is.na(uni_data$Addresses))][which(stringr::str_detect(uni_data$Authors[which(!is.na(uni_data$Addresses))],pattern = stringr::str_trim(names[k])))] == "Yes"
    if(length(match.qs)!=0){
      isit = isit | all(uni_data$qsTop25[which(!is.na(uni_data$Addresses))][which(stringr::str_detect(uni_data$Authors[which(!is.na(uni_data$Addresses))],pattern = stringr::str_trim(names[k])))] == "Yes")
    }
  }
  
  FN_qs25[i] = isit
}
uni_data$qsTop25[which(is.na(uni_data$Addresses))][FN_qs25] = "Yes"


# Convert to lowercase

uni_data$Abstract = iconv(uni_data$Abstract, "UTF-8", "UTF-8",sub='')
uni_data$Article.Title = iconv(uni_data$Article.Title, "UTF-8", "UTF-8",sub='')
# Title + Abstract
uni_data$b_abstract <- paste(uni_data$Article.Title, ". ", uni_data$Abstract, sep = "" )
# Remove numbers and punctuations
uni_data$b_abstract <- gsub("[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*"," ",uni_data$b_abstract)
# Remove 1 letter words
uni_data$b_abstract <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", uni_data$b_abstract)
# Remove extra spaces
uni_data$b_abstract <- trimws(gsub("\\s+", " ", uni_data$b_abstract))

uni_data$Addresses[uni_data$Addresses == ";;;"] = ""
uni_data$Addresses_sub = NULL

stop("break here")

write.csv(uni_data,"data/final_data.csv",row.names = FALSE)


## ADDITIONAL ANALYSES DAVE

final_data <- read.csv(file = "data/final_data.csv")

n <- length(final_data$Year)
n

pos <- c()
for (i in 1:n) {
  cat(i, "\n")
  tmp_abstract <- final_data$Abstract[i]
  test1 <- any(grep("\\[", tmp_abstract) & grep("\\]", tmp_abstract))
  test2 <- any(grep("\\(", tmp_abstract) & grep("\\)", tmp_abstract))
  if (test1 & test2) {
    pos <- c(pos, i)
  }
}
length(pos)
final_data$Abstract[pos[1:5]]

listsource <- unique(final_data$Journal)

tbl <- c()
for (s in listsource) {
  pos    <- final_data$Journal == s
  abbr_  <- final_data$abbrev[which(pos == 1)[1]]
  y_min  <- min(final_data$Year[pos])
  y_max  <- max(final_data$Year[pos])
  n_     <- sum(pos) 
  a_     <- round(mean(table(final_data$Year[pos])))
  tbl    <- rbind(tbl, cbind(abbr_, n_, a_, y_min, y_max))
}
rownames(tbl) <- listsource
tbl
xtable::xtable(tbl)

tmp <- as.numeric(tbl[,3])
min(tmp); max(tmp); median(tmp); sort(tmp)[.5*length(tmp)]; mean(tmp); sum(tmp)

tmp <- as.numeric(tbl[,4])
min(tmp); max(tmp); median(tmp); sort(tmp)[.5*length(tmp)]; mean(tmp); sum(tmp)

tbl <- c()
pos <- c()
for (s in listsource) {
  pos_s <- which(final_data$Journal == s)
  n_s   <- ceiling(0.005 * length(pos_s))
  tmp   <- stringr::str_count(final_data$b_abstract[pos_s], pattern = "\\W+") + 1
  pos   <- c(pos, pos_s[order(tmp, decreasing = TRUE)][1:n_s])
  pos   <- c(pos, pos_s[order(tmp, decreasing = FALSE)][1:n_s])
  tbl   <- rbind(tbl, c(n_s * 2, round(mean(tmp)), round(quantile(tmp, c(0, 0.005, 0.5, 0.995, 1)))))
}
rownames(tbl) <- listsource
tbl
pos
xtable::xtable(tbl)

sort(unlist(lapply(final_data$Abstract[pos], nchar)))

final_data_filt <- final_data[-pos[c(-51,-52)],]

barplot(t(tbl), horiz = TRUE, las = 1, cex.names = .5)

write.csv(final_data_filt, "data/final_data_filt.csv", row.names = FALSE)

