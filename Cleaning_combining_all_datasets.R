#Load libraries
library(tidyverse)
library(dplyr)
library(tools)
library(stringr)

#Import csv
Dogs <- read_csv("Registrations_Master_SA.csv")
Crimes <- read_csv("Crimes.csv")
Dog_breeds <- read_csv("Breed_Ratings.csv")


#-----------------Cleaning of base set of registered dogs in Southern Australia------------------------------------------------

#Show all the different suburbs and regions in dataset
Dogs$Suburb <- parse_factor(Dogs$Suburb)
Dogs$Region <- parse_factor(Dogs$Region)
levels(Dogs$Suburb) 
levels(Dogs$Region)
    
#Make all suburb entries lowercase
Dogs$Suburb <- tolower(Dogs$Suburb)
    
#Make a new dataset with the columns we need
Dogs_clean <- select(Dogs, 
                        "Year", 
                        "Suburb", 
                        "Breed_Description")
    
#Calculate the amount of registered dogs per suburb, per dog breed
Breeds_per_suburb <-  Dogs_clean %>%
  group_by(Suburb, Year, Breed_Description) %>%
  tally()
    
#Calculate the amount of registered dogs per suburb, remove n.a. entries
Dogs_per_suburb <-  Dogs_clean %>%
  group_by(Suburb, Year) %>%
  tally() %>%
  na.omit(Dogs_clean) 
    
#Rename n column to dogs per suburb and breeds per suburb
colnames(Dogs_per_suburb) <- c("Suburb", "Year", "Total dogs")
colnames(Breeds_per_suburb) <- c("Suburb", "Year", "Dog_breed", "Total_dogs")

    
#-----------------Cleaning of base set of registered offences in Southern Australia------------------------------------------------

#Import csv
Crimes <- read_csv("Crimes.csv")

#Rename columns
Crimes <- rename(Crimes, "Year" = "Reported Date", "Suburb" = "Suburb - Incident", "Offences" = "Offence count", "Offence description" = "Offence Level 3 Description")

#Save as new csv
write_csv(Crimes, "Crimes_2.csv")

#Show all the different suburbs in dataset
Crimes$`Suburb` <- parse_factor(Crimes$`Suburb`)
levels(Crimes$`Suburb`)

#Make all suburbs lower case
Crimes$Suburb <- tolower(Crimes$Suburb)

#Convert date to date format and remove day and month
Crimes$Year <- format(as.Date(Crimes$Year, format = "%d/%m/%Y"), "%Y")

#Convert factor to string
Crimes$Suburb <- as.character(Crimes$Suburb)

#Make a new dataset with the columns we need, including offence description for possible later use
Crimes_clean <- select(Crimes, 
                       "Year", 
                       "Suburb", 
                       "Offences",
                       "Offence description")

#Calculate the amount of registered offences per suburb
Crimes_per_suburb <-  Crimes_clean %>%
  group_by(`Suburb`, `Year`) %>%
  summarise(`Offences per suburb` = sum(`Offences`))





#-----------------Combining the total amount of dogs with the total amount of registered offences, per suburb, per year---------

Dogs_vs_crime <-  merge(Dogs_per_suburb, Crimes_per_suburb)

  
#-----------------Save all datasets as CSV--------------------------------------------------------------------------------------

write_csv(Breeds_per_suburb, "Breeds_per_suburb.csv")

write_csv(Dogs_per_suburb, "Dogs_per_suburb.csv")

write_csv(Crimes_per_suburb, "Crimes_per_suburb.csv")

write_csv(Dogs_vs_crime, "Dogs_vs_crime.csv")
    
#-----------------Clean the registered dog breeds to combine them with the information about dog breed profiles------------------

#Make all dog breeds lowercase
Breeds_per_suburb$Dog_breed <- tolower(Breeds_per_suburb$Dog_breed)
Dog_breeds$Type_of_Breed <- tolower(Dog_breeds$Type_of_Breed)

#Remove cross breeds
Removed_cross_breeds <- Breeds_per_suburb[-grep(" x | x|cross", Breeds_per_suburb$Dog_breed), ]
All_cross_breeds <- Breeds_per_suburb[grep(" x | x|cross", Breeds_per_suburb$Dog_breed), ]

#Remove all unnecessary details from breeds, i.e. coat pattern
No_vague_breeds <- Removed_cross_breeds
No_vague_breeds$Dog_breed <- gsub("\\s*\\([^\\)]+\\)|*\\(\\)*|-.*|.\\)|standard |wire haired |wire hair ","", No_vague_breeds$Dog_breed)
No_vague_breeds$Dog_breed <- gsub("dachshund *","dachshund", No_vague_breeds$Dog_breed)
#Remove double spaces
No_vague_breeds$Dog_breed <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",  No_vague_breeds$Dog_breed, perl=TRUE)
#Remove NAS
No_vague_breeds <- na.omit(No_vague_breeds)

#Change dog breed to correct naming

  #Convert into factors
  No_vague_breeds$Dog_breed <- as.factor(No_vague_breeds$Dog_breed)
  No_vague_breeds$Suburb <- as.factor(No_vague_breeds$Suburb)
  
  #Show all types of dog breeds, both registered and the dog breeds we have information on
  Registered_dog_breeds <- levels(No_vague_breeds$Dog_breed)
  Available_dog_breeds <- Dog_breeds$Type_of_Breed
  Difference_breeds <- Registered_dog_breeds[!(Registered_dog_breeds %in% Available_dog_breeds)]

  #Find the most mistyped dogs in the dataset
  Missing_dogs <- No_vague_breeds[No_vague_breeds$Dog_breed %in% Difference_breeds,]
  Most_missing_dogs <- aggregate(Missing_dogs$Total_dogs, by = list('Dog_breed' = Missing_dogs$Dog_breed), sum)
  
  #Change the most commonly mistyped dogs which are significant for research, ignoring accidentally missed cross breeds
  No_vague_breeds$Dog_breed <- plyr::revalue(No_vague_breeds$Dog_breed, c("spoodle" = "cockapoo","moodle" = "maltipoo","blue heeler" = "australian cattle dog", "dalmation" = "dalmatian","maltese terrier" = "maltese","doberman pinscher" = "dobermann pinscher", "shar pei" = "chinese shar pei", "kelpie" = "australian kelpie","west highland terrier" = "west highland white terrier", "cavoodle" = "cavapoo", "schnauzer" = "standard schnauzer", "schnauzer miniature" = "miniature schnauzer", "labrador" = "labrador retriever", "german shepherd" = "german shepherd dog", "groodle" = "goldendoodle", "schnauser" = "schnauzer", "british bulldog" = "bulldog", "afghan" = "afghan hound", "retriever golden" = "golden retriever", "american bull terrier" = "american pit bull terrier", "american staff terrier" = "american staffordshire terrier", "american staffordshire bull terrier" = "american staffordshire terrier", "anatolian shephard dog" = "anatolian shepherd dog", "anatolian shepherd" = "anatolian shepherd dog", "anotolian shepherd" = "anatolian shepherd dog", "american bull dog" = "american bulldog", "belgian shepherd dog" = "belgian sheepdog", "belgian shepherd" = "belgian sheepdog", "doberman" = "doberman pinscher", "dobermann" = "dobermann pinscher"
  ))
  
  #Collapse so all the double entries with different total_dogs value due to cleaning are one row again
  No_vague_breeds <- aggregate(No_vague_breeds$Total_dogs, by = list('Year' = No_vague_breeds$Year, 'Suburb' = No_vague_breeds$Suburb, 'Dog_breed' = No_vague_breeds$Dog_breed), sum)

#Calculate average friendliness per breed
Friendliness_per_breed <- Dog_breeds %>%
  mutate(Average_friendliness = (Friendly_towards_strangers + Dog_Friendly + Affectionate_with_family + Potential_to_Mouthiness) / 4) %>%
  select(Type_of_Breed, Average_friendliness)

#Rename columns
colnames(Friendliness_per_breed) <- c("Dog_breed", "Average_friendliness_of_breed")
colnames(No_vague_breeds) <- c("Year", "Suburb", "Dog_breed", "Total_dogs")

#Combine average friendliness rating with dog breeds per suburb
Dog_friendliness_per_suburb <-  merge(No_vague_breeds, Friendliness_per_breed)

#Change order of columns
Dog_friendliness_per_suburb <- Dog_friendliness_per_suburb %>%
  select("Suburb", "Year", "Dog_breed", "Average_friendliness_of_breed", "Total_dogs")

#Write as csv
write_csv(Dog_friendliness_per_suburb, "Dog_friendliness_per_suburb.csv")

#Make a new dataset containing the total amount of dogs per suburb, using only the dog breeds we have information on
Total_dogs_per_suburb_cleaned <- aggregate(Dog_friendliness_per_suburb$Total_dogs, by = list('Year' = Dog_friendliness_per_suburb$Year, 'Suburb' = Dog_friendliness_per_suburb$Suburb), sum)
colnames(Total_dogs_per_suburb_cleaned) <- c("Year", "Suburb", "Total_dogs")

#Calculate the average friendliness score per suburb
  #Multiply the friendliness rating with the amount of dogs from this breed and add this to a new column "Friendliness_Total"
  Dog_score_per_suburb <- Dog_friendliness_per_suburb %>%
    mutate(Friendliness_Total = Average_friendliness_of_breed*Total_dogs)
  
  #Collapse so you get the total dog score per suburb per year
  Dog_score_per_suburb <- aggregate(Dog_score_per_suburb$Friendliness_Total, by = list('Year' = Dog_score_per_suburb$Year, 'Suburb' = Dog_score_per_suburb$Suburb), sum)
  
  #Merge with total dogs per year per suburb
  Dog_score <-  inner_join(Dog_score_per_suburb, Total_dogs_per_suburb_cleaned)
  
  #Change column names
  colnames(Dog_score) <- c("Year", "Suburb", "Breed_total_score", "Total_dogs_per_suburb")

#Calculate the total score of dog friendliness per neighbourhood
Dog_score <- Dog_score %>%
  transmute(Year, Suburb, Suburb_dog_friendliness_score = (Breed_total_score/Total_dogs_per_suburb))

#Round this score
Dog_score$Suburb_dog_friendliness_score <- round(Dog_score$Suburb_dog_friendliness_score, digits = 4)

#Change classes of columns in order for them to be merge-able
Crimes_per_suburb$Year<-as.double(Crimes_per_suburb$Year)
options(digits=4)
Dog_score$Suburb <- as.character(Dog_score$Suburb)


#-----------------Combine the average dog friendliness rating with the total amount of registered offences, per suburb per year------------

Breed_friendliness_crime_per_suburb <-  inner_join(Dog_score, Crimes_per_suburb)

#Write as csv
write_csv(Breed_friendliness_crime_per_suburb, "Breed_friendliness_crime_per_suburb.csv")





    






