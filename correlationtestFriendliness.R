library(tidyverse)
library(dplyr)
library(ggplot2)


Breed_friendliness_crime_per_suburb<-read_csv("
Breed_friendliness_crime_per_suburb.csv")

#put the x and y if you want to do more visualisations
ggplot(Breed_friendliness_crime_per_suburb1, aes(x = Suburb_dog_friendliness_score, y = totalOffences))+geom_point()+geom_smooth(method="lm")+scale_x_log10()

#A scale determines how an attribute of the data is mapped into an aesthetic property of a geom (e.g., the geom's position along the x axis, or a geom's fill color in a color space)


cor(Breed_friendliness_crime_per_suburb1$Suburb_dog_friendliness_score, Breed_friendliness_crime_per_suburb1$totalOffences)


cor.test(Breed_friendliness_crime_per_suburb1$Suburb_dog_friendliness_score, Breed_friendliness_crime_per_suburb1$totalOffences)

round(cor(cbind(Breed_friendliness_crime_per_suburb1$Suburb_dog_friendliness_score,Breed_friendliness_crime_per_suburb1$totalOffences)),2)

ggplot(Breed_friendliness_crime_per_suburb1, aes(x =Suburb_dog_friendliness_score ))+geom_histogram()+geom_histogram(bins = 30)

ggplot(Breed_friendliness_crime_per_suburb1, aes(x = totalOffences))+geom_histogram()+geom_histogram(bins = 30)

Breed_friendliness_crime_per_suburb1%>%
  summarise(medianSuburb_dog_friendliness_score = median(Suburb_dog_friendliness_score), meanSuburb_dog_friendliness_score = mean(Suburb_dog_friendliness_score), modeSuburb_dog_friendliness_score = mode(Suburb_dog_friendliness_score), sdSuburb_dog_friendliness_score = sd(Suburb_dog_friendliness_score))

Breed_friendliness_crime_per_suburb1%>%
  summarise(mediantotalOffences = median(totalOffences), meantotalOffences = mean(totalOffences), modetotalOffences = mode(totalOffences), sdtotalOffences = sd(totalOffences))
