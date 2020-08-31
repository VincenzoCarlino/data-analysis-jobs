library(readr)
library("dplyr")
library("tidyr")
library(ggplot2)
library(stringr)
library(plyr)
library(tidyverse)
library(tidytext)
theme_set(theme_classic())

# Importazione del csv
jobs_listings <- read.csv("dataset/glassdoor.csv")

#Prima domanda: Quali sono le 3 città col maggior numero di annunci?
jobs_listings_top_four_cities <- jobs_listings %>% 
  group_by(header.location) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  slice(1:3)

theme_set(theme_classic())
ggplot(jobs_listings_top_four_cities, aes(x=header.location, y=count)) + 
  geom_bar(stat = "identity") +
  labs(title = "Le 3 migliori città",y="Numero di annunci", x= "Città") + 
  coord_flip()

#Seconda domanda: Qual'è la città col maggior numero di annunci per data analyst
jobs_for_data_science <- jobs_listings %>%
  filter(str_detect(header.normalizedJobTitle, "data analyst"))

jobs_listings_top_four_cities_for_data_science  <- jobs_for_data_science %>%
  group_by(header.location) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  slice(1:3)

ggplot(jobs_listings_top_four_cities_for_data_science, aes(x=header.location, y=count)) + 
  geom_bar(stat = "identity") +
  labs(title = "Le 3 migliori città con opportunità come data analyst",
       y="Numero di annunci", x= "Città") + 
  coord_flip()

#Terza domanda: Qual'è l'azienda col maggior numero di annunci al mondo?
most_employer_jobs <- jobs_listings %>% 
  filter(!is.na(map.employerName) & !is.null(map.employerName) & map.employerName != "") %>%
  group_by(map.employerName) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  slice(1:3)

ggplot(most_employer_jobs, aes(x=map.employerName, y=count)) + 
  geom_bar(stat = "identity") +
  geom_bar(stat = "identity") +
  labs(title = "Le 3 aziende con più opportunità lavorative",
       y="Numero di annunci", x= "Azienda") + 
  coord_flip()

#Quarta domanda: nell'azienda con più opportunità al mondo, qual'è la figura più richiesta
companies_with_most_opportunities_for_da <- jobs_listings %>%
  filter(!is.na(map.employerName) & !is.null(map.employerName) & map.employerName != "" & str_detect(header.normalizedJobTitle, "data analyst")) %>%
  group_by(map.employerName) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  slice(1:3)

ggplot(companies_with_most_opportunities_for_da, aes(x=map.employerName, y=count)) + 
  geom_bar(stat = "identity") +
  labs(title = "Le 3 aziende con più annunci di lavoro per data analyst",
       y="Numero di annunci", x= "Città") + 
  coord_flip()

#Quinta domanda: Tra il 1900 ad oggi, qual'è la decade con più aziende fondate?E nella decade con più imprese nate, quali erano i settori di punta?
years <- c(1899, 1909, 1919, 1929, 1939, 1949, 1959, 1969, 1979, 1989, 1999, 2009, 2019)

manipulate_data <- function(frame, n)
{
  companies_foundation_year <- jobs_listings %>%
    filter(overview.foundedYear > years[n-1] & overview.foundedYear < (years[n] + 1)) %>%
    group_by(overview.foundedYear) %>%
    summarise(count = n())
  total <- sum(companies_foundation_year$count)
  label <- paste((years[n-1]+1), (years[n]+1), sep = " - ")
  frame <- rbind(frame, data.frame(decade = label, count = total))
  if (length(years) > n){
    return(manipulate_data(frame, n+1))
  }
  return(frame)
}

result <- manipulate_data(data.frame(decade = c(), count = c()), 2)

pct <- round(result$count/sum(result$count)*100)
lbls <- paste(result$decade, pct, sep = ", ")
lbls <- paste(lbls,"%",sep="")
pie(result$count,labels = lbls, col=rainbow(length(lbls)),
    main="Aziende fondate dal 1900 ad oggi")

companies_2000_2010 <- jobs_listings %>%
  filter(overview.foundedYear > 1999 & overview.foundedYear <= 2010 & overview.sector != "") %>%
  select(overview.sector, overview.foundedYear)

plot(companies_2000_2010$overview.sector, companies_2000_2010$overview.foundedYear)
abline(lm(companies_2000_2010$overview.sector ~ companies_2000_2010$overview.foundedYear))

ggplot(companies_2000_2010, aes(x = overview.foundedYear, fill = overview.sector)) +            
  geom_histogram(position = "identity", alpha = 1, bins = 25)

g <- ggplot(companies_2000_2010, aes(x = as.character(overview.foundedYear), fill = overview.sector))
g + geom_bar(aes(fill=overview.sector), width = 0.5) + 
  labs(title="Settori di imprese fondate negli anni 2000-2010", 
       x="Anno di fondazione",
       fill="# Settori",
       y="Numero")

jobs_for_it_companies <- jobs_listings %>%
  filter(overview.foundedYear > 1999 & overview.foundedYear <= 2010 & overview.sector == "Information Technology") %>%
  group_by(header.location) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:20)

ggplot(jobs_for_it_companies, aes(x=header.location, y=count)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title="Le 20 città con più aziende nel settore dell' IT", 
       x="Città",
       y="Numero")

#Sesta domanda: In base alla tipologia di azienda, qual'è il numero di dipendenti?
type_of_company_for_sector <- jobs_listings %>%
  filter(overview.size != "" & overview.size != "Unknown" & overview.type != "" & overview.type != "") %>%
  select(overview.size, overview.type) 

g <- ggplot(type_of_company_for_sector, aes(x = overview.type, fill = overview.size))
g + geom_bar(aes(fill=overview.size), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title="Tipologie e numero dipendenti per azienda", 
       x="Tipologia di azienda",
       fill="# Numero di dipendenti",
       y="Numero")

#Settima domanda: numero dipendenti per aziende che ricercando figure di data analyst
employers_for_company <- jobs_listings %>%
  filter(overview.size != "" & overview.size != "Unknown" & str_detect(header.normalizedJobTitle, "data analyst")) %>%
  group_by(overview.size) %>%
  summarise(count = n())

total <- sum(employers_for_company$count)
total

pct <- round(employers_for_company$count/total*100)
lbls <- paste(employers_for_company$overview.size, pct, sep = ", ") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(employers_for_company$count,labels = lbls, col=rainbow(length(lbls)),
    main="Dimensioni delle aziende con annunci per data analyst")

# Ora prendiamo in considerazione una nazione singola

#Ottava domanda: qual'è l'andamento dei settori di impresa in Inghilterra
jobs_uk <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.sector != "") %>%
  group_by(overview.sector) %>%
  summarise(count = n())

jobs_uk$count_normalize <- round((jobs_uk$count - avg)/sd(jobs_uk$count), 2)

ggplot(jobs_uk, aes(x=`overview.sector`, y=count_normalize, label=count_normalize)) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `overview.sector`, 
                   yend = count_normalize, 
                   xend = `overview.sector`), 
               color = "black") +
  geom_text(color="white", size=3) +
  labs(title="Media dei settori di impresa in Inghiltgerra", y = "Media normalizzata", x = "Settori") + 
  ylim(-7.5, 7.5) +
  coord_flip()
  
#Nona domanda: parliamo di soldi, in inghilterra qual'è il settore di impresa che offre i migliori salari?
jobs_uk_salaries <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.sector != "" & salary.currency.name == "United Kingdom Pound") %>%
  select(overview.sector, salary.salaries)
jobs_uk_avg_salaries <- ddply(jobs_uk_salaries, .(overview.sector), summarize,  avg=mean(salary.salaries))

ggplot(jobs_uk_avg_salaries, aes(x=overview.sector, y=avg)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=overview.sector, 
                   xend=overview.sector, 
                   y=min(avg), 
                   yend=max(avg)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Media dei salari per settore", y = "Media salari", x= "Settori") +  
  coord_flip()

jobs_uk_salaries <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.foundedYear > 1900 & salary.currency.name == "United Kingdom Pound") %>%
  select(overview.foundedYear, salary.salaries)

plot(jobs_uk_salaries$overview.foundedYear, jobs_uk_salaries$salary.salaries)
abline(lm(jobs_uk_salaries$salary.salaries ~ jobs_uk_salaries$overview.foundedYear))

# Salari per linguaggio di programmazione, in UK
jobs_uk_programming_language <- jobs_listings %>%
  filter((map.country == "United Kingdom"
          | map.country == "Uk"
          | map.country == "England")
         & overview.sector != ""
         & salary.currency.name == "United Kingdom Pound") %>%
  select(salary.salaries, job.description) %>%
  unnest_tokens(word, job.description) %>%
  filter(word == "java" | word == "swift" | word == "c" | word == "python" |
           word == "c++" | word == "javascript" | 
           word == "sql" | word == "typescript") %>%
  group_by(word)

jobs_uk_programming_language <- ddply(jobs_uk_programming_language,
                                      .(word), 
                                      summarize,  
                                      avg=mean(salary.salaries))

theme_set(theme_bw())

ggplot(jobs_uk_programming_language, aes(x=word, y=avg)) + 
  geom_bar(stat="identity", width=.2, fill="tomato3") + 
  labs(title="Linguaggi di programmazione e media salario", 
       x = "Linguaggio", 
       y="Salario") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

# Decima domanda: Anno di fondazione delle aziende?
companies_uk_foundation_year <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.foundedYear > 0) %>%
  group_by(overview.foundedYear) %>%
  summarise(count = n())

ggplot(companies_uk_foundation_year, aes(x=overview.foundedYear)) + 
  geom_line(aes(y=count)) + 
  labs(title="Anno di fondazione di un azienda", 
       x="Anno", 
       y="Numero")

# Più nel dettaglio, dal 1900 ad oggi
companies_uk_foundation_year_over_1900 <- companies_uk_foundation_year %>%
  filter(overview.foundedYear >= 1900)

ggplot(companies_uk_foundation_year_over_1900, aes(x=overview.foundedYear)) + 
  geom_line(aes(y=count)) + 
  labs(title="Anno di fondazione di un azienda", 
       x="Anno", 
       y="Numero")

#Anno in cui sono state fondate più aziende
max_foundationyear <- companies_uk_foundation_year %>%
  slice(which.max(count))

View(max_foundationyear)

#Nel 2006, ovvero l'anno con più aziende fondate, qual'è stato il settore più in crescita
sectors_companies_founded_2006 <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.sector != "" & overview.foundedYear == 2006) %>%
  group_by(overview.sector) %>%
  summarise(count = n())


pct <- round(sectors_companies_founded_2006$count/sum(sectors_companies_founded_2006$count)*100)
lbls <- paste(sectors_companies_founded_2006$overview.sector, pct, sep = ", ") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(sectors_companies_founded_2006$count,labels = lbls, col=rainbow(length(lbls)),
    main="Settori delle aziende fondate nel 2006")


salary_recent_companies <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.foundedYear >= 2000 & salary.salaries > 0)

View(mean(salary_recent_companies$salary.salaries))


salary_old_companies <- jobs_listings %>%
  filter((map.country == "United Kingdom" | map.country == "Uk" | map.country == "England") & overview.foundedYear < 2000 & salary.salaries > 0)

View(mean(salary_old_companies$salary.salaries))

