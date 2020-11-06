# suppressPackageStartupMessages(library(dplyr))

library(gh)
library(here)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(rio)

#Retrieve all of RLadies's public GitHub repositories.
repos <- gh("/users/rladies/repos", .limit = Inf)
length(repos)



# Retrieve all the Chapteres presentation repos
chapters_repo <- 
  tibble(
    repo = repos %>% map_chr("name"),
    url = repos %>% map_chr("html_url"),
  ) %>%
  filter(str_detect(url,"presentation"))

#We used csv2 because we have some encode issues (windows and/or non english characters)
readr::write_excel_csv2(chapters_repo, path = here::here("list_rladies_chapters_repos.csv"))

# Get all the files an folders for each repo 
# rate is used to avoid API rate limits in GitHub API
rate <- rate_delay(0.5)

# Iterate in each chapter repo and get it contents (files and folders)
content <- chapters_repo %>%
  pull(repo) %>%
  map(slowly(~ gh(repo = .x, endpoint = "/repos/rladies/:repo/contents", 
                  .token ="",
                  .limit = Inf), rate = rate))

# We get a list with another list for each contents in the repo 
# Iterate in the list to get the name, url and type of content
files <- tibble(
  name = content %>% flatten() %>% map_chr("name"),
  url= content %>% flatten() %>% map_chr("html_url"),
  type = content %>% flatten() %>% map_chr("type")
  
)

#We used csv2 because we have some encode issues (windows and/or non english characters)  
readr::write_excel_csv2(files, path = here::here("files_in_rladies_chapters_repos.csv"))


# Clean the files retrieved from the chapter's repo

lessons <- files %>%
  # Trying to get a date, at least the year
  mutate(date = gsub("[^0-9]", "", name),
         date_2 = as_date(date),
         date_3 = if_else(is.na(date_2),
                          case_when(
                            #Only the year
                            str_length(date) == 4 ~ date,
                            # The year and the month, we keep only the year
                            str_length(date) == 6 ~ str_sub(date,1,4),
                            # a date with the format Month, day and Year
                            str_length(date) == 8 ~ as.character(year(mdy(date))),
                            # this has the full date with another number before or after that date 
                            # TODO: find a way to don't loss some date with dmy fortmat 
                            str_length(date) == 9 ~ if_else(str_sub(date,1,4) > 2010, as.character(year(as_date(str_sub(date,1,8)))), as.character(year(as_date(str_sub(date,2,9))))),
                            # this has the full date with another number before or after that date 
                            # this could be as the one in the top, replace the 9 and the 10 for lenght
                            # and the start for ???
                            str_length(date) == 10 ~ if_else(str_sub(date,1,4) > 2010, str_sub(date,1,4), as.character(year(as_date(str_sub(date,3,10))))),
                            # this has the full date with another number before or after that date 
                            str_length(date) == 11 ~ if_else(str_sub(date,1,4) > 2010, str_sub(date,1,4), as.character(year(as_date(str_sub(date,4,11))))),
                            # this has a full date and some other numbers after that date
                            str_length(date) == 12 ~ str_sub(date,1,4),
                            # numbers that aren't years
                          ), 
                          # This are the date that lubridate convert right in date_2
                          as.character(year(date_2))                          
                          ),
         date_3 = if_else(date_3 < 2010, "", date_3),
         #Get the name of the repo, for future join with data from chapter
         repo = str_sub(url, 28), 
         repo = str_sub(repo, 1, str_locate(repo, "/")[,2]-1),
         #Get the city from the repo name
         city = str_extract(repo, "_(.*)"),
         city = str_remove(city, "_"),
         city = str_to_title(city)) %>%
  # Filter some content in order to get only educational material
  filter(!(name %in% c("README.md",".gitignore", ".gitmodules", "README.html", ".github", "_config.yml", "LICENSE"))) %>%
  filter(city != 'Global_presentations') %>%
  # keep only the columns needed
  select(name, url, type, date_3, repo, city) 

# Arrange the name of the resources
lessons <- lessons %>%
  mutate(name = str_replace_all(name, "[:digit:]", ""),
         name = str_replace_all(name, "^-", ""),
         name = str_replace_all(name, "-", " "),
         name = str_replace_all(name, "_", " "),
         name = str_replace_all(name, "_", " "),
         name = str_remove(name, "[.](md|Rmd|pdf|pptx|R|Rproj|jpg|csv|txt|xls|PNG|png|docx)"),
         name = trimws(name)) %>%
  filter(name != 'master')

# Add topic to the lesson
# During the campaing we build a clodword and with thatchoose the topics

lessons <- lessons %>%
  mutate(
    topic = ifelse(str_detect(name, "intro|Básico|beggin|Novice|scratch|Scratch|Intro"), "Intro", NA),
    topic = ifelse(str_detect(name, "datavi|visualizacion"), "Data Vizualization", topic),
    topic = ifelse(str_detect(name, "ggplot|Ggplot"), "ggplot", topic),
    topic = ifelse(str_detect(name, "shiny|Shiny"), "shiny", topic),
    topic = ifelse(str_detect(name, "tidy|Tidy"), "tidyverse", topic),
    topic = ifelse(str_detect(name, "rmarkdown|Rmarkdown|Markdown|markdown"), "rmarkdown", topic),
    topic = ifelse(str_detect(name, "git|Git"), "git", topic),
    topic = ifelse(str_detect(name, "dplyr"), "dplyr", topic),
    topic = ifelse(str_detect(name, "blog"), "blogdown", topic),
    topic = ifelse(str_detect(name, "scrap"), "Web scrapping", topic),
    topic = ifelse(str_detect(name, "map|spatial|espacial"), "spatial", topic),
    topic = ifelse(str_detect(name, "docker"), "docker", topic),
    topic = ifelse(str_detect(name, "mining"), "data mining", topic),
    topic = ifelse(str_detect(name, "Machine learning"), "Machine learning", topic),
    topic = ifelse(str_detect(name, "pack"), "Package", topic),
    topic = ifelse(str_detect(name, "pur"), "purrr", topic),
    topic = ifelse(str_detect(name, "model"), "Modelling", topic),
    topic = ifelse(str_detect(name, "RStudio|rstudio"), "RStudio", topic),
    topic = ifelse(str_detect(name, "SQL|sql|Sql"), "SQL", topic),
  )

# Add language

lessons <- lessons %>%
  mutate(language = case_when(
    city %in% c("Montevideo", "Cordoba", "Barcelona", "Buenosaires", "Madrid", "Mendoza", "Santarosa") ~ "Spanish",
    city %in% c("Belohorizonte") ~ "Portuguese" ,
    city %in% c("Bari") ~ "Italian" ,
    TRUE ~ "English"
  ))



