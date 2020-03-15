# suppressPackageStartupMessages(library(dplyr))

library(gh)
library(here)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)

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
# Iterate in thi list to get the name, url and type of content
files <- tibble(
  name = content %>% flatten() %>% map_chr("name"),
  url= content %>% flatten() %>% map_chr("html_url"),
  type = content %>% flatten() %>% map_chr("type")
  
)

#We used csv2 because we have some encode issues (windows and/or non english characters)  
readr::write_excel_csv2(files, path = here::here("files_in_rladies_chapters_repos.csv"))


# Clean the files retrivied from the chapter's repo

lessons <- files %>%
  # Trying to get a date, at leat the year
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
                          # This arethe date that lubridate convert right in date_2
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

  


