library(data.table)
library(dplyr)
library(stringdist)


# Load data ####
## Using data from "Most Liked Comments on YouTube" with some minor cleaning
## https://www.kaggle.com/datasets/nipunarora8/most-liked-comments-on-youtube
d_comments <- fread("00_data/d_comments.csv")
d_comments_wide <- fread("00_data/d_comments_wide.csv")

# Quick practice ####
#--dplyr--
d_comments %>% 
  select(matches("m{2}"))

#--base R--
names(d_comments)[grepl("m{2}", names(d_comments))]

### Correct systematic typos and other errors
#--dplyr--
View(d_comments %>%
  mutate(comment_clean = gsub("\\d+\\sK", "1000K", Comment)) %>% 
    select(Comment, comment_clean) %>% 
    filter(grepl("1000K", comment_clean)))

#--data.table--
d_comments[, comment_clean := gsub("\\d+\\sK", "1000K", Comment)]

#--base R--
d_comments$comment_clean <- gsub("\\d+\\sK", "1000K", d_comments$Comment)
  
  
### Find all instances of a certain pattern
#--dplyr--
d_comments %>%
    filter(grepl("[[:alpha:]]$", Comment))
  
#--data.table--
d_comments[grepl("[[:alpha:]]$", Comment),]
  
#--base R--
d_comments[grepl("[[:alpha:]]$", d_comments$Comment)]
    
### Lookbehind example
d_comments[grep("(?<=\\d{1}\\s)K", `User Name`, perl = TRUE), .(`User Name`)]

# Select variable names by pattern ####
## Which comments were left by bots?
### Identify patterns
names(d_comments_wide)

### Create vector of desired column names
#cols <- names(d_comments_wide)[grepl("\\s", names(d_comments_wide), perl = TRUE)]
cols2 <- c(names(d_comments_wide)[1:6], 
           names(d_comments_wide)[grepl("(?=.*\\s)(?=.*\\d)", names(d_comments_wide), perl = TRUE)])

### Subset
d_comments_bots <- d_comments_wide[, ..cols2]

### Create vector of desired column names and subset
d_comments_bots <- d_comments_wide %>% 
  select(c(1:6), matches("(?=.*\\s)(?=.*\\d)", perl = TRUE))
# Note: contains() uses a fixed match

### Dplyr wins here

# Correct systematic errors ####
## Fix misspelled bot names (use stringdist Jaro matching)
### Identify patterns
unique(d_comments$`User Name`)
length(unique(d_comments$`User Name`))
unique(d_comments$`User Name`[grepl("\\d", d_comments$`User Name`)])
unique(d_comments$`User Name`[grepl("\\d.+K", d_comments$`User Name`)])

### Extra step! Similarity matching
d_comments[, name_match := stringsim(`User Name`, Responders, method = "jw", p = 0.2)]
## equivalent to mutate(name_match = stringsim(`User Name`, Responders, method = "jw", p = 0.2))

v_sim <- sort(unique(d_comments$name_match), decreasing = TRUE)[2:11]

d_comments[name_match %in% v_sim, .(`User Name`, Responders, Comment)]

### Correct errors
d_comments[, username_clean := ifelse(name_match %in% v_sim, sub("\\s\\d", " 1", `User Name`, perl = TRUE), `User Name`)]
d_comments[name_match %in% v_sim, .(`User Name`, username_clean, Responders)]

d_comments[, username_clean := ifelse(name_match %in% v_sim, gsub("Kindly", "", username_clean, perl = TRUE), username_clean)]
d_comments[name_match %in% v_sim, .(`User Name`, username_clean, Responders)]

# Filter by text pattern ####
## Find comments using non-English characters
### Identify patterns
unique(d_comments$`User Name`)
sample(unique(d_comments$Comment), 15)

### Pull only those comments with non-English characters
d_comments[grepl("(?=\\pL)(?![a-zA-Z])", Comment, perl = TRUE),]


### Only match comments with characters that exist in English
nrow(d_comments[grepl("[:alpha:]", Comment),])
### Match comments with characters in any written language
nrow(d_comments[grepl("\\p{L}", Comment, perl = TRUE),])
