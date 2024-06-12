library(data.table)
library(dplyr)
library(fastDummies)

d_comments <- fread("00_data/youtube_dataset.csv")

unique(d_comments$`Channel Name`[!grepl("VEVO", d_comments$`Channel Name`)])

d_names <- d_comments[, .(num = .N), by = `User Name`]
setorderv(d_names, "num", order = -1)
d_names[num > 20,]
v_reps <- d_names$`User Name`[d_names$num > 20]

sample(v_reps, 35, replace = TRUE)

d_comments$Responders <- rep(sample(v_reps, 35, replace = TRUE), length.out = nrow(d_comments))

d_comments_wide <- dcast(d_comments, `Video Name` + `Channel Name` + `Comment Id` + Comment + Date + Likes ~ Responders, value.var = "User Name", fun.aggregate = length)

fwrite(d_comments, "00_data/d_comments.csv")
fwrite(d_comments_wide, "00_data/d_comments_wide.csv")
