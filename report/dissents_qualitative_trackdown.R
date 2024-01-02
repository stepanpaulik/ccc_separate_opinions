library(googledrive)
library(trackdown)

googledrive::drive_auth(email = "stepanpaulik@gmail.com")
googledrive::drive_empty_trash()

# UPLOAD RMARKDOWN
# trackdown::upload_file(file = "report/dissents_qualitative_interviews.Rmd",
#                        gpath = "dissents_articles/qualitative_interviews_article",
#                        hide_code = TRUE)

trackdown::update_file(file = "report/dissents_qualitative_interviews.Rmd",
                       gpath = "dissents_articles/qualitative_interviews_article", 
                       hide_code = TRUE)

# googledrive::drive_put(media = "report/dissents_qualitative_interviews.pdf",
#                        path = "dissents_articles/qualitative_interviews_article/dissents_qualitative_interviews.pdf")

googledrive::drive_update(media = "report/dissents_qualitative_interviews.pdf", 
                          file = "dissents_articles/qualitative_interviews_article/dissents_qualitative_interviews.pdf")

# Download the file
trackdown::download_file(file = "report/dissents_qualitative_interviews.Rmd",
                         gpath = "dissents_articles/qualitative_interviews_article",
                         rm_gcomments = TRUE)
