library(googledrive)
library(trackdown)

googledrive::drive_auth(email = "stepanpaulik@gmail.com")
googledrive::drive_empty_trash()

# UPLOAD RMARKDOWN
# trackdown::upload_file(file = "report/dissents_article.Rmd",
# #                        gpath = "dissents_articles/quantitative_article", 
#                        hide_code = TRUE)

trackdown::update_file(file = "report/dissents_article.Rmd",
                       gpath = "dissents_articles/quantitative_article", 
                       hide_code = TRUE)

# googledrive::drive_put(media = "report/dissents_article.pdf",
#                        path = "dissents_articles/quantitative_article/dissents_article.pdf")

googledrive::drive_update(media = "report/dissents_article.pdf", 
                          file = "dissents_articles/quantitative_article/dissents_article.pdf")

# Download the file
trackdown::download_file(file = "report/dissents_article.Rmd",
                         gpath = "dissents_articles/quantitative_article",
                         rm_gcomments = TRUE)
