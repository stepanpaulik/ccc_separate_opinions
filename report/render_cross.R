getwd()

rmarkdown::render("report/separate_opinions.Rmd", output_file="separate_opinions.pdf",
                  bookdown::pdf_document2(template = stevetemplates::templ_article2(),
                                          keep_tex = FALSE,
                                          latex_engine = "xelatex",
                                          toc = TRUE, number_sections = TRUE))
