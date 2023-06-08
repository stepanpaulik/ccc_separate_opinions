xfun::pkg_attach2("tidyverse", "ggplot2", "progress", "foreach", "jsonlite",  "word2vec")

#Load data
source("supporting_functions.R")
# Load data
US_metadata = readRDS("../data/US_metadata.rds")
US_texts = readRDS("../data/US_texts.rds")
doc2vec_df = readRDS("../data/doc2vec_df.rds")

# Create sample for manual tagging
# sample = US_metadata %>% 
#   filter(rapportools::is.empty(dissenting_opinion)) %>% 
#   sample_n(size = 50)
# sample = US_metadata %>% 
#   filter(!rapportools::is.empty(dissenting_opinion)) %>% 
#   sample_n(size = 50) %>% 
#   rbind(., sample) %>% 
#   select(doc_id) %>% 
#   left_join(., US_texts)
# write_csv(sample, file = "../data/US_sample_annotate.csv")
# 
# sample2 = US_metadata %>% 
#   filter(rapportools::is.empty(dissenting_opinion)) %>% 
#   sample_n(size = 70)
# sample2 = US_metadata %>% 
#   filter(!rapportools::is.empty(dissenting_opinion)) %>% 
#   sample_n(size = 30) %>% 
#   rbind(., sample2) %>% 
#   select(doc_id) %>% 
#   left_join(., US_texts)
# write_csv(sample2, file = "../data/US_sample_annotate2.csv")

# Word2vec
# Window parameter:  for skip-gram usually around 10, for cbow around 5
# Sample: sub-sampling of frequent words: can improve both accuracy and 
# speed for large data sets (useful values are in range 0.001 to 0.00001)
# # hs: the training algorithm: hierarchical so􏰂max (better for infrequent
# words) vs nega􏰁ve sampling (better for frequent words, better with low
#                              dimensional vectors)
word2vec_model_CBOW = word2vec(x = US_texts$text, dim = 300)
write.word2vec(word2vec_model_CBOW, file = "models/word2vec_model_CBOW.bin")


word2vec_model_skipgram = word2vec(x = US_texts$text, dim = 300, type = "skip-gram", window = 10)
write.word2vec(word2vec_model_skipgram, file = "models/word2vec_model_skipgram.bin")

# Load annotated data and create it into tibble of tag-level observations, including creating paragraph IDs within group
judgments_annotated1 = jsonlite::fromJSON(txt = "../data/us_partitioning_sample1.json")
judgments_annotated2 = jsonlite::fromJSON(txt = "../data/us_partitioning_sample2.json")
judgments_annotated1 = judgments_annotated1$examples %>% as.data.frame()
judgments_annotated2 = judgments_annotated2$examples %>% as.data.frame()
judgments_annotated = bind_rows(judgments_annotated1, judgments_annotated2)

judgments_annotated_parts = foreach(i = seq(judgments_annotated$annotations), .combine = "rbind") %:%
  foreach (j = seq(judgments_annotated$annotations[[i]]), .combine = "rbind") %do% {
    text_length = str_length(judgments_annotated$content[[i]])
    output = tibble(
      "doc_id" = judgments_annotated$metadata$doc_id[i],
      "value" = judgments_annotated$annotations[[i]][j,4],
      "tag" = judgments_annotated$annotations[[i]][j,2],
      "start" = judgments_annotated$annotations[[i]][j,3]/text_length,
      "end" = judgments_annotated$annotations[[i]][j,1]/text_length,
      "length" = str_length(judgments_annotated$annotations[[i]][j,4])/text_length
    )
    return(output)
  } %>% drop_na()

judgments_annotated_paragraphs = foreach(i = seq(nrow(judgments_annotated_parts)), .combine = "rbind") %do% {
  # Create the temporary vector of paragraph texts
  judgments_annotated_paragraphs_temp = judgments_annotated_parts$value[i] %>% str_split(pattern = "\n\n")
  judgments_annotated_paragraphs_temp = judgments_annotated_paragraphs_temp[[1]] %>% as.vector()
  judgments_annotated_paragraphs_temp = judgments_annotated_paragraphs_temp[! judgments_annotated_paragraphs_temp %in% c("")]
  
  #Save the whole judgment text
  text_temp = judgments_annotated$content[judgments_annotated$metadata$doc_id == judgments_annotated_parts$doc_id[i]]
  
  # Split the original parts tibble into paragraph level observations
  output = foreach(j = seq(judgments_annotated_paragraphs_temp), .combine = "rbind") %do% {
    location_temp = str_locate(string = text_temp, pattern = fixed(judgments_annotated_paragraphs_temp[j])) %>% as.list()
    text_length = str_length(text_temp)
    output = tibble(
      "doc_id" = judgments_annotated_parts$doc_id[i],
      "value" = judgments_annotated_paragraphs_temp[j],
      "tag" = judgments_annotated_parts$tag[i],
      "start" = as.numeric(location_temp[1])/text_length,
      "end" = as.numeric(location_temp[2])/text_length,
      "length" = str_length(judgments_annotated_paragraphs_temp[j])/text_length
    )
    return(output)
  }
} 

# %>% 
#   group_by(doc_id) %>% 
#   mutate(paragraph_id = as.integer(gl(n(), 1, n()))) %>%
#   ungroup()

# Creating doc2vec model for the final preparation

# Load the word2vec model for doc2vec use
word2vec_model = read.word2vec(file = "models/word2vec_model_skipgram.bin")


judgments_annotated_paragraphs$doc_id = judgments_annotated_paragraphs$doc_id %>% make.unique()

# Create doc2vec model
doc2vec_df = judgments_annotated_paragraphs %>% 
  select(doc_id, value) %>% 
  rename(text = value) %>% 
  doc2vec(word2vec_model, newdata = ., type = "embedding") %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "doc_id") %>%
  drop_na()

# Create the final tibble for 
doc2vec_df = judgments_annotated_paragraphs %>% 
  select(-value) %>% 
  left_join(doc2vec_df, .)

# Cosmetic edits at the end
doc2vec_df$doc_id = modify(doc2vec_df$doc_id, .f = str_remove, pattern = "\\.\\d+")
doc2vec_df = doc2vec_df %>% modify(.f = unlist) %>% as_tibble()

# Save the file
saveRDS(doc2vec_df, file = "../data/doc2vec_df.rds")


# Create a binary variable for the presence of dissent in the decision, and include only the information on dissents
doc2vec_df = US_metadata %>% 
  select(doc_id, dissenting_opinion) %>% 
  left_join(doc2vec_df, ., by = "doc_id") %>% 
  mutate(dissenting_opinion = if_else(dissenting_opinion == "", 0, 1)) %>%
  filter(dissenting_opinion == 1) %>%
  mutate(tag = case_when(
    tag != "dissent" ~ "not_dissent",
    .default = "dissent"
  )) %>%
  select(-dissenting_opinion) %>% 
  mutate(tag = factor(tag))

# Filter paragraphs without a dissent
doc2vec_df = US_metadata %>% 
  select(doc_id, dissenting_opinion) %>% 
  left_join(doc2vec_df, ., by = "doc_id") %>% 
  mutate(dissenting_opinion = case_when(dissenting_opinion != "" ~ 1,
                                        .default = 0)) %>%
  filter(dissenting_opinion == 0) %>%
  select(-dissenting_opinion) %>% 
  mutate(tag = factor(tag))
