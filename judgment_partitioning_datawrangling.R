xfun::pkg_attach2("tidyverse", "ggplot2", "progress", "foreach", "jsonlite",  "word2vec")

#Load data
source("supporting_functions.R")
load("data/US_texts.RData")
load("data/US_metadata.RData")
load("data/US_texts_paragraphs.RData")

# Create sample for manual tagging
sample <- data_metadata %>% 
  filter(is.empty(dissenting_opinion)) %>% 
  slice_sample(n = 50)
sample_paragraphs <- data_metadata %>% 
  filter(!is.empty(dissenting_opinion)) %>% 
  slice_sample(n = 50) %>% 
  rbind(., sample) %>% 
  select(doc_id) %>% 
  left_join(., US_texts)
write_csv(sample_paragraphs, file = "data/US_sample_annotate.csv")

# DATA PREP
# Split the texts into paragraphs and index them
paragraphs_split <- function(US_texts) {
    # Create temporary object with doc_id + text split up into paragraphs
  judgments_annotations_paragraphs_temp <- US_texts %>% group_by(doc_id) %>% summarise(paragraphs = str_split(text, pattern = "\n\n"))
  
  # The meat of the function: nested foreach loop
  judgments_annotations_paragraphs <- foreach(i = seq(judgments_annotations_paragraphs_temp$doc_id), .combine='rbind') %:%
    foreach(j = 1:length(judgments_annotations_paragraphs_temp$paragraphs[[i]]), .combine = 'rbind') %do% {
      paragraph_temp <- judgments_annotations_paragraphs_temp$paragraphs[[i]][j] %>% str_trim(side = "both")
      location_temp <- str_locate(string = US_texts$text[US_texts$doc_id == judgments_annotations_paragraphs_temp$doc_id[i]], pattern = fixed(judgments_annotations_paragraphs_temp$paragraphs[[i]][j])) %>% as.list()
      text_length <- str_length(US_texts$text[US_texts$doc_id == judgments_annotations_paragraphs_temp$doc_id[i]])
      output <- list(
        "doc_id" = judgments_annotations_paragraphs_temp$doc_id[i], 
        "paragraph_id" = j, 
        "paragraph_text" = paragraph_temp, 
        "paragraph_start" = as.numeric(location_temp[1])/text_length, 
        "paragraph_end" = as.numeric(location_temp[2])/text_length, 
        "paragraph_length" = str_length(paragraph_temp)/text_length
      )
      return(output)
    } %>% as_tibble() %>% df_unlist()

  # Drop NA values
  judgments_annotations_paragraphs <- judgments_annotations_paragraphs  
  
  return(judgments_annotations_paragraphs)
}

# Run the function and save the file
judgments_annotations_paragraphs <- paragraphs_split(US_texts = US_texts)
save(judgments_annotations_paragraphs, file = "data/US_texts_paragraphs.RData")


# Word2vec
# Window parameter:  for skip-gram usually around 10, for cbow around 5
# Sample: sub-sampling of frequent words: can improve both accuracy and 
# speed for large data sets (useful values are in range 0.001 to 0.00001)
# # hs: the training algorithm: hierarchical so􏰂max (better for infrequent
# words) vs nega􏰁ve sampling (better for frequent words, better with low
#                              dimensional vectors)
word2vec_model_CBOW <- word2vec(x = US_texts$text, dim = 300)
write.word2vec(word2vec_model_CBOW, file = "models/word2vec_model_CBOW.bin")
read.word2vec(file = "models/word2vec_model_CBOW.bin")

word2vec_model_skipgram <- word2vec(x = US_texts$text, dim = 300, type = "skip-gram", window = 10)
save(word2vec_model_skipgram, file = "models/word2vec_model_skipgram.bin")

# Load annotated data and create it into tibble of tag-level observations, including creating paragraph IDs within group
judgments_annotated <- jsonlite::fromJSON(txt = "data/US_judgments_annotated.json")
judgments_annotations <- judgments_annotated$examples %>% as.data.frame()

judgments_annotations_parts <- foreach(i = seq(judgments_annotations$annotations), .combine = "rbind") %:%
  foreach (j = seq(judgments_annotations$annotations[[i]]), .combine = "rbind") %do% {
    text_length <- str_length(judgments_annotations$content[[i]])
    output <- tibble(
      "doc_id" = judgments_annotations$metadata$doc_id[i],
      "value" = judgments_annotations$annotations[[i]][j,4],
      "tag" = judgments_annotations$annotations[[i]][j,2],
      "start" = judgments_annotations$annotations[[i]][j,3]/text_length,
      "end" = judgments_annotations$annotations[[i]][j,1]/text_length,
      "length" = str_length(judgments_annotations$annotations[[i]][j,4])/text_length
    )
    return(output)
  } %>% drop_na()

judgments_annotations_paragraphs <- foreach(i = seq(nrow(judgments_annotations_parts)), .combine = "rbind") %do% {
  # Create the temporary vector of paragraph texts
  judgments_annotations_paragraphs_temp <- judgments_annotations_parts$value[i] %>% str_split(pattern = "\n\n")
  judgments_annotations_paragraphs_temp <- judgments_annotations_paragraphs_temp[[1]] %>% as.vector()
  judgments_annotations_paragraphs_temp <- judgments_annotations_paragraphs_temp[! judgments_annotations_paragraphs_temp %in% c("")]
  
  #Save the whole judgment text
  text_temp <- judgments_annotations$content[judgments_annotations$metadata$doc_id == judgments_annotations_parts$doc_id[i]]
  
  # Split the original parts tibble into paragraph level observations
  output <- foreach(j = seq(judgments_annotations_paragraphs_temp), .combine = "rbind") %do% {
    location_temp <- str_locate(string = text_temp, pattern = fixed(judgments_annotations_paragraphs_temp[j])) %>% as.list()
    text_length <- str_length(text_temp)
    output <- tibble(
      "doc_id" = judgments_annotations_parts$doc_id[i],
      "value" = judgments_annotations_paragraphs_temp[j],
      "tag" = judgments_annotations_parts$tag[i],
      "start" = as.numeric(location_temp[1])/text_length,
      "end" = as.numeric(location_temp[2])/text_length,
      "length" = str_length(judgments_annotations_paragraphs_temp[j])/text_length
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
word2vec_model_CBOW <- read.word2vec(file = "models/word2vec_model_CBOW.bin")

judgments_annotations_paragraphs$doc_id <- judgments_annotations_paragraphs$doc_id %>% make.unique()

# Create doc2vec model
doc2vec_df <- judgments_annotations_paragraphs %>% 
  select(doc_id, value) %>% 
  rename(text = value) %>% 
  doc2vec(word2vec_model_CBOW, newdata = ., type = "embedding") %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "doc_id") %>%
  drop_na()

# Create the final tibble for 
doc2vec_df <- judgments_annotations_paragraphs %>% 
  select(-value) %>% 
  left_join(doc2vec_df, .)

doc2vec_df$doc_id <- modify(doc2vec_df$doc_id, .f = str_remove, pattern = "\\.\\d+")


# Create a binary variable for the presence of dissent in the decision
doc2vec_df <- US_metadata %>% 
  select(doc_id, dissenting_opinion) %>% 
  left_join(doc2vec_df, ., by = "doc_id") %>% 
  mutate(dissenting_opinion = if_else(dissenting_opinion == "", 0, 1))

doc2vec_df <- doc2vec_df %>% modify(.f = unlist) %>% as_tibble()

save(data = doc2vec_df, file = "data/doc2vec_df.RData")
load(file = "data/doc2vec_df.RData")