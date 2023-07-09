xfun::pkg_attach2("tidyverse", "ggplot2", "progress", "foreach", "jsonlite",  "word2vec", "udpipe", "tidytext")

source("../supporting_functions.R")
# Load data
US_metadata = readRDS("../data/US_metadata.rds")
# US_texts = readRDS("../data/US_texts.rds")
judgments_annotated_doc2vec = readRDS("../data/judgments_annotated_doc2vec.rds")

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

data_ud = readRDS(file = "../apex_courts_dataset/models/US_UDmodel.rds")

# Transform the UDPipe model into a paragraph-level lemmatized text with ID information as well as information on length and position of the paragraphs
udpipe_into_lemma = function(data, paragraphs = TRUE){
  if(paragraphs){
  output = data %>%
    group_by(doc_id, paragraph_id) %>%
    filter(str_length(lemma) >= 3) %>%
    filter(
      !str_detect(lemma, "[0-9]")
    ) %>%
    filter(upos != "PUNCT") %>%
    summarise(text = paste(lemma, collapse = " ") %>% # Normalise and tidy the text
                str_to_lower() %>%
                str_squish(),
              start = min(start, na.rm = TRUE),
              end = max(end, na.rm = TRUE),
              length = end - start) %>%
    ungroup() %>%
    group_by(doc_id) %>%
    mutate(
      length = length/max(end),
      start = start/max(end),
      end = end/max(end)
    )
  return(output)
  }
  else{
    output = data %>%
      group_by(doc_id) %>%
      filter(str_length(lemma) >= 3) %>%
      filter(
        !str_detect(lemma, "[0-9]")
      ) %>%
      filter(upos != "PUNCT") %>%
      summarise(text = paste(lemma, collapse = " ") %>% # Normalise and tidy the text
                  str_to_lower() %>%
                  str_squish())
    return(output)
  }
}

data_paragraphs = udpipe_into_lemma(data = data_ud) 
remove(data_ud)

# Word2vec
# Window parameter:  for skip-gram usually around 10, for cbow around 5
# Sample: sub-sampling of frequent words: can improve both accuracy and 
# speed for large data sets (useful values are in range 0.001 to 0.00001)
# # hs: the training algorithm: hierarchical so􏰂max (better for infrequent
# words) vs nega􏰁ve sampling (better for frequent words, better with low
#                              dimensional vectors)
word2vec_model_CBOW = word2vec(x = data_paragraphs$text, dim = 300, threads = 8)
write.word2vec(word2vec_model_CBOW, file = "models/word2vec_model_CBOW.bin")

# The more accurate but computationally intensive skip-gram
word2vec_model_skipgram = word2vec(x = data_paragraphs$text, dim = 300, type = "skip-gram", window = 10, threads = 8)
write.word2vec(word2vec_model_skipgram, file = "models/word2vec_model_skipgram.bin")

# Prepare the lemmatized paragraphs for classification
# Load the word2vec model for doc2vec use
word2vec_model = read.word2vec(file = "models/word2vec_model_skipgram.bin")

udpipe_to_doc2vec = function(data, model = word2vec_model) {
  output = data %>%
    mutate(doc_id = paste("doc_id=", doc_id, ", paragraph_id=", paragraph_id, sep = "")) %>%
    select(doc_id, text) %>% 
    doc2vec(object = model, newdata = ., type = "embedding") %>% 
    as_tibble(rownames = NA) %>%
    rownames_to_column(var = "doc_id") %>% 
    mutate(
      paragraph_id = doc_id %>%
        str_extract(pattern = "(?=(, paragraph_id=)).*") %>%
        str_remove(pattern = ", paragraph_id=") %>%
        as.numeric(),
      doc_id = doc_id %>%
        str_extract(pattern = "^.*(?=(, paragraph_id=))") %>%
        str_remove(pattern = "doc_id=") %>%
        str_remove(pattern = ", paragraph_id="))
  return(output)
}

data_paragraphs_doc2vec = udpipe_to_doc2vec(data = data_paragraphs, model = word2vec_model) %>%
  left_join(data_paragraphs, ., by = c("doc_id", "paragraph_id"))

saveRDS(data_paragraphs_doc2vec, file = "../data/data_paragraphs_doc2vec.rds")


## TRANSFORM ANNOTATED DATA INTO DOC2VEC
# Load annotated data and create it into tibble of tag-level observations, including creating paragraph IDs within group
judgments_annotated1 = jsonlite::fromJSON(txt = "../data/us_partitioning_sample1.json")
judgments_annotated2 = jsonlite::fromJSON(txt = "../data/us_partitioning_sample2.json")
judgments_annotated1 = judgments_annotated1$examples %>% as_tibble()
judgments_annotated2 = judgments_annotated2$examples %>% as_tibble()
judgments_annotated = bind_rows(judgments_annotated1, judgments_annotated2) %>%
  rename("text" = "content") %>%
  mutate(doc_id = metadata$doc_id)
remove(judgments_annotated1, judgments_annotated2)

judgments_annotated_parts = foreach(i = seq(judgments_annotated$annotations), .combine = "rbind") %:%
  foreach (j = 1:nrow(judgments_annotated$annotations[[i]]), .combine = "rbind") %do% {
    text_length = str_length(judgments_annotated$text[[i]])
    output = tibble(
      "doc_id" = judgments_annotated$doc_id[i],
      "text" = judgments_annotated$annotations[[i]][j,4],
      "class" = judgments_annotated$annotations[[i]][j,2],
      "start" = judgments_annotated$annotations[[i]][j,3]/text_length,
      "end" = judgments_annotated$annotations[[i]][j,1]/text_length,
      "length" = str_length(judgments_annotated$annotations[[i]][j,4])/text_length
    )
    return(output)
  }

judgments_annotated_paragraphs = foreach(i = seq(nrow(judgments_annotated_parts)), .combine = "rbind") %do% {
  # Create the temporary vector of paragraph texts
  judgments_annotated_paragraphs_temp = judgments_annotated_parts$text[i] %>% str_split(pattern = "\n\n")
  judgments_annotated_paragraphs_temp = judgments_annotated_paragraphs_temp[[1]] %>% as.vector()
  judgments_annotated_paragraphs_temp = judgments_annotated_paragraphs_temp[! judgments_annotated_paragraphs_temp %in% c("")]
  
  #Save the whole judgment text
  text_temp = judgments_annotated$text[judgments_annotated$doc_id == judgments_annotated_parts$doc_id[i]]
  
  # Split the original parts tibble into paragraph level observations
  output = foreach(j = seq(judgments_annotated_paragraphs_temp), .combine = "rbind") %do% {
    location_temp = str_locate(string = text_temp, pattern = fixed(judgments_annotated_paragraphs_temp[j])) %>% as.list()
    text_length = str_length(text_temp)
    output = tibble(
      "doc_id" = judgments_annotated_parts$doc_id[i],
      "text" = judgments_annotated_paragraphs_temp[j],
      "class" = judgments_annotated_parts$class[i],
      "start" = as.numeric(location_temp[1])/text_length,
      "end" = as.numeric(location_temp[2])/text_length,
      "length" = str_length(judgments_annotated_paragraphs_temp[j])/text_length
    )
    return(output)
  }
} %>% mutate(
  doc_id = doc_id %>% 
    make.unique()
  )

  # Clear up the environment
remove(judgments_annotated, judgments_annotated_parts)

# Set up UDPIPE
# ud_model = udpipe_download_model(language = "czech")
ud_model = udpipe_load_model(file = "../apex_courts_dataset/czech-pdt-ud-2.5-191206.udpipe")
n.cores = parallel::detectCores() - 1

# Lemmatize the text
judgments_annotated_paragraphs %<>%
  select(doc_id, text) %>% 
  udpipe(x = ., object = ud_model, parallel.cores = n.cores) %>%
  as_tibble() %>%
  select(doc_id, paragraph_id, start, end, lemma, upos) %>% 
  udpipe_into_lemma(paragraphs = FALSE) %>%
  left_join(judgments_annotated_paragraphs %>% 
              select(-text), .) %>%
  drop_na()

# Load the word2vec model for doc2vec use
word2vec_model = read.word2vec(file = "models/word2vec_model_skipgram.bin")

# Create doc2vec model
judgments_annotated_doc2vec = judgments_annotated_paragraphs %>% 
  select(doc_id, text) %>% 
  doc2vec(word2vec_model, newdata = ., type = "embedding") %>% 
  as.data.frame() %>% # Tibble throws error when working with rownames
  rownames_to_column(var = "doc_id") %>%
  drop_na() %>%
  left_join(., judgments_annotated_paragraphs %>% 
              select(-text)) %>%
  mutate(doc_id = doc_id %>% str_remove(pattern = "\\.\\d+"),
         class = factor(class)) %>% 
  modify(.f = unlist) %>% 
  as_tibble()

# Save the file
saveRDS(judgments_annotated_doc2vec, file = "../data/judgments_annotated_doc2vec.rds")
judgments_annotated_doc2vec = readRDS("../data/judgments_annotated_doc2vec.rds")

# Annotated texts into td-idf
judgments_annotated_dtm = judgments_annotated_paragraphs %>%
  select(doc_id, text) %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE) %>% 
  bind_tf_idf(word, doc_id, n) %>% 
  cast_dtm(doc_id, word, tf_idf) %>%
  as.matrix() %>%
  as_tibble(rownames = "doc_id") %>%
  left_join(., judgments_annotated_paragraphs %>% select(-text)) %>%
  mutate(doc_id = doc_id %>% str_remove(pattern = "\\.\\d+"),
         class = factor(class)) %>% 
  modify(.f = unlist)

# Save the file
saveRDS(judgments_annotated_dtm, file = "../data/judgments_annotated_dtm.rds")








