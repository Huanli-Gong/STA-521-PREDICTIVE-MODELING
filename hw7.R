library(topicmodels)
library(tm)
library(SnowballC)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

data("AssociatedPress")

terms = Terms(AssociatedPress)
stemmed_terms = wordStem(terms, language = "english")

dimnames(AssociatedPress)$Terms = stemmed_terms

ap_lda <- LDA(AssociatedPress, k = 12, control = list(seed = 1234))

ap_lda <- posterior(ap_lda, newdata = AssociatedPress)$terms %>%
  as.data.frame(stringsAsFactors = FALSE) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE)

colnames(ap_lda) <- paste("topic", 1:ncol(ap_lda))

ap_topics <- ap_lda %>%
  rownames_to_column("term") %>%
  pivot_longer(cols = starts_with("topic"), 
               names_to = "topic", 
               values_to = "distinctivity")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(distinctivity, n = 10) %>%
  ungroup() %>%
  arrange(topic, -distinctivity)

ap_top_terms %>%
  mutate(term = reorder_within(term, distinctivity, topic)) %>%
  ggplot(aes(distinctivity, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
