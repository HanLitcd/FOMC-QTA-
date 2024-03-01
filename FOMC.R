library(quanteda)
library(stm) # STM
library(wordcloud)
library(word2vec)
library(ggplot2)
if(!require(devtools)) install.packages("devtools")
# Next two lines used to install stmBrowser from GitHub
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE) # select option 1 to install all dependencies
library("stmBrowser")
library(quanteda.textstats)
set.seed(21)
library("LDAvis")
library(fmsb)
library(tidyverse)
speech_data <- read.csv("5yearspeeches.csv", 
                        stringsAsFactors=FALSE,
                        encoding = "utf-8")
corpus <- corpus(speech_data$Text)
docvars(corpus) <- speech_data
# create tokens object
toks <- tokens(corpus,
               include_docvars = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(stopwords('english'), padding = TRUE) %>% 
  tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE) %>%
  tokens_remove('amp', valuetype = 'fixed', padding = TRUE)

# detect collocations and merge with tokens object (choose your own parameters)
col <- textstat_collocations(toks, 
                             method = "lambda", 
                             size = 2, 
                             min_count = 10,
                             smoothing = 0.5
)

toks <- tokens_compound(toks, pattern = col[col$z > 3,]) 
toks <- tokens_remove(tokens(toks), "")
docfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

stmdfm <- convert(docfm, to = "stm")
modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = 15,
                prevalence = ~ Speaker + occasion,
                data = stmdfm$meta,
                max.em.its = 500,
                init.type = "Spectral",
                seed = 1234,
                verbose = TRUE)
toLDAvis(mod = modelFit,
         docs = stmdfm$documents,
         open.browser = interactive(),
         reorder.topics = TRUE)
clusters <- list(
  Hawks = c(1,11, 7, 6, 15),
  Doves = c(5,9, 13, 12,14),
 Stability = c( 8, 10, 2, 3, 4))
topic_probabilities = modelFit$theta
cluster_weights <- matrix(nrow = nrow(topic_probabilities), ncol = length(clusters), dimnames = list(NULL, names(clusters)))
# Aggregate weights for each cluster
for (cluster_name in names(clusters)) {
  cluster_indices <- clusters[[cluster_name]]
  cluster_weights[, cluster_name] <- rowSums(topic_probabilities[, cluster_indices])
}

# Determine the dominant cluster for each document
max_cluster_per_doc <- apply(cluster_weights, 1, which.max)
dominant_cluster <- names(clusters)[max_cluster_per_doc]
speech_data$Dominant_Cluster <- dominant_cluster

topic_counts <- speech_data %>%
  group_by(Speaker, Dominant_Cluster) %>%
  summarise(count = n()) %>%
  ungroup()
speaker_totals <- topic_counts %>%
  group_by(Speaker) %>%
  summarise(total_count = sum(count))
topic_shares <- topic_counts %>%
  left_join(speaker_totals, by = "Speaker") %>%
  mutate(share = count / total_count)
radar_data <- topic_shares %>%
  pivot_wider(names_from = Dominant_Cluster, values_from = share)
radar_data <- radar_data %>%
  select(Speaker, Doves, Hawks, Stability)
radar_data<- radar_data %>%
  group_by(Speaker) %>%
  summarise(Doves = coalesce(mean(Doves, na.rm = TRUE), 0),
            Hawks = coalesce(mean(Hawks, na.rm = TRUE), 0),
            Stability = coalesce(mean(Stability, na.rm = TRUE), 0))
#validation
return_data<-read.csv('withreturnlong.csv')
range_data<- read.csv('withrangelong.csv')
#reg_df_range <- cbind(cluster_weights,range_data[,c('BTC.USD','DX.Y.NYB')])
#reg_df_return<-cbind(cluster_weights,return_data[,c('BTC.USD','DX.Y.NYB')])
#reg_df$Date <-as.Date(return_data$Date)

rdt<-cbind(return_data,cluster_weights)
rdv<-cbind(range_data,cluster_weights)
rdt$logbirds<- log(rdt$Hawks/rdt$Doves)
pw_rdt<-rdt[rdt$Speaker=='Chair Jerome H. Powell',c('logbirds','Stability','BTC.USD','DX.Y.NYB','Date')]
scatter_plot <- ggplot(pw_rdt, aes(x = logbirds, y = DX.Y.NYB)) +
  geom_point() +
  labs(title = "Scatter Plot",
       x = "log position",
       y = "BTC Range") +
  theme_minimal()
pwplot<-ggplot(pw_rdt, aes(x = as.Date(Date), y = logbirds)) +
       geom_line() 
       labs(title = "Line Plot of logbirds Over Time",
                       x = "Date",
                     y = "logbirds") +geom_smooth(method = "loess", se = FALSE, color = "red")+
       theme_minimal()
#add cpi
cpi<-read.csv('CPIAUCSL.csv')
cpi$DATE <- as.Date(cpi$DATE)
pw_rdt$Date <- as.Date(pw_rdt$Date)
cpi$Year_Month <- format(cpi$DATE, "%Y-%m")
pw_rdt$Year_Month <- format(pw_rdt$Date, "%Y-%m")
merged_data <- merge(pw_rdt[,c('Date','Year_Month','logbirds')], cpi, by.x = "Year_Month", by.y = "Year_Month", all.x = TRUE)
cpiplot <- ggplot(merged_data, aes(x = Date)) +
  geom_line(aes(y = logbirds, color = "logbirds"), size = 1) +  # Add line plot for logbirds
  geom_smooth(aes(y = logbirds, color = "logbirds smoothed"), method = "loess", se = FALSE, size = 1.5) +  # Add smoothed line for logbirds
  geom_line(aes(y = CPIAUCSL_CCA, color = "CPI"), size = 1) +  # Add line plot for CPI
  labs(title = "Line Plot of lg(Hawk/Dove) and CPI Over Time",
       x = "Date",
       y = "Value") +
  scale_color_manual(name = "Variable", 
                     values = c("logbirds" = "blue", "Relative hawkishness smoothed" = "green", "CPI" = "red")) +  # Specify colors and labels for legend
  theme_minimal()

#validate stability
lm_rdv<- lm(formula = Stability ~ BTC.USD + DX.Y.NYB, data = rdv)

#word2vec
docs <- as.list(toks) # extract doc for word2vecc
docs <- tolower(docs)
model <- word2vec(x = docs, 
                  type = "skip-gram",
                  dim = 300, 
                  window = 6, 
                  iter = 10, 
                  threads = 15)
write.word2vec(model, 
               file = "feddocs", 
               type = c("bin", "txt"), 
               encoding = "UTF-8")
emb <- as.matrix(model)
#wordcloud
# Define the number of clusters
# Define the number of top words to display for each topic
# Define the number of top words to display for each topic
# Assuming 'modelFit' is your STM model object and 'clusters' is your list of clusters with topic numbers

# Corrected function to extract 'Highest Prob' top words for specified topics
# Assuming 'modelFit' is your STM model object and 'clusters' is your list of clusters with topic numbers
extractTopWordsForCluster <- function(modelFit, topicNumbers) {
  words <- unlist(lapply(topicNumbers, function(topic) {
    labelTopics(modelFit, topics = topic, n = 10)$prob[1,]
  }))
  return(unique(words))
}

# Extract top words for each cluster
hawksWords <- labelTopics(modelFit,,10)$prob[clusters$Hawks,]
dovesWords <- labelTopics(modelFit,,10)$prob[clusters$Doves,]
stabilityWords <- labelTopics(modelFit,,10)$prob[clusters$Stability,]

# Combine words into a single data frame for the word cloud
wordsDF <- data.frame(
  word = c(hawksWords, dovesWords, stabilityWords),
  cluster = factor(c(rep("Hawks", length(hawksWords)), 
                     rep("Doves", length(dovesWords)), 
                     rep("Stability", length(stabilityWords))))
)
# Assuming wordsDF is your initial dataframe


add_cluster_marker <- function(word, cluster) {
  if(cluster == "Hawks") {
    return(word)  # No marker for Hawks for simplicity
  } else if(cluster == "Doves") {
    return(paste0(word, "_"))  # Append '_d' for Doves
  } else if(cluster == "Stability") {
    return(paste0(word, "+"))  # Append '_s' for Stability
  } else {
    return(word)  # Default case, no marker
  }
}
wordsDF$word_marked <- mapply(add_cluster_marker, wordsDF$word, wordsDF$cluster)
wordsDF <- wordsDF %>%
  group_by(word_marked,cluster) %>%
  summarise(freq = n(), .groups = 'drop')
wordsDF$color <- ifelse(wordsDF$cluster=='Doves', "green",  # For Doves
                        ifelse(wordsDF$cluster=='Stability', "blue",  # For Stability
                               "red"))
wordcloud(words = wordsDF$word_marked, freq = wordsDF$freq, min.freq = 1,
          max.words = Inf, random.order = FALSE, rot.per = 0.35)

