
library(googlesheets)
library(magrittr)
library(ggplot2)
library(dplyr)
library(viridis)
library(wordcloud)
library(lubridate)

process_word = function(x){ 
    if(!is.na(x)){
        splits = unlist(strsplit(x, ' '))
        if(length(splits) > 1){
            return(NULL)
        }
        if(nchar(x, allowNA = TRUE) > 15){
            return(NULL)
        }
        return(x)
    }else{
        return(NULL)
    }
}


process_word_vector = function(word_vector){
    processed_words = word_vector %>% sapply(process_word)
    processed_words[sapply(processed_words, is.null)] <- NULL
    return(data.frame(words = unlist(processed_words), stringsAsFactors = FALSE))
}


wine_sheet = gs_title('WinePartyAugust2016')
data = wine_sheet %>% gs_read

names(data) = c("timestamp", "first_name", "wine_number", "rating", "tasting_notes")

data$timestamp = mdy_hms(data$timestamp)

summary_table = data %>% group_by(wine_number) %>% summarise(mean_rating = mean(rating), sd_rating = sd(rating))

words = process_word_vector(data$tasting_notes)
word_counts = words %>% count(words) %>% arrange(desc(n))


rating_graph = ggplot(summary_table, aes(x = factor(wine_number), y = mean_rating, fill = mean_rating)) + 
                   geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
                   scale_fill_viridis(guide = FALSE) + 
                   geom_errorbar(aes(ymin = mean_rating - sd_rating, ymax = mean_rating + sd_rating), width = .2, position = position_dodge(.9)) +
                   labs(x = "Wine Number", y = "Rating", title = "Overall ratings")

# words = data.frame(words = sample(letters, 150, replace = TRUE), stringsAsFactors = FALSE)


wordcloud(word_counts$words, word_counts$n, max.words = 100, min.freq = 1, col = viridis(8, begin = .1, end = .95, option = 'C'))


ggplot(data, aes(x = timestamp, y = factor(rating))) + 
    geom_point(aes(fill = wine_number), col = "black", pch = 21, size = 2) +
    scale_fill_viridis(name = "Wine Number") + 
    labs(x = "Time", y = "Rating", title = "Ratings over time")





### Ratings by Time

```{r}
ggplot(data, aes(x = timestamp, y = factor(rating))) + 
    geom_point(aes(fill = wine_number), col = "black", pch = 21, size = 2) +
    scale_fill_viridis(name = "Wine Number") + 
    labs(x = "Time", y = "Rating", title = "Ratings over time")
```
