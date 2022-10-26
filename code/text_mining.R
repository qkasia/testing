#set working directory
setwd("C:/Users/Kasia/Documents")
# Library -----------------------------------------------------------------

library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)


library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

#library(ggmap)
#library(wordcloud)

### Sentiment Score Function
library("stringr")

library("plyr")


# Data Pull with twitteR package ------------------------------------------

library(twitteR)
library(rtweet)

#Establishing A Connection 
# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "1lxhc8say701rxryf6EazFTUi"
consumer_secret <- "ZjhRto9Jjrr9YDeKV8fcIjHugZ3ENrkTw0GIUEyX2euDkYuK9P"
access_token <- "1546824620753534976-7f22Vp7BecQujRe44nGP4qPioDzULM"
access_secret <- "gjCiA3wnJrtVvQO2khGm0I3xVcPVtaxtB91nuc3BJZ1jz"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#searchTwitter for search particular phrase in tweets ->search query
#geocode="60.0,15.00,150mi"
#min_retweets:100

tweets <- searchTwitter("Iphone", n=1000, lang="en", since="2022-10-20", until="2022-10-21")

tweets.df <- twListToDF(tweets)

######How do add location of user of tweets
#Create two new columns; one for storing the location information disclosed on a user’s Twitter bio, 
#and the other column for storing the URL to a user’s profile image

user_info <- lookupUsers(unique(tweets.df$screenName))
user_info.df <- twListToDF(user_info)
user_info.df<-user_info.df[11:12]
user_info.df<-subset(user_info.df, location!="")

library(sqldf)
## left join (substitute 'right' for right join)
results<-merge(x=tweets.df,y=user_info.df,by="screenName",all.x = TRUE)


#test fun
new<-for (user in tweets.df$screenName[1:100]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(tweets.df[tweets.df$screenName==user,]$user_location <- sapply(tweets.df, function(x) getUser(x)))
}


#screen name 
sc_name<-sapply(tweets, function(x) x$getScreenName())
head(sc_name)

#contingency table
table<-table(tweets.df$screenName)
head(table)

#sort the table desc order of tweet counts

sc_name_sort<-sort(table, decreasing = TRUE)
head(sc_name_sort)


#Filter Tweets

ukraine <- search_tweets("Superbowl -filter:retweets -filter:quote -filter:replies", n = 100)


#tweets posted by a given user to their timeline instead of searching based on a query
#include the includeRts=TRUE argument to get max of the number of tweets

user<-userTimeline("iga_swiatek", n=100, includeRts=TRUE)
user_profile<-lookupUsers("evestlee")
user_profile$location
user_profile <- twListToDF(user_profile)
network<-get_friends("iga_swiatek")

#get text of tweets subfunction to extract metadata from list
tweets.txt <- sapply(tweets, function(x) x$getText())

# Ignore graphical Parameters to avoid input errors - perform multiple replacements in each element of string
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")


# cleaning stage -------------------------------------------------

clean.text = function(x)
{

  # remove rt
  x = gsub("RT", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('htt.*','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = gsub('@.*', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  # convert to lower case
  x = tolower(x)
  return(x)
}

# test<-c("RT @davenewworld_2: Russian people are crossing the Georgian border on foot to escape Putin's tyranny https://t.co/E5yKdizAZ0")
cleanText <- clean.text(tweets.txt)


# remove empty results (if any)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]

#add index 
#tweets.df <- cbind(ID = 1:nrow(tweets.df), tweets.df)

# Getting the opinion lexicons from working directory -------------------


pos <-readLines("positive-words.txt")
neg <-readLines("negative-words.txt")


#add words to the lexicon

neg<-c(neg, "sad");
tail(neg)


# Sentiment Function ------------------------------------------------------
  
  # Function is called sentimentfun
  sentimentfun = function(tweettext, pos, neg, .progress='non')
  {
    # Parameters
    # tweettext: vector of text to score
    # pos: vector of words of postive sentiment
    # neg: vector of words of negative sentiment
    # .progress: passed to laply() 4 control of progress bar
    # we want a simple array of scores back so function laply:
    
  
  # create simple array of scores with laply
  scores = laply(tweettext,
                 function(x, pos, neg)
                 {
                   # tokenization, split sentence into words with str_split (stringr package)
                   
                   word.list = str_split(x, "\\s+")
                   words = unlist(word.list)
                   stop_words<- tidytext::stop_words
                   words<-unlist(words)[!(unlist(words) %in% stop_words)]

                   #match() returns the position of the matched term or NA
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   ## TRUE/FALSE will be treated as 1/0 by sum():
                   score = sum(pos.matches) - sum(neg.matches)
                   
                   return(score)
                 }, 
                 
                 pos, neg, .progress=.progress )
  
  # data frame with scores for each sentence
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}


## apply function score.sentiment
scores <- sentimentfun(cleanText, pos, neg, .progress='text')


# Sentiment Package -------------------------------------------------------
library(syuzhet)
library(tidyverse)
library(tm)
#sentiment

library(textstem)
lemma_dictionary <- make_lemma_dictionary(cleanText, engine = 'hunspell')
new<-lemmatize_strings(cleanText, dictionary = lemma_dictionary)
d <- tibble(txt = new)


bing_word_counts <- d %>% unnest_tokens(word, txt) %>% anti_join(get_stopwords()) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) 

bing_word_counts <-d %>% unnest_tokens(word, txt) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 


nrc_word_counts <-d %>% unnest_tokens(word, txt) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) 

#data frame with scores
nrc_data <- cleanText %>% get_nrc_sentiment()


sentimentTotals <- data.frame(colSums(nrc_data))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

#histogram showing the distribution of my tweets over time
ggplot(data = tweets.df, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# Visualization -----------------------------------------------------------


# sentiment score frequency table

table(scores$score)


#Histogram of sentiment score
scores %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequency") + 
  xlab("sentiment score") +
  ggtitle("Distribution of Sentiment scores of the tweets") +
  ggeasy::easy_center_title()

#Barplot of sentiment type:
neutral <- length(which(scores$score == 0))
positive <- length(which(scores$score > 0))
negative <- length(which(scores$score < 0))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count)) +
  geom_bar(stat = "identity", aes(fill = Sentiment)) +
  ggtitle("Barplot of Sentiment type of 5000 tweets")

#Wordcloud
#convert text vector to a collection of documents
##creates a corpus from a document source
text_corpus <- Corpus(VectorSource(cleanText))
# create a content transformer that replace string pattern by a space
# remove your own stop word
text_corpus <- tm_map(text_corpus,removeWords, c("putin","putins", "russian", "ukraine", "men", "let", "now", "think", "like", "amp", "russia", "vladimir", "will", "one", "russians", "can", "likely", "ukrainian", "country", "first", "just","women","asked","want"))
# convert text to lower case
text_corpus <- tm_map(text_corpus, content_transformer(tolower))

# remove numbers
text_corpus <- tm_map(text_corpus, removeNumbers)

# remove english common stopwords
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))

# remove punctuations
text_corpus <- tm_map(text_corpus, removePunctuation)

# eliminate extra white spaces
text_corpus <- tm_map(text_corpus, stripWhitespace)


#ngram version
#bigrams

# convert corpus to a data.frame
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)


# create wordcloud
#to make sure that we get the same results for randomization
set.seed(123)
wordcloud(words = tdm$word, freq = tdm$freq, scale = c(1,0.2), min.freq = 9,
          max.words = 200, random.order = FALSE, rot.per = 0.45,
          colors = brewer.pal(8, "Dark2"))

#word frequency plot
ggplot(tdm[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Terms") + 
  ylab("Count") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Most common word frequency plot') +
  ggeasy::easy_center_title()

#bigram
bi.gram.words <- data_samsung_galaxy2 %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words %>% 
  select(bigram) %>% 
  head(10)

extra.stop.words <- c('https')
stopwords.df <- tibble(
  word = c(stopwords(kind = 'es'),
           stopwords(kind = 'en'),
           extra.stop.words)
)

bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

bi.gram.count <- bi.gram.words %>% 
  dplyr::count(word1, word2, sort = TRUE) %>% 
  dplyr::rename(weight = n)

bi.gram.count %>% head()

bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram log-Weight Distribution")


threshold <- 50

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)






# Final stage data gathering and export----------------------------------------------------

## extracting further elements (besides text) for the export csv

tweetdate=lapply(tweets, function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet=sapply(tweets, function(x) x$getIsRetweet())
retweetcount=sapply(tweets, function(x) x$getRetweetCount())
favoritecount=sapply(tweets, function(x) x$getFavoriteCount())
isreply=sapply(tweets, function(x) x$getReplyToSID())
isreply[isreply=="character(0)"]<-0
isreply[isreply!=0]<-1

## Creating the Data Frame
data=as.data.frame(cbind(text=tweets.txt,
                         date=tweetdate,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         isreply=isreply,
                         score = scores$score,
                         location = results$location,
                         product = "Samsung Galaxy"))

## remove duplicates
data2 = duplicated(data[,1])
data$duplicate = data2


data[,3]<-as.character(data[,3])
data[,4]<-as.numeric(data[,4])
data[,5]<-as.numeric(data[,5])
data[,6]<-as.character(data[,6])
data[,7]<-as.numeric(data[,7])
data <- apply(data,2,as.character)
#add score from nrc data


## create file to wd row.names = FALSE
write.csv(data, file="data_samsung_galaxy2.csv")


write.csv(bing_word_counts, file="data_samsung_galaxy1.csv")


write.csv(nrc_word_counts, file="data_samsung_galaxy1.csv")

write.csv(nrc_data, file="data_samsung_galaxy1.csv")


data <- as.data.frame(data)
colnames(location)[1] <- "location"
# deleting punctuations
data$location<-gsub("[[:punct:][:blank:]]+", " ", data$location)
# deleting trailing space
data$location<-gsub("\\n"," ", data$location)
# remove numbers
data$location = gsub("[[:digit:]]", "", data$location)
data$location = gsub('[^[:graph:]]', ' ',data$location)


#creating a location column
data$description_stateUSA <- NA 
#using for loop to extract all the states 
for (i in 1:length(location$location)){    
  #split the text by space  
  split <- strsplit(location$location[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  state <- match(split, state.abb)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  data$description_stateUSA[i] <- state_split[1]  
}
library(ISOcodes)
#ISO_3166_1  UN_M.49_Countries
data$description_country <- NA 
for (i in 1:length(location$location)){    
  #split the text by space  
  split <- strsplit(location$location[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  state <- match(split, ISO_3166_1$Name)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  data$description_country[i] <- state_split[1]  
}


location$description_Alpha_3 <- NA 
for (i in 1:length(location$location)){    
  #split the text by space  
  split <- strsplit(location$location[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  state <- match(split, ISO_3166_1$Official_name)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  data$description_Alpha_3[i] <- state_split[1]  
}




rm(sqldf1)

library(countrycode)
data$City <- NA 

city_country <- read.csv("https://raw.githubusercontent.com/girijesh18/dataset/master/City_and_province_list.csv")
write.csv(city_country, file="city_country.csv")


for (i in 1:length(location$location)){    
  #split the text by space  
  split <- strsplit(location$location[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  state <- match(split, city_country$City)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  data$City[i] <- state_split[1]
}

#replace null with blank before merge 

location$description_stateUSA<-ifelse(location$description_stateUSA==" ",NA, "USA")


location_new<- location%>%
  unite(Country, description_stateUSA, description_country, description_Alpha_3, sep=" ", na.rm = TRUE) 

location_new <- location_new%>%unite(City, City, sep=" ", na.rm = TRUE) 
#ISO_3166_1 Name

# Random playing with data ------------------------------------------------
#make data frame
str(tweets)
df <- do.call("rbind", lapply(tweets, as.data.frame))

#most popular tweets, what is viral?
mostPopular <- df %>% 
               dplyr::select(text, retweetCount, screenName) %>% 
               arrange(desc(retweetCount))


#look for words, split tweets into words

nGrams <- mostPopular %>% unnest_tokens(word, text, token="ngrams", n=1)

#group by words, looking words associated with most virality online 
#group individual word, look at the mean retweet count associated with this word, filter words >10, arrange desc

nGramSort <-nGrams %>% 
            group_by(word) %>% 
            dplyr::summarize(n=n(), avg_retweets=mean(retweetCount)) %>%
            filter(n>10)  %>% 
  arrange(desc(avg_retweets))

#quanteda 



# test Twiteer API Call with httr package---------------------------------------------------------

library(rjson)
require(httr)
require(jsonlite)
require(dplyr)
library(purrr)

#set up to authenticate to the Twitter API

bearer_token <- "AAAAAAAAAAAAAAAAAAAAANiNhAEAAAAAwT6%2FkbRxwKuaeJLkbN1BCpOvb20%3DypyKOXS8Tmg85tHwioQGYZEcuyFcpdSWomfbnHwaj5YsORa4st"
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))


#httr package to make a GET request to the URL you just created, pass in our authentication credential via the header, and pass in the parameters you defined


# Tweets by user 


#define the parameters of your request

params <- list(user.fields = "public_metrics,description",
               expansions = "pinned_tweet_id")

#define user and endpoint

handle <- 'JoeBiden'
url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)


#sending the request to the API
response <- httr::GET(url = url_handle,
                      httr::add_headers(.headers = headers),
                      query = params)

obj <- httr::content(response, as = "text")
#JSON file
prettify(obj)
class(obj)
#conver JSON to data frame
json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame()
as_tibble(json_data)

#JSON is to use a data frame, which allows you to easily access complex nested data. 
#To do this, use the fromJSON method of the jsonlite package to flatten your file to allow the fields to be in the same object. 
#Then, pass that object into a data frame. Now you are ready to view this data frame.


# Search by Tweets


endpoint <- 'https://api.twitter.com/2/tweets/search/recent'

params <- list(query = "(iphone 14) lang:en -is:retweet -has:links",
               tweet.fields = "author_id,created_at,lang",
               expansions="geo.place_id",
               place.fields="country,country_code,id,name,place_type",
               max_results = 100)

response <- httr::GET(url = endpoint,
                      httr::add_headers(.headers = headers),
                      query = params) %>% 
  httr::content(., as = "text") %>% 
  fromJSON(., flatten = TRUE) %>% 
  as.data.frame()

as_tibble(response)


# Search by tweets example 2 and function for access more data


endpoint <- 'https://api.twitter.com/2/tweets/search/recent'

params <- list(query = "(Biden OR Trump)",
               tweet.fields = "author_id,created_at,public_metrics,geo",
               place.fields="country,country_code,id,name,place_type",
               max_results = 100)

params <- list(query = "(Biden OR Trump)",
               tweet.fields = "id,text,author_id,created_at,geo",
               place.fields="country,country_code,geo,name",
               expansions="author_id,geo.place_id",
               user.fields="location")




response1 <- httr::GET(url = endpoint,
                      httr::add_headers(.headers = headers),
                      query = params) %>% 
  httr::content(., as = "text") %>% 
  fromJSON(., flatten = TRUE) %>% 
  as.data.frame()

as_tibble(response)


#function to grab up to 3200 Tweets from a user

last_n_tweets <- function(bearer_token = "", user_id = "", n = 100,
                          tweet_fields = c("attachments",
                                           "created_at",
                                           "entities",
                                           "in_reply_to_user_id",
                                           "public_metrics",
                                           "referenced_tweets",
                                           "source")){
  
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))
  
  # Convert User ID into Numerical ID
  
  sprintf('https://api.twitter.com/2/users/by?usernames=%s', user_id) %>% 
    httr::GET(url = .,
              httr::add_headers(.headers = headers),
              query = list()) %>% 
    httr::content(.,as="text") %>% 
    fromJSON(.,flatten = T) %>% 
    as.data.frame() -> tmp
  
  num_id <- tmp$data.id
  
  # For that user, grab most recent n tweets, in batches of 100
  
  if(n <= 100){
    requests <- n
  }else{
    requests <- rep(100,floor(n/100))
    if(n %% 100 != 0){
      requests <- c(requests, n %% 100)
    }
  }
  
  next_token <- NA
  
  all <- list()
  
  # Initialize, grab first results  
  paste0('https://api.twitter.com/2/users/',num_id,'/tweets') %>% 
    httr::GET(url = .,
              httr::add_headers(.headers = headers),
              query = list(`max_results` = requests[1],
                           tweet.fields = paste(tweet_fields,collapse=","))) %>% 
    httr::content(.,as="text") %>% 
    fromJSON(.,flatten = T) %>% 
    as.data.frame() -> out
  
  all[[1]] <- out
  
  # For more than 100, need to use pagination tokens.
  if(length(requests) >= 2){
    next_token[2] <- unique(as.character(all[[1]]$meta.next_token))
    
    for(i in 2:length(requests)){
      paste0('https://api.twitter.com/2/users/',num_id,'/tweets') %>% 
        httr::GET(url = .,
                  httr::add_headers(.headers = headers),
                  query = list(`max_results` = requests[i],
                               tweet.fields = paste(tweet_fields,collapse=","),
                               pagination_token = next_token[i])) %>% 
        httr::content(.,as="text") %>% 
        fromJSON(.,flatten = T) %>% 
        as.data.frame() -> out
      
      all[[i]] <- out
      next_token[i + 1] <- unique(as.character(all[[i]]$meta.next_token))
    }
  }
  
  
  do.call("rbind.fill",all)
  
}

out <- last_n_tweets(bearer_token,"JoeBiden",3200)

as_tibble(out)




#sapply() function takes list, vector or data frame as input and gives output in vector or matrix. It is useful for operations on list objects and returns a list object of same length of original set. Sapply function in R does the same job as lapply() function but returns a vector.

#Arguments:
#  -X: A vector or an object
#-FUN: Function applied to each element of x