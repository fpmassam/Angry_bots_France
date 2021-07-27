require(rtweet)
require(syuzhet)
require(tidyverse)
require(tweetbotornot)

api_key = ''
api_secret = ''
access_token = ''
access_secret = ''

token = create_token(
  app = 'EuPolitics',
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret)

#Get_tweets
France = search_tweets('#PassSanitaire', retryonratelimit = TRUE)
users = data.frame(table(France$user_id))
save.image()
tweets_only = France %>% select(status_id, text)
sentiment_nrc_tweets = function(x){
  require(pbapply)
  testi = split(x, f = x$status_id)
  foo = function(a){
    tokens = get_tokens(a$text)
    sent = get_nrc_sentiment(tokens, language = 'french')
    }
  sent = pblapply(testi, foo)
  return(sent)
}
save.image()
nrc_tweets = sentiment_nrc_tweets(tweets_only)
save.image()
nrc_tweets = lapply(nrc_tweets, colSums)
nrc_tweets = do.call(rbind, nrc_tweets)
nrc_tweets = data.frame(tweet = rownames(nrc_tweets), nrc_tweets)
rownames(nrc_tweets) = NULL
key = France %>% select(status_id, screen_name, created_at)
nrc_tweets = merge(key, nrc_tweets, by.x = 'status_id', by.y = 'tweet')
ggplot(nrc_tweets, aes(x = created_at, y = anger)) +  
  geom_area(alpha = .07) +
  geom_line(
  aes(y = zoo::rollmean(anger, k = 20, fill = NA))
)

nrc_total = as.data.frame(colSums(nrc_tweets[4:13]))
nrc_total = data.frame(
  Sentiment = rownames(nrc_total),
  Value = nrc_total
)
colnames(nrc_total)[2] = 'Value'
rownames(nrc_total) = NULL
nrc_total = nrc_total[order(nrc_total$Value),]
pos_neg = subset(nrc_total, Sentiment %in% c('positive', 'negative'))
just_nrc = subset(nrc_total, !(Sentiment %in% c('positive', 'negative')))
just_nrc = just_nrc %>%
  mutate(Share = round(Value/sum(Value),2))
#Sentiment share 
ggplot(just_nrc, aes(x = 1, y = Share, fill = reorder(Sentiment, Value))) + geom_col() + 
  xlab('') + ylab('') + scale_fill_brewer(palette =  'Set3') +
  labs(title = 'Anger and fear strong in French Twitter',
                  subtitle = '% of sentiment over the text of 18,000 tweets',
                  caption = 'SOURCE: Share of Nrc scores/Syuzhet R package') +
  geom_text(aes(y = Share, label = Share), position = position_stack(vjust = .5), 
            color = 'black', size = 4) +
  coord_flip() + 
  picci + theme(
   panel.grid.major.y = element_blank(),
   panel.grid.minor.y = element_blank(),
   panel.grid.major.x = element_blank(),
   panel.grid.minor.x = element_blank(),
   axis.ticks.x = element_line(),
   axis.text.y = element_blank(),
   legend.position = 'top',
   legend.box = 'horizontal',
   legend.justification = 0
  ) +  guides(fill=guide_legend(title="",
                                nrow = 2,
                                reverse = TRUE))
ggsave('sentiment.png', width = 20, height = 8, units = 'cm')

#Sentiment_correspondence
ggplot(nrc_tweets, aes(x = anger, y = fear)) + geom_tile(aes(fill = fear))

#Top angry bots?
anger = aggregate(anger~screen_name, FUN = sum, data = nrc_tweets)
nrc_tweets = nrc_tweets[order(-nrc_tweets$anger),]
angry_tweets =  tweet_shot(nrc_tweets[2,]$status_id)

anger = anger[order(-anger$anger),]
bot_seeking = anger[1:10,]
Angry_bot_agg = botornot(bot_seeking$screen_name)
Angry_bot_agg
ggplot(bot_seeking, aes(y = reorder(screen_name, anger), x = anger)) + geom_col(
  fill = 'SteelBlue3') + scale_x_continuous(expand = c(0,0)) +
  geom_text(aes(x = 2, y = screen_name, label = paste('Anger:',anger))) + ylab('') + 
  picci + 
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.text.x = element_blank()) + 
  labs(title = 'Angry netizens?',
       subtitle = 'Top ten angry Twitter profile',
       caption = 'SOURCE: Twitter API, Syuzhet R package')
ggsave('anger.png', width = 20, height = 8, units = 'cm')


ggplot(Angry_bot_agg, aes(y = reorder(screen_name, prob_bot), x = prob_bot)) + geom_col(
  fill = 'SteelBlue3') + scale_x_continuous(expand = c(0,0)) +
  geom_text(aes(x = .04, y = screen_name, label = round(prob_bot, 3))) + ylab('') + 
  picci + 
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.text.x = element_blank()) + 
  labs(title = 'Angry bots',
       subtitle = 'Probability (%) that profiles are bot',
       caption = 'SOURCE: Twitter API, Tweetbotornot')
ggsave('bot_or_not.png', width = 20, height = 8, units = 'cm')

View(nrc_tweets)

#Sentiment evolution
ggplot(nrc_tweets, aes(x = created_at)) + 
  geom_line(aes(y = zoo::rollmean(anger, k = 50, fill = NA)), color = 'red',
            alpha = 1) +
  geom_line(aes(y = zoo::rollmean(fear, k = 50, fill = NA)), color = 'blue',
            alpha = .5) +
  geom_line(aes(y = zoo::rollmean(joy, k = 50, fill = NA)), color = 'yellow')

#rewtweets
retweets = data.frame(table(France$is_retweet))
retweets$type = c('Original', 'Reteweet')
retweets$share = retweets$Freq/sum(retweets$Freq)
ggplot(retweets, aes(x = 1, y = share, fill = reorder(type, Freq))) + geom_col() + 
  xlab('') + ylab('') + scale_fill_brewer(palette =  'Set3') +
  labs(title = 'Tweets are overwhelmingly not origintal',
       subtitle = '% of retweets and original tweets',
       caption = 'SOURCE: Twitter API') +
  geom_text(aes(y = share, label = round(share, 3)), position = position_stack(vjust = .5), 
            color = 'black', size = 4) +
  coord_flip() + 
  picci + theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    legend.position = 'top',
    legend.box = 'horizontal',
    legend.justification = 0
  ) +  guides(fill=guide_legend(title="",
                                nrow = 1,
                                reverse = TRUE))
ggsave('retweets.png', width = 20, height = 8, units = 'cm')


#ANger and retweet
tweet_info = France %>% select(
  status_id, retweet_count, is_retweet
)

tweet_info = merge(nrc_tweets, tweet_info, by = 'status_id')
library(ppcor)
require(psych)
require(ggfortify)
pcor(tweet_info[4:11])
inds = tweet_info[4:11]
inds_matrix = cor(inds)
KMO(inds_matrix)
parallel <- fa.parallel(inds, fm = 'minres', fa = 'fa')
fanone <-  fa(r=inds, nfactors = 2, rotate="varimax",fm="pa")
fa.diagram(fanone)
head(fanone$scores) 
tweet_info = cbind(tweet_info, fanone$scores)
colnames(tweet_info)[16:17] = c('Anti Green Pass sentiment', 'Confidence')
summary(lm(retweet_count~Confidence, data = tweet_info))
ggplot(tweet_info, aes(x = `Anti Green Pass sentiment`, y = retweet_count)) + geom_point(size = 4, color = 'black',
                                                                   fill = 'SteelBlue3',
                                                                   shape = 21,
                                                                   alpha = .5) +
  geom_smooth(method = lm, se = FALSE, color = 'red') +
  ylab('Number of retweets') +  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) + 
  picci + labs(title = 'Being against the Green Pass is not cool',
               subtitle = 'Retweets independent from Green Pass opposition',
               caption = 'SOURCE: Factorial analysis, Twitter API')

ggsave('retweets.png', width = 20, height = 10, units = 'cm')

#Assess vaxx campaign 
Vax = read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv')
Vax_france = subset(Vax, location == 'France')
Vax_france$Seven_Days_Avg = zoo::rollmean(Vax_france$daily_vaccinations_raw, k = 7, fill = NA)
label_1 = subset(Vax_france, 
                 Seven_Days_Avg == 688285.0)
label_2 = subset(Vax_france, 
                 daily_vaccinations_raw == 865654)
ggplot(Vax_france, aes(x = date, y = daily_vaccinations_raw)) + geom_col(fill = 'SteelBlue3',
                                                                         alpha = .5) + 
  geom_line(aes(y = Seven_Days_Avg), color = 'red') + 
  geom_vline(xintercept = as.Date('2021-07-12'), linetype = 'dashed', ymin = 0,
             alpha = .6) +
  annotate('text', x = as.Date('2021-07-8'), y = 200000, label = "Macron's statement",
           angle = 90) + geom_text(data = label_1, 
                                   aes(x = date, 
                                       y = Seven_Days_Avg, 
                                       label = paste("7-day avg:", 
                                                     scales::comma(Seven_Days_Avg),
                                                     'as of', (format(date, format = "%b %d")
                                                     ))),
                                   color = 'red',
                                   hjust = 1) + 
  geom_text(data = label_2, 
            aes(x = date, 
                y = daily_vaccinations_raw, 
                label = paste("Daily vaccinations", 
                              paste0(format(date, format = "%b %d"),':')
                              , scales::comma(daily_vaccinations_raw)),
            hjust = 1),
            color = 'SteelBlue4') + 
  labs(
    x = "",
    y = '',
    title = "Macron's statement boosted vaccinations",
    subtitle = "Numbers grew quickly in France",
    caption = 'SOURCE: Our world in data'
  ) + scale_y_continuous(labels = function(x) format(x, big.mark = ",")) + picci
  
ggsave('vaccination_france.png', width = 20, height = 12, units = 'cm')

