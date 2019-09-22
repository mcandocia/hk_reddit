library(readr)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
library(cetcolor)
library(knitr)

sc_rel_df = read_csv('subreddit_comment_relationships.csv')

st_rel_df = read_csv('subreddit_thread_relationships.csv')

csubs = c('China','Sino','HongKong','Hong_Kong')

com_weights = read_csv('general_comment_weights.csv')
t_weights = read_csv('general_thread_weights.csv')

comment_creddit = read_csv('comment_creddit_scores.csv')
thread_creddit = read_csv('thread_creddit_scores.csv')
creddit = bind_rows(
  comment_creddit %>% mutate(type='comment'),
  thread_creddit %>% mutate(type='submission')
)

ngrams = read_csv('hongkong_ngrams.csv')

hcom_weights = read_csv('hongkong_comment_weights.csv')
ht_weights = read_csv('hongkong_thread_weights.csv')

target_transpose <- function(data){
  data2 = data
  data2 = data2 %>% mutate(
    source = data$target,
    target = data$source,
    sub_1_count = data$sub_2_count,
    sub_2_count = data$sub_1_count,
    average_ratio = 1/average_ratio
  )
  data2
}

cmutate <- function(data){
  data %>%
    filter(
      source %in% csubs | target %in% csubs
    ) %>%
    mutate(
      sub1 = ifelse(
        source %in% csubs,
        source,
        target
      ),
      sub2 = ifelse(
        source %in% csubs,
        target,
        source 
      )
    )
}

# comments
top_comments = sc_rel_df %>%
  dplyr::union_all((sc_rel_df %>% target_transpose())) %>% 
  filter(source %in% csubs | target %in% csubs)
  
selected_comments_adj = top_comments %>% 
  filter(
  source %in% csubs & weight >= 3
) %>%
  left_join(com_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/(n_authors+10), adj_weight=adj_weight/max(adj_weight,na.rm=T)) %>%
  arrange(desc(adj_weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)

selected_comments_adjh = top_comments %>% 
  filter(
    source %in% csubs & weight >= 3
  ) %>%
  left_join(hcom_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/(n_authors+10), adj_weight=adj_weight/max(adj_weight,na.rm=T)) %>%
  arrange(desc(adj_weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)

selected_comments = top_comments %>% 
  filter(
    source %in% csubs & weight >= 3
  ) %>%
  left_join(com_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/(n_authors+10), adj_weight=adj_weight/max(adj_weight, na.rm=T), weight=weight/max(weight,na.rm=T)) %>%
  arrange(desc(weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)

# threads
top_threads = st_rel_df %>%
  dplyr::union_all((st_rel_df %>% target_transpose())) %>% 
  filter(source %in% csubs | target %in% csubs)

selected_threads_adj = top_threads %>% 
  filter(
    source %in% csubs
  ) %>%
  left_join(t_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/(n_authors+10), adj_weight=adj_weight/max(adj_weight,na.rm=T)) %>%
  arrange(desc(adj_weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)

selected_threads_adjh = top_threads %>% 
  filter(
    source %in% csubs
  ) %>%
  left_join(ht_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/(n_authors+10), adj_weight=adj_weight/max(adj_weight,na.rm=T)) %>%
  arrange(desc(adj_weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)



selected_threads = top_threads %>% 
  filter(
    source %in% csubs
  ) %>%
  left_join(t_weights %>% rename(target=subreddit), on='target') %>%
  group_by(source) %>%
  mutate(adj_weight = weight/n_authors, weight=weight/max(weight, na.rm=T)) %>%

  arrange(desc(weight)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  arrange(source, rank)


# viz

ggplot(selected_comments_adj) +
  geom_bar(aes(x=rank, y=adj_weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=adj_weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who commented 3+ times in both subreddits, scaled by approximate size of subreddit userbase.') + 
  theme_bw()


ggplot(selected_threads_adj) +
  geom_bar(aes(x=rank, y=adj_weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=adj_weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who made submissions in both subreddits, scaled by approximate size of subreddit userbase.') + 
  theme_bw()



ggplot(selected_comments_adjh) +
  geom_bar(aes(x=rank, y=adj_weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=adj_weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who commented 3+ times in both subreddits, scaled by approximate size of subreddit userbase from sample.') + 
  theme_bw()


ggplot(selected_threads_adjh) +
  geom_bar(aes(x=rank, y=adj_weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=adj_weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who made submissions in both subreddits, scaled by approximate size of subreddit userbase from sample.') + 
  theme_bw()

ggplot(selected_comments) +
  geom_bar(aes(x=rank, y=weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who commented 3+ times in both subreddits, unscaled.') + 
  theme_bw()


ggplot(selected_threads) +
  geom_bar(aes(x=rank, y=weight, fill=source), stat='identity') + 
  geom_label(aes(x=rank, y=weight + (1:length(rank) %% 2 - 0.5) * (nchar(target) > 12) * 0.12, label=target), alpha=0.7, size=3) +
  facet_grid(source~.) +
  scale_x_discrete('Rank') + 
  scale_fill_discrete('Subreddit', cet_pal(4, 'rainbow')) + 
  ylab('Relative Occurrence') + 
  ggtitle('Relative Popularity of Subreddits Related to Hong Kong',subtitle='Based on users who made submissions in both subreddits, unscaled.') + 
  theme_bw()


ggplot(creddit) +
  geom_boxplot(aes(x=subreddit, y=score, fill=subreddit)) + 
  facet_grid(type~.) + 
  coord_flip() + 
  ggtitle('Reddit Ban Risk Score Profile of Hong Kong-related Subreddits', subtitle='0 = highest risk, 1,000 = lowest; risk halves every 100 points; see /u/CredditReportingBot') +
  ylab('Risk Score (0=highest risk, 1,000=lowest risk)') + 
  xlab('Subreddit') +
  scale_y_continuous(breaks=seq(0,1000, 100)) +
  theme_bw() + 
  geom_abline(slope=0, intercept=c(200,400), lty='dashed', color='red') + 
  scale_fill_discrete(cet_pal(4, 'rainbow'))


max_ratio <- function(x){
  x/sapply(1:length(x), function(i) max(x[-i]))
}

ranked_ngrams = ngrams %>% 
  filter(!grepl('\\?|\\.', text)) %>%
  group_by(category) %>%
  mutate(prop = prop.table(total_count)) %>%
  ungroup() %>%
  group_by(text) %>%
  filter(sum(total_count > 10) > 2) %>%
  mutate(
    ratio = max_ratio(prop)
  ) %>% 
  filter(
    ratio > 1
  ) %>%
  ungroup() %>%
  group_by(category) %>%
  arrange(desc(ratio)) %>%
  mutate(
    rank = 1:n()
  ) %>%
  ungroup() %>%
  arrange(category, rank)


ggplot(ranked_ngrams %>% rename(subreddit=category) %>% filter(rank <= 15)) + 
  geom_bar(aes(x=rank, y=ratio, fill=subreddit), stat='identity') + 
  geom_label(aes(x=rank, y=ratio, label=text), hjust='inward', size=3.5) + 
  facet_grid(.~subreddit, scales='free') + 
  coord_flip() + 
  scale_x_reverse('Rank', breaks=1:15) + 
  scale_y_continuous('Ratio vs. Next Highest') + 
  ggtitle('Most Distinctive Phrases Used Among Hong Kong-related Subreddits') + 
  theme_bw() + theme_readable +
  theme(plot.title=element_text(size=rel(2))) +
  guides(fill=FALSE)
 

knit('article.rhtml')
