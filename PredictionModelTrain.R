library(plyr)

train_players = read.csv('dota2.csv', header = T)
test_labels = read.csv('test_labels.csv', header = T)
test_players = read.csv('test_player.csv', header = T)
player_ratings = read.csv('player_ratings.csv', header = T)

# player_ratings_pos: only include positive ratings. 
player_ratings_pos = player_ratings[player_ratings$account_id >= 0,]
player_ratings_pos = player_ratings_pos[order(player_ratings_pos$account_id),]
rownames(player_ratings_pos) = NULL

# Add rating into data frame. If not exist in player_rating, just represent it with the initial rating.
get.rating = function(id) { 
  idx = findInterval(id, player_ratings_pos$account_id)
  idx = ifelse(player_ratings_pos$account_id[idx] == id, idx, 1)
  return (c(rating_mu = player_ratings_pos[idx, 4], rating_sigma = player_ratings_pos[idx, 5]))
}

train_basic = train_players[,c('match_id', 'account_id', 'hero_id', 'last_hits', 'Win')]
compute.win.eachplayer = function(rw) { return (rep(c(as.numeric(rw == 1), as.numeric(rw == 0)), each = 5, times = 1)) } 
test_basic = data.frame(test_players[,c('match_id', 'account_id', 'hero_id')], 
                        Win = as.vector(mapply(compute.win.eachplayer, test_labels$radiant_win))) # add win column into test_basic

# Hero-based statistics table for preprocess()
hero_winrate_table = tapply(train_basic$Win, train_basic$hero_id, mean)
hero_winttl_table = table(train_basic$hero_id)
hero_ave_lh_table = tapply(train_basic$last_hits, train_basic$hero_id, mean)

preprocess = function(df){ # Given basic df, add hero-based win_rate, total_win and last_hits, rating_mu, rating_sigma
  stopifnot(!is.null(df$hero_id) & !is.null(df$account_id))
  hero_id_str_v = as.character(df$hero_id) 
  df$hero_winrate = as.numeric(hero_winrate_table[hero_id_str_v])
  df$hero_winttl = as.numeric(hero_winttl_table[hero_id_str_v])
  df$hero_ave_lh = as.numeric(hero_ave_lh_table[hero_id_str_v])
  m_rating = t(mapply(get.rating, df$account_id))
  return (data.frame(df, m_rating))
}

featureEngineering = function(df){ # Transfrom every block of df (10 rows) into a vector of features for training/testing/prediction
  stopifnot(!is.null(df$rating_mu) & !is.null(df$rating_sigma) & !is.null(df$hero_winrate) & !is.null(df$hero_ave_lh))
  radWin = ifelse(is.null(df$Win), NA, df$Win[1])
  # 1. Total rating (mu & sigma)
  u1 = sum(df$rating_mu[1:5]); u2 = sum(df$rating_mu[6:10]);
  s1 = sqrt(sum(df$rating_sigma[1:5]^2)); s2 = sqrt(sum(df$rating_sigma[6:10]^2));
  
  # 2. Total winrate
  w1 = sum(df$hero_winrate[1:5]); w2 = sum(df$hero_winrate[6:10]);
  # tot.gm1 = sum(df$hero.total[1:5]); tot.gm2 = sum(df$hero.total[6:10]);
  
  # 3. Total last_hits
  lh1 = sum(df$hero_ave_lh[1:5]); lh2 = sum(df$hero_ave_lh[6:10]);
  
  v = c(radWin, u1, s1, u2, s2, w1, w2, lh1, lh2)
  return (v)
}

# Construct train/testing data
train_add = preprocess(train_basic) # Takes half an hour
test_add = preprocess(test_basic)
colName = c('radWin', 'u1', 's1', 'u2', 's2', 'w1', 'w2', 'lh1', 'lh2')
train_final_feature = daply(train_add, .(match_id), featureEngineering)
test_final_feature = daply(test_add, .(match_id), featureEngineering) 
colnames(train_final_feature) = colName
colnames(test_final_feature) = colName

# Model training, validation and storing.
logit1 = glm(radWin ~ u1 + s1 + u2 + s2 + w1 + w2 + lh1 + lh2,
             family = "binomial", data = data.frame(train_final_feature))
prdt1 = predict(logit1, newdata = data.frame(test_final_feature), type = "response")
sum(test_final_feature[,1] == round(prdt1)) / length(round(prdt1)) # Accuracy of the 1st version: 0.60076
saveRDS(logit1, "prediction_logit.rds")

