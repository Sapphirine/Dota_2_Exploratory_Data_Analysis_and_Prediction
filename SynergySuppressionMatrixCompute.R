## Synergy & Suppression Matrix Computation. 
## Create 'node.csv' & 'edge.csv' for IBM System G.

train_players = read.csv('dota2.csv', header = T)
train_basic = train_players[,c('match_id', 'account_id', 'hero_id', 'last_hits', 'Win')]

synergy = data.frame(matrix(0, nrow = 111, ncol = 111))
colnames(synergy) = paste(rep('C', 111), names(table(train_basic$hero_id)), sep = '') # Add 'R' & 'C' to the row/col name to make it more readable
rownames(synergy) = paste(rep('R', 111), names(table(train_basic$hero_id)), sep = '')
suppression_rowHeroWin = synergy  # synergy & total_teammate are synergy matrixes; suppression_rowHeroWin & total_enemy are suppression matrix
total_teammate = synergy
total_enemy = synergy

counter = 1
# Construct Synergy & Suppression matrixes
for (i in 1:(nrow(train_basic)/10)){
  if (counter %% 1000 == 0) # counter to remind about the procedure
    print (counter)
  counter = counter + 1
  
  # Extract 10 rows of data as data for one game
  radiant_win = train_basic$Win[10*i-9] 
  hero_id_v = train_basic$hero_id[(10*i-9): (10*i)]
  
  # Update Synergy matrix
  rc_mtx = matrix(rep(c('R', 'C'), 10), nrow = 10, byrow = T)
  pair_mtx = t(combn(hero_id_v[1:5], 2)) # Radiant data
  rc_name_mtx = matrix(paste(rc_mtx, pair_mtx, sep = ''), nrow = 10)
  for (j in 1:nrow(pair_mtx)){ # compute synergy for radiant
    rn = rc_name_mtx[j, 1]; cn = rc_name_mtx[j, 2]
    total_teammate[rn, cn] = total_teammate[rn, cn] + 1
    if (radiant_win){ synergy[rn, cn] = synergy[rn, cn] + 1 }
  }
  pair_mtx = t(combn(hero_id_v[6:10], 2)) # Dire data
  rc_name_mtx = matrix(paste(rc_mtx, pair_mtx, sep = ''), nrow = 10)
  for (j in 1:nrow(pair_mtx)){# compute synergy for dire 
    rn = rc_name_mtx[j, 1]; cn = rc_name_mtx[j, 2]
    total_teammate[rn, cn] = total_teammate[rn, cn] + 1
    if (!radiant_win){ synergy[rn, cn] = synergy[rn, cn] + 1 }
  }
  
  # Update Synergy matrix
  rc_mtx = matrix(rep(c('R', 'C'), 25), nrow = 25, byrow = T) # 25 elements in the matrix need updation
  pair_mtx1 = as.matrix(expand.grid(hero_id_v[1:5], hero_id_v[6:10]))
  pair_mtx2 = as.matrix(expand.grid(hero_id_v[6:10], hero_id_v[1:5]))
  rc_name_mtx1 = matrix(paste(rc_mtx, pair_mtx1, sep = ''), nrow = 25)
  rc_name_mtx2 = matrix(paste(rc_mtx, pair_mtx2, sep = ''), nrow = 25)
  for (j in 1:nrow(pair_mtx)){
    rn1 = rc_name_mtx1[j, 1]; cn1 = rc_name_mtx1[j, 2]
    rn2 = rc_name_mtx2[j, 1]; cn2 = rc_name_mtx2[j, 2]
    total_enemy[rn1, cn1] = total_enemy[rn1, cn1] + 1 # total_enemy[i, j] is the number of games when hero i & hero j appear in different sides of the team
    if (radiant_win)
      suppression_rowHeroWin[rn1, cn1] = suppression_rowHeroWin[rn1, cn1] + 1 
    # suppression_rowHeroWin[i, j] is the number of games when hero i win, and hero j is in the other team
    else
      suppression_rowHeroWin[rn2, cn2] = suppression_rowHeroWin[rn2, cn2] + 1
  }
}

## Compute syn_coef and sup_coef for hero1 and hero2; 
compute_syn_coef = function(heroID1, heroID2){
  rn1 = paste('R', heroID1, sep = ''); cn1 = paste('C', heroID2, sep = '')
  rn2 = paste('R', heroID2, sep = ''); cn2 = paste('C', heroID1, sep = '')
  syn_total_win = synergy[rn1, cn1] + synergy[rn2, cn2]
  syn_total = total_teammate[rn1, cn1] + total_teammate[rn2, cn2]
  syn_winrate = syn_total_win / syn_total
  return (c(syn_winrate, syn_total))
}

compute_sup_coef = function(heroID1, heroID2){
  rn1 = paste('R', heroID1, sep = ''); cn1 = paste('C', heroID2, sep = '')
  rn2 = paste('R', heroID2, sep = ''); cn2 = paste('C', heroID1, sep = '')
  sup_total_win = suppression_rowHeroWin[rn1, cn1]
  sup_total = total_enemy[rn1, cn1] + total_enemy[rn2, cn2]
  sup_winrate = sup_total_win / sup_total
  return (c(sup_winrate, sup_total))
}

# Precompute syn_coef_df & sup_coef_df for all heros pairs, which are the 'edge.csv'; 
# Thus we can build up 2 graphs representing synergy & suppression relations
hero_pair = expand.grid(as.numeric(names(table(train_basic$hero_id))), as.numeric(names(table(train_basic$hero_id)))) # pairwise hero_id for ALL heroes
colnames(hero_pair) = c('hero1', 'hero2')
all_syn_coef = t(mapply(compute_syn_coef, hero_pair$hero1, hero_pair$hero2))
all_sup_coef = t(mapply(compute_sup_coef, hero_pair$hero1, hero_pair$hero2))
colnames(all_syn_coef) = c('winrate_syn', 'total_syn')
colnames(all_sup_coef) = c('winrate_sup', 'total_sup')
syn_coef_df = data.frame(hero_pair, all_syn_coef)
sup_coef_df = data.frame(hero_pair, all_sup_coef)
syn_coef_matrix_rm0 = round(subset(syn_coef_df, hero1 != 0 & hero2 != 0 & hero1 != hero2), 4) # eliminate rows when hero_id == 0 & hero_id1 == hero_id2. These rows makes no sense
sup_coef_matrix_rm0 = round(subset(sup_coef_df, hero1 != 0 & hero2 != 0 & hero1 != hero2), 4)
# Note that: subset(syn_coef_matrix_rm0, hero_id1 == a & hero_id2 == b, select = winrate) == 
#            subset(syn_coef_matrix_rm0, hero_id1 == b & hero_id2 == a, select = winrate);
#            subset(sup_coef_matrix_rm0, hero_id1 == a & hero_id2 == b, select = winrate) + 
#            subset(sup_coef_matrix_rm0, hero_id1 == b & hero_id2 == a, select = winrate) == 1;
write.csv(syn_coef_matrix_rm0, 'systemG_syn_edges.csv', row.names = F)
write.csv(sup_coef_matrix_rm0, 'systemG_sup_edges.csv', row.names = F)

# Generate 'node.csv' 
hero_names = read.csv('hero_names.csv', header = T, as.is = T)
test_labels = read.csv('test_labels.csv', header = T)
test_players = read.csv('test_player.csv', header = T)
compute.win.eachplayer = function(rw) { return (rep(c(as.numeric(rw == 1), as.numeric(rw == 0)), each = 5, times = 1)) } 
test_basic = data.frame(test_players[,c('match_id', 'account_id', 'hero_id')], 
                        Win = as.vector(mapply(compute.win.eachplayer, test_labels$radiant_win))) # add win column into test_basic
node = data.frame(avg_last_hits = round(tapply(train_basic$last_hits, train_basic$hero_id, mean), 4))
node$hero_id = as.numeric(row.names(node))
node$avg_last_hits = as.numeric(node$avg_last_hits)
get.hero.name = function(id) {return (as.character(subset(hero_names, hero_id == id, select = localized_name))) }
node$hero_name = mapply(get.hero.name, node$hero_id)
total_win_train = tapply(train_basic$Win, train_basic$hero_id, sum)
total_win_test = tapply(test_basic$Win, test_basic$hero_id, sum)
total_win = total_win_train + total_win_test
total_game = table(train_basic$hero_id) + table(test_basic$hero_id)
node$win_rate = as.numeric(round(total_win/total_game, 4))
node$total_game = as.numeric(total_game)
node = node[-1, c(2,3,4,5,1)]
write.csv(node, 'systemG_heroes_nodes.csv', row.names = F)

## Interface to get syn/sup winrate or total.
get.syn_coef = function(heroID1, heroID2){
  return ( subset(syn_coef_matrix_rm0, hero1 == heroID1 & hero2 == heroID2, select = c('winrate', 'total')) )
}
get.sup_coef = function(heroID1, heroID2){
  return ( subset(sup_coef_matrix_rm0, hero1 == heroID1 & hero2 == heroID2, select = c('winrate', 'total')) )
}
