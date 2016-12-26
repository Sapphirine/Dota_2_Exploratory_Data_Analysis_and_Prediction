# Dota2_-Exploratory_Data_Analysis_Prediction

Dataset: downloaded from https://www.kaggle.com/devinanzelmo/dota-2-matches, including hero_names.csv, player_ratings.csv, players.csv, test_play.csv, test_labels.csv, match.csv.

Codes:

Data Cleaning.R attaches label indicating win or lost to the player data.

HeroRecommendation APP.R, Player's Performance APP.R, Prediction App.R integrate hero recommendation, player's performance evaluation and match outcome prediction into a single shiny useful app.

PredictionModelTrain.R trains a model for match outcome prediction in the above app, while the resulting model is stored as prediction_logit.rds in the repo.

SynergySuppressionMatrixCompute.R is used to compute the node file and edge files for the IBM System G Visualizer to run.

Demo Videos:

Youtube websites: https://www.youtube.com/watch?v=TJCH4NnWR1I
