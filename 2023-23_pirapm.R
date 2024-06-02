library(tidyverse)
library(bigballR)
library(Matrix)
teams <- read_csv("C:/Users/ryanjobates/Downloads/teams.csv") #D1 Teams
teams_2022_23 <- subset(teams, Season == "2023-24")
all_rosters <- read_csv("C:/Users/ryanjobates/Downloads/rosters_2023_24.csv") #Rosters for each team

#Imputing missing player heights
g = all_rosters |>
  filter(Pos == "G")
g1 = mean(g$HtInches, na.rm = TRUE)
f = all_rosters |>
  filter(Pos == "F")
f1 = mean(f$HtInches, na.rm = TRUE)
c = all_rosters |>
  filter(Pos == "C")
c1 = mean(c$HtInches, na.rm = TRUE)
all_rosters = all_rosters |>
  mutate(Pos = ifelse(is.na(Pos), "F", Pos)) |>
  mutate(HtInches = ifelse(is.na(HtInches) & Pos == "G", g1, HtInches)) |>
  mutate(HtInches = ifelse(is.na(HtInches) & Pos == "F", f1, HtInches)) |>
  mutate(HtInches = ifelse(is.na(HtInches) & Pos == "C", c1, HtInches))

#2023-24 PBP
pbp =  read_csv("C:/Users/ryanjobates/Downloads/2023_24_pbp.csv", 
                col_types = cols(ID = col_character(), 
                                 Time = col_character(), Game_Time = col_character()))
player_stats = get_player_stats(pbp, multi.games = TRUE) 
player_stats = player_stats |>
  left_join(all_rosters, by = c("Player","Team"))
possessions = get_possessions(pbp) #parsing pbp into possesions
possessions = possessions |>
  filter(Home %in% teams & Away %in% teams)

#calculating team ratings
ratings = function(y) {
  #y = y |>
  # group_by(ID, Poss_Team) |>
  # summarise(Home = Home, Away = Away, Score = sum(PTS), Poss = n()) |>
  # distinct(Poss_Team, .keep_all = TRUE) |>
  # mutate(Orating = (Score/Poss)*100)
  teams <- unique(c(y$Home, y$Away), na.rm = TRUE)
  cols <- c(paste0(teams,"_O"), paste0(teams, "_D"))
  
  
  row_id <- rep(1:nrow(y), each=2)
  len <- length(teams)
  
  home <- apply(y[,3, drop = F], 2, function(x){
    match(x, teams)+ifelse(y$Home==y$Poss_Team, 0, len)
  })
  away <- apply(y[,4, drop = F], 2, function(x){
    match(x, teams)+ifelse(y$Away==y$Poss_Team, 0, len)
  })
  result <- cbind(home, away)
  col_id <- as.vector(t(result))
  
  row_id_filter <- row_id[which(!is.na(col_id))]
  col_id_filter <- col_id[which(!is.na(col_id))]
  
  
  row_id_filter <- c(row_id_filter, which(y$Home == y$Poss_Team))
  col_id_filter <- c(col_id_filter, rep(max(col_id_filter)+1, length(which(y$Home == y$Poss_Team))))
  
  sparse_mat <- sparseMatrix(i=row_id_filter, j = col_id_filter)
  
  z <- y$PTS * 100
  
  cv_model <- glmnet::cv.glmnet(x = sparse_mat, ##ncol = 1058
                                y = z,
                                alpha = 0, 
                                lambda = seq(0,.2, .001),
                                standardize = FALSE)
  plot(cv_model)
  lam <- cv_model$lambda.min ## best lambda
  
  ## this is the model refit using that lambda
  coef_model <- glmnet::glmnet(x = sparse_mat, 
                               y = z,
                               alpha = 0, 
                               standardize = FALSE,
                               lambda = lam
                               
  )
  
  coefs <- coef_model$beta
  c(max(coefs), min(coefs))
  
  team_rapm <- data.frame(
    Team = teams,
    ORAPM = coefs[1:length(teams)],
    DRAPM = -coefs[(length(teams)+1):(length(teams)*2)]
  ) %>%
    mutate(
      Ortg = ORAPM,
      Drtg = DRAPM,
      Netrtg = ORAPM + DRAPM
    )
  
  return(team_rapm)
}
team_ratings = ratings(possessions)
player_stats = player_stats |>
  left_join(team_ratings, by = "Team")
priors <- player_stats %>% #Calculating player priors
  mutate(pp_100 = ifelse(oPOSS != 0, ((PTS/oPOSS)*100)-((sum(PTS)/sum(oPOSS))*100)+19.11653, 0),
         orb_100 = ifelse(oPOSS != 0, ((ORB/oPOSS)*100)-((sum(ORB)/sum(oPOSS))*100)+2.39773, 0),
         drb_100 = ifelse(oPOSS != 0, ((DRB/oPOSS)*100)-((sum(DRB)/sum(oPOSS))*100)+6.159547, 0),
         ast_100 = ifelse(oPOSS != 0, ((AST/oPOSS)*100)-((sum(AST)/sum(oPOSS))*100)+3.500493, 0),
         stl_100 = ifelse(oPOSS != 0, ((STL/oPOSS)*100)-((sum(STL)/sum(oPOSS))*100)+1.63356, 0),
         blk_100 = ifelse(oPOSS != 0, ((BLK/oPOSS)*100)-((sum(BLK)/sum(oPOSS))*100)+0.9256608, 0),
         tov_100 = ifelse(oPOSS != 0, ((TOV/oPOSS)*100)-((sum(TOV)/sum(oPOSS))*100)+3.323674, 0),
         pf_100 = ifelse(oPOSS != 0, ((PF/oPOSS)*100)-((sum(PF)/sum(oPOSS))*100)+5.082373, 0),
         rima_100 = ifelse(oPOSS != 0, (((RIMA)/oPOSS)*100)-((sum(RIMA)/sum(oPOSS))*100)+5.233876, 0),
         tpa_100 = ifelse(oPOSS != 0, ((TPA/oPOSS)*100)-((sum(TPA)/sum(oPOSS))*100)+5.359009, 0),
         fta_100 = ifelse(oPOSS != 0, ((FTA/oPOSS)*100)-((sum(FTA)/sum(oPOSS))*100)+5.522203, 0),
         MPG = ifelse(GP != 0, (MINS/(GP)), 0),
         mida = ifelse(oPOSS != 0, ((MIDA/oPOSS)*100)-((sum(MIDA)/sum(oPOSS))*100)+4.617823, 0),
         twopa_100 = ifelse(oPOSS != 0, ((FGA/oPOSS)*100)-tpa_100, 0),
         gsper = ifelse(GS<=GP, (GS/GP)^2, 1),
         Oprior = ifelse(MINS >= 200,
                         (-0.28962443*mida)+(-0.35633250*rima_100)+(-0.23184412*tpa_100)+(-0.09630575*fta_100)+(0.43472087*pp_100)+(0.25792662*orb_100)+(.0*drb_100)+(0.59222615*ast_100)+(-0.57123956*tov_100)+(-0.2557139*pf_100)+(0.08103536*HtInches)-8.698411 , 
                         ((MINS/200)*((-0.28962443*mida)+(-0.35633250*rima_100)+(-0.23184412*tpa_100)+(-0.09630575*fta_100)+(0.43472087*pp_100)+(0.25792662*orb_100)+(.0*drb_100)+(0.59222615*ast_100)+(-0.57123956*tov_100)+(-0.2557139*pf_100)+(0.08103536*HtInches)-8.698411))+(1-(MINS/200))*(-5.353843 + (0.161660*Ortg) + (0.225311*MPG))),
         Dprior = ifelse(MINS >= 200,(-0.009619549*mida)+(-0.098136361*rima_100)+(0.001914766*tpa_100)+(0.104272323*fta_100)+(-.04*pp_100)+(0.101923361*orb_100)+(0.082965074*drb_100)+(0.316151644*ast_100)+(-0.393467242*tov_100)+(0.623944175*stl_100)+(0.309028086*blk_100)+(0.030040656*pf_100)+(0.131946380*HtInches)-11.80907 , 
                         ((MINS/200)*((-0.009619549*mida)+(-0.098136361*rima_100)+(0.001914766*tpa_100)+(0.104272323*fta_100)+(-.04*pp_100)+(0.101923361*orb_100)+(0.082965074*drb_100)+(0.316151644*ast_100)+(-0.393467242*tov_100)+(0.623944175*stl_100)+(0.309028086*blk_100)+(0.030040656*pf_100)+(0.131946380*HtInches)-11.80907)) +
                           ((1-(MINS/200))*(-0.5719127+(0.0286545*MPG)+(0.1049781*Drtg)))))
priors = priors |>
  mutate(Player2 = paste0(Player,"_",Team))
possessions = possessions |>
  group_by(ID) |>
  mutate(HomePTS = ifelse(Poss_Team == Home, PTS, 0)) |>
  mutate(AwayPTS = ifelse(Poss_Team == Away, PTS, 0)) |>
  mutate(HomeScore = lag(cumsum(HomePTS), default = 0)) |>
  mutate(AwayScore = lag(cumsum(AwayPTS), default = 0)) |>
  ungroup() |>
  mutate(margin = ifelse(Poss_Team == Home, HomeScore - AwayScore, AwayScore - HomeScore))
possessions = possessions |>
  mutate(PTS = ifelse(margin <= -3 & margin >= -16, PTS  - ((-2.91790 + (margin * -0.70212))/100), PTS)) |>
  mutate(PTS = ifelse(margin == 0, PTS - (-5.83/100), PTS)) |>
  mutate(PTS = ifelse(margin < -16, PTS - (7.148/100), PTS)) |>
  mutate(PTS = ifelse(margin == -2 | margin == -1 | margin == 1 | margin == 2 | margin == 3, PTS - (-2.31611/100), PTS)) |>
  mutate(PTS = ifelse(margin > 3, PTS - (-1.664496/100), PTS))
possessions = possessions |>
  mutate(Home.1 = paste0(Home.1,"_",Home),
         Home.2 = paste0(Home.2,"_",Home),
         Home.3 = paste0(Home.3,"_",Home),
         Home.4 = paste0(Home.4,"_",Home),
         Home.5 = paste0(Home.5,"_",Home),
         Away.1 = paste0(Away.1,"_",Away),
         Away.2 = paste0(Away.2,"_",Away),
         Away.3 = paste0(Away.3,"_",Away),
         Away.4 = paste0(Away.4,"_",Away),
         Away.5 = paste0(Away.5,"_",Away))
#adding priors into possessions
homeprior = ifelse(
  possessions$Poss_Team == possessions$Home,
  priors$Oprior[match(possessions$Home.1, priors$Player2)] +
    priors$Oprior[match(possessions$Home.2, priors$Player2)] +
    priors$Oprior[match(possessions$Home.3, priors$Player2)] +
    priors$Oprior[match(possessions$Home.4, priors$Player2)] +
    priors$Oprior[match(possessions$Home.5, priors$Player2)],
  priors$Dprior[match(possessions$Home.1, priors$Player2)] +
    priors$Dprior[match(possessions$Home.2, priors$Player2)] +
    priors$Dprior[match(possessions$Home.3, priors$Player2)] +
    priors$Dprior[match(possessions$Home.4, priors$Player2)] +
    priors$Dprior[match(possessions$Home.5, priors$Player2)]
)
homeprior <- ifelse(is.na(homeprior), 0, homeprior)

awayprior = ifelse(
  possessions$Poss_Team == possessions$Away,
  priors$Oprior[match(possessions$Away.1, priors$Player2)] +
    priors$Oprior[match(possessions$Away.2, priors$Player2)] +
    priors$Oprior[match(possessions$Away.3, priors$Player2)] +
    priors$Oprior[match(possessions$Away.4, priors$Player2)] +
    priors$Oprior[match(possessions$Away.5, priors$Player2)],
  priors$Dprior[match(possessions$Away.1, priors$Player2)] +
    priors$Dprior[match(possessions$Away.2, priors$Player2)] +
    priors$Dprior[match(possessions$Away.3, priors$Player2)] +
    priors$Dprior[match(possessions$Away.4, priors$Player2)] +
    priors$Dprior[match(possessions$Away.5, priors$Player2)]
)
awayprior <- ifelse(is.na(awayprior), 0, awayprior)

exp_pts <- ifelse(possessions$Poss_Team == possessions$Home | is.na(possessions$Poss_Team),
                  homeprior - awayprior,
                  awayprior - homeprior
)

#creating new response vector (actual points - expected points from priors)
y_prior = (possessions$PTS*100) - exp_pts
# Create a vector of player names
players <- unique(c(possessions$Home.1, possessions$Home.2, possessions$Home.3, possessions$Home.4, possessions$Home.5, possessions$Away.1, possessions$Away.2, possessions$Away.3, possessions$Away.4, possessions$Away.5), na.rm = TRUE)
cols <- c(paste0(players,"_O"), paste0(players, "_D"))

# Store possessions in sparse matrix format
row_id <- rep(1:nrow(possessions), each=10)
len <- length(players)

home <- apply(possessions[,8:12], 2, function(x){
  match(x, players)+ifelse(possessions$Home==possessions$Poss_Team, 0, len)
})
away <- apply(possessions[,13:17], 2, function(x){
  match(x, players)+ifelse(possessions$Away==possessions$Poss_Team, 0, len)
})
result <- cbind(home, away)
col_id <- as.vector(t(result))

row_id_filter <- row_id[which(!is.na(col_id))]
col_id_filter <- col_id[which(!is.na(col_id))]

# Adding HCA
row_id_filter <- c(row_id_filter, which(possessions$Home == possessions$Poss_Team))
col_id_filter <- c(col_id_filter, rep(max(col_id_filter)+1, length(which(possessions$Home == possessions$Poss_Team))))

sparse_mat <- sparseMatrix(i=row_id_filter, j = col_id_filter)
pen = c(rep(1, length(players)), rep(.41, length(players)), 0.5)

cv_model <- glmnet::cv.glmnet(x = sparse_mat, ##ncol = 1058
                              y = y_prior,
                              alpha = 0, 
                              standardize = FALSE)
plot(cv_model)
lam <- cv_model$lambda.min ## best lambda

## this is the model refit using that lambda
coef_model <- glmnet::glmnet(x = sparse_mat, 
                             y = y_prior,
                             alpha = 0, 
                             standardize = FALSE,
                             lambda = lam)

coefs <- coef_model$beta
c(max(coefs), min(coefs))

player_rapm <- data.frame(
  Player2 = players,
  Name = gsub("_(.*)", "", players),
  Team = gsub("(.*)_", "", players),
  ORPM = coefs[1:length(players)],
  DRPM = -coefs[(length(players)+1):(length(players)*2)]
) %>%
  mutate(
    RPM = ORPM + DRPM
    
  ) %>%
  left_join(priors, by = "Player2") %>% # Add back prior to new RAPM estimate
  mutate(
    ORAPM_prior = round(ORPM+Oprior,1),
    DRAPM_prior = round(DRPM+Dprior,1),
    RAPM_prior = round(ORAPM_prior+DRAPM_prior,1)
  ) |>
  drop_na()

player_rapm = player_rapm |>
  mutate(PTS = round(ifelse(oPOSS == 0, 0, (PTS/oPOSS)*60), 1),
         ORB = round(ifelse(oPOSS == 0, 0, (ORB/oPOSS)*60),1),
         DRB = round(ifelse(oPOSS == 0, 0, (DRB/oPOSS)*60),1),
         AST = round(ifelse(oPOSS == 0, 0, (AST/oPOSS)*60),1),
         STL = round(ifelse(oPOSS == 0, 0, (STL/oPOSS)*60),1),
         BLK = round(ifelse(oPOSS == 0, 0, (BLK/oPOSS)*60),1),
         TOV = round(ifelse(oPOSS == 0, 0, (TOV/oPOSS)*60),1),
         PF = round(ifelse(oPOSS == 0, 0, (PF/oPOSS)*60),1),
         RIMA = round(ifelse(oPOSS == 0, 0, (RIMA/oPOSS)*60),1),
         MIDA = round(ifelse(oPOSS == 0, 0, (MIDA/oPOSS)*60),1),
         TPA = round(ifelse(oPOSS == 0, 0, (TPA/oPOSS)*60),1),
         FTA = round(ifelse(oPOSS == 0, 0, (FTA/oPOSS)*60),1))


player_ppm = player_rapm |> #final ratings with stats
  select(CleanName, Team.x,Ht,  GP, MINS, ORAPM_prior, DRAPM_prior, RAPM_prior,
         PTS, ORB, DRB, AST, STL, BLK, TOV, PF, RIMA, RIM., MIDA, MID., TPA, TP., FTA, FT., TS., eFG.) |>
  rename(Name = CleanName, Team = Team.x) |>
  arrange(desc(RAPM_prior)) |>
  rename(OPPM = ORAPM_prior, DPPM = DRAPM_prior, PPM = RAPM_prior)