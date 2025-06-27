rm(list = ls())
library(nflreadr)
library(tidyverse)
library(igraph)
library(knitr)
#------------------Load and set up dataset---------------------------

#load trades
df <- load_trades(2017:2022) %>% distinct(trade_id,.keep_all = TRUE) %>%
  mutate(gave = ifelse(gave == "OAK","LV",gave),
         received = ifelse(received == "OAK","LV",received))

#load team information
teams <- load_teams() %>% select(team_abbr,team_conf,team_division)

#add team info with prefixes, classify as same-division + same-conference
df <- df %>% inner_join(teams,by = join_by(gave == team_abbr)) %>%
  rename_with(~ paste0("gave_", .x, recycle0 = TRUE),
              starts_with("team")) %>%
  inner_join(teams,by = join_by(received == team_abbr)) %>%
  rename_with(~ paste0("received_", .x, recycle0 = TRUE),
              starts_with("team")) %>%
  mutate(Same_Conf = ifelse(gave_team_conf==received_team_conf,1,0),
         Same_Div = ifelse(gave_team_division==received_team_division,1,0),
         Same_Conf_Dif_Div = ifelse(Same_Conf == 1 & Same_Div == 0,1,0))

#compute observed values
Obs_Same_Conf <- sum(df$Same_Conf)
Obs_Same_Div <- sum(df$Same_Div)
Obs_Same_Conf_Dif_Div <- sum(df$Same_Conf_Dif_Div)

#----------------Bootstrapping Simulation---------------------

#simulated value vectors
n_sims <- 1000
Same_Conf_Sims <- numeric(n_sims)
Same_Div_Sims <- numeric(n_sims)
Same_Conf_Dif_Div_Sims <- numeric(n_sims)

#run simulation
for(i in 1:n_sims){
  #shuffle indices for both teams in trade
  rand_indices_gave <- sample(nrow(df))
  rand_indices_received <- sample(nrow(df))
  
  #temporary dataset with shuffled conferences
  df_sim <- df
  df_sim[,c("gave_team_conf",
            "gave_team_division")] <- df[rand_indices_gave,c("gave_team_conf",
                                                             "gave_team_division")]
  df_sim[,c("received_team_conf",
            "received_team_division")] <- df[rand_indices_received,c("received_team_conf",
                                                             "received_team_division")]
  
  #classify trades
  df_sim <- df_sim %>% 
    mutate(Same_Conf = ifelse(gave_team_conf==received_team_conf,1,0),
          Same_Div = ifelse(gave_team_division==received_team_division,1,0),
          Same_Conf_Dif_Div = ifelse(Same_Conf == 1 & Same_Div == 0,1,0))
  
  #record numbers of trades
  Same_Conf_Sims[i] <- sum(df_sim$Same_Conf)
  Same_Div_Sims[i] <- sum(df_sim$Same_Div)
  Same_Conf_Dif_Div_Sims[i] <- sum(df_sim$Same_Conf_Dif_Div)
}

#--------------Inference-----------------------

#compute mean, sd, Z-score for Same-Conference Trades
mean_same_conf <- mean(Same_Conf_Sims)
sd_same_conf <- sd(Same_Conf_Sims)
Z_same_conf <- (Obs_Same_Conf - mean_same_conf)/sd_same_conf
p_same_conf <- pnorm(Z_same_conf)

#compute mean, sd, Z-score for Same-Division Trades
mean_same_div <- mean(Same_Div_Sims)
sd_same_div <- sd(Same_Div_Sims)
Z_same_div <- (Obs_Same_Div - mean_same_div)/sd_same_div
p_same_div <- pnorm(Z_same_div)

#compute mean, sd, Z-score for Same-Conference-Different-Division Trades
mean_same_conf_dif_div <- mean(Same_Conf_Dif_Div_Sims)
sd_same_conf_dif_div <- sd(Same_Conf_Dif_Div_Sims)
Z_same_conf_dif_div <- (Obs_Same_Conf_Dif_Div - mean_same_conf_dif_div)/sd_same_conf_dif_div
p_same_conf_dif_div <- pnorm(-Z_same_conf_dif_div)

#------------Full League plot---------------------------------

#convert to graph
g <- graph_from_data_frame(df[,c("gave","received")],directed = F)

#add edge weight and add up for each pair
E(g)$weight <- 1
g <- simplify(g,edge.attr.comb = "sum")

#add team information
indices <- match(V(g)$name,teams$team_abbr)
V(g)$division <- teams$team_division[indices]
V(g)$division <- word(V(g)$division,2)
V(g)$conference <- teams$team_conf[indices]
V(g)$shape <- ifelse(V(g)$conference == "NFC","square","circle")
V(g)$color <- case_when(V(g)$division == "West" ~ "red",
                        V(g)$division == "East" ~ "blue",
                        V(g)$division == "North" ~ "green",
                        V(g)$division == "South" ~ "orange")

#plot
E(g)$width <- .5 * E(g)$weight
l1 <- layout_with_kk(g)
plot(g,vertex.label = NA,layout = l1)
legend("topright",
       legend = c("NFC","AFC"),
       pch = c(0,1),
       title = "Conference",bty = "n")
legend("topright",
       legend = c("North","South","East","West"),
       inset = c(0,.2),
       fill = c("green","orange","blue","red"),
       title = "Division",bty = "n")
title("NFL Trade Network (2017-2022)")

#----------Flexible Backbone Pruning visual-------------

#get two-way edgelist
df2 <- as_data_frame(g)
df2_opp <- df2 %>% rename(Team_y= from,Team_x =to)
df2 <- df2 %>% rename(Team_x = from, Team_y = to)
df2 <- rbind(df2,df2_opp)

#summarize number of trades for each team
df_summs <- df2 %>% group_by(Team_x) %>%
  summarise(Mean_Trades = mean(weight),
            SD_Trades = sd(weight))%>%
  rename(Team = Team_x)

#add summaries, compute Z-scores
df2 <- inner_join(df2,df_summs,
                            by = join_by(Team_x==Team)) %>%
  inner_join(df_summs,by = join_by(Team_y==Team)) %>%
  mutate(Z_x = (weight - Mean_Trades.x)/SD_Trades.x,
         Z_y = (weight - Mean_Trades.y)/SD_Trades.y)

#filter based on Z-scores
df2 <- df2 %>% filter(Z_x > 1 | Z_y > 1)

#turn into graph, remove double edges
g_pruned <- graph_from_data_frame(df2,
                                  directed = F)
g_pruned <-simplify(g_pruned,edge.attr.comb = "first")

#add team information
indices <- match(V(g_pruned)$name,teams$team_abbr)
V(g_pruned)$division <- teams$team_division[indices]
V(g_pruned)$division <- word(V(g_pruned)$division,2)
V(g_pruned)$conference <- teams$team_conf[indices]
V(g_pruned)$shape <- ifelse(V(g_pruned)$conference == "NFC","square","circle")
V(g_pruned)$color <- case_when(V(g_pruned)$division == "West" ~ "red",
                        V(g_pruned)$division == "East" ~ "blue",
                        V(g_pruned)$division == "North" ~ "green",
                        V(g_pruned)$division == "South" ~ "orange")

#plot
l2 <- layout_with_kk(g_pruned,weights = sqrt(E(g_pruned)$weight))
E(g_pruned)$width <-.5* E(g_pruned)$weight
plot(g_pruned,vertex.label = NA,layout = l2)
legend("topright",
       legend = c("NFC","AFC"),
       pch = c(0,1),
       title = "Conference",bty = "n")
legend("topright",
       legend = c("North","South","East","West"),
       inset = c(0,.2),
       fill = c("green","orange","blue","red"),
       title = "Division",bty = "n")
title("NFL Trading Network (2017-2022, Pruned)")

#------------------markdown table for results-------------
df_res <- data.frame(
  v1 = c("Same-division","Same-conference",
         "Same-conference but different-division"),
  v2 = c(Obs_Same_Div,Obs_Same_Conf,Obs_Same_Conf_Dif_Div),
  v3 = c(mean_same_div,mean_same_conf,mean_same_conf_dif_div),
  v4 = c(sd_same_div,sd_same_conf,sd_same_conf_dif_div),
  v5 = c(Z_same_div,Z_same_conf,Z_same_conf_dif_div),
  v6 = c(p_same_div,p_same_conf,p_same_conf_dif_div)
)
colnames(df_res) <- c("Trade classification",
                         "Observed value",
                         "Simulation mean",
                         "Simulation SD",
                         "Z-score",
                          "p-value")
                        
kable(df_res,"pipe",digits = 3)


