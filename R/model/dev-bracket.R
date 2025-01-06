teams <- 
  tibble(tid = 1:64) %>%
  mutate(beta = rnorm(nrow(.)))

teams <- 
  teams %>%
  bind_cols(game = sort(rep(1:32, 2))) %>%
  mutate(location = if_else(tid %% 2 == 1, "home", "away"))

beta <- teams$beta

tid <- 
  teams %>%
  select(-beta) %>%
  pivot_wider(names_from = location,
              values_from = tid) %>%
  select(home, away) %>%
  as.matrix()

p_advance <- matrix(0, nrow = nrow(teams), ncol = 6)

for(s in 1:1e4) {
  
  wid <- matrix(0, nrow = 64, ncol = 7)
  wid[,1] <- 1:64

  for (r in 2:7) {
    
    G <- 2^(7-r)
    winners <- wid[,r-1]
    gid <- matrix(nrow = G, ncol = 2)
    gid[,1] <- winners[which(1:(2*G) %% 2 == 1)]
    gid[,2] <- winners[which(1:(2*G) %% 2 == 0)]
    
    for (g in 1:G) {
      
      p <- softmax(beta[gid[g,]])
      if (wid[g,r] == 0) {
        wid[g,r] <- sample(gid[g,], 1, prob = p)
      }
      
    }
  }
  
  for (t in 1:nrow(teams)) {
    for (r in 2:7) {
      if (t %in% wid[,r]) {
        p_advance[t,r-1] <- p_advance[t,r-1] + 1
      }
    }
  }
  
}

p_advance <- p_advance/1e4

p_advance %>% 
  as_tibble() %>% 
  rowid_to_column("team") %>% 
  pivot_longer(starts_with("V"), 
               names_to = "round", 
               values_to = "p") %>% 
  mutate(round = parse_number(round)) %>% 
  group_by(team) %>% 
  ggplot(aes(x = round, 
             y = p, 
             group = team)) + 
  geom_line(alpha = 0.25)



