teams <- 
  tibble(tid = 1:64) %>%
  mutate(beta = rnorm(nrow(.)))

# teams: 64 32 16 8 4 2 1
# games: 32 16  8 4 2 1

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
  
  G <- nrow(teams) / 2
  wid <- matrix(0, nrow = G, ncol = 6)

  for (r in 1:6) {
    
    if (r == 1) {
      
      gid <- tid
      G <- nrow(teams) / 2
      
    } else {
      
      G <- 2^(6-r)
      winners <- wid[,r-1]
      gid <- matrix(nrow = G, ncol = 2)
      gid[,1] <- winners[which(1:(2*G) %% 2 == 1)]
      gid[,2] <- winners[which(1:(2*G) %% 2 == 0)]
      
    }
    
    for (g in 1:G) {
      
      p <- softmax(beta[gid[g,]])
      if (wid[g,r] == 0) {
        wid[g,r] <- sample(gid[g,], 1, prob = p)
      }
      
    }
  }
  
  for (t in 1:nrow(teams)) {
    for (r in 1:6) {
      if (t %in% wid[,r]) {
        p_advance[t,r] <- p_advance[t,r] + 1
      }
    }
  }
  
}

p_advance <- p_advance/1e4
