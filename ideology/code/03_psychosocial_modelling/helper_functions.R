##### Helper Functions
recover_divergence_from_neutral <- function(cat, r){
  temp_2 <- data_long %>% filter(category == cat)
  resolution <- 1000
  Z <- rep(0,resolution-1)
  if(substr(cat,1,4) == 'bias'){
    i<- floor(-1*resolution*min(temp_2$sentiment) / (max(temp_2$sentiment) - min(temp_2$sentiment)))
    Z[i] <- 1.0
  }else{
    Z[1] <- 1.0 
  }
  # print(min(temp_2$sentiment))
  # print(max(temp_2$sentiment))
  A <- (temp_2)$sentiment
  h_A <- hist(A, breaks=seq(min(temp_2$sentiment),max(temp_2$sentiment),length.out=resolution),freq=FALSE, plot=F)
  A_proportions <- h_A$counts
  
  
  N <- (temp_2 %>% filter(label == 'N'))$sentiment
  # print(length(N))
  h_N <- hist(N, breaks=seq(min(temp_2$sentiment),max(temp_2$sentiment),length.out=resolution),freq=FALSE, plot=F)
  N_proportions <- h_N$counts
  
  L <- (temp_2 %>% filter(label == 'L'))$sentiment
  # print(length(L))
  h_L <- hist(L, breaks=seq(min(temp_2$sentiment),max(temp_2$sentiment),length.out=resolution),freq=FALSE, plot=F)
  L_proportions <- h_L$counts
  
  R <- (temp_2 %>% filter(label == 'R'))$sentiment
  # print(length(R))
  h_R <- hist(R, breaks=seq(min(temp_2$sentiment),max(temp_2$sentiment),length.out=resolution),freq=FALSE, plot=F)
  R_proportions <- h_R$counts
  
  FR <- (temp_2 %>% filter(label == 'FR'))$sentiment
  # print(length(FR))
  h_FR <- hist(FR, breaks=seq(min(temp_2$sentiment),max(temp_2$sentiment),length.out=resolution),freq=FALSE, plot=F)
  FR_proportions <- h_FR$counts
  
  A <- mean((temp_2)$sentiment)
  N <- mean((temp_2 %>% filter(label == 'N'))$sentiment)
  L <- mean((temp_2 %>% filter(label == 'L'))$sentiment)
  R <- mean((temp_2 %>% filter(label == 'R'))$sentiment)
  FR <- mean((temp_2 %>% filter(label == 'FR'))$sentiment)
  
  if(sum(R_proportions) == 0){
    R_proportions <-  N_proportions
  }
  if(sum(FR_proportions) == 0){
    FR_proportions <-  N_proportions
  }
  # print(sum(FR_proportions))
  
  c(A = sign(A-N)*as.numeric(KL(rbind(N_proportions/sum(N_proportions), A_proportions/sum(A_proportions)))),
    N = sign(N-N)*as.numeric(KL(rbind(N_proportions/sum(N_proportions), N_proportions/sum(N_proportions)))),
    L = sign(L-N)*as.numeric(KL(rbind(N_proportions/sum(N_proportions), L_proportions/sum(L_proportions)))),
    R = sign(R-N)*as.numeric(KL(rbind(N_proportions/sum(N_proportions), R_proportions/sum(R_proportions)))),
    FR= sign(FR-N)*as.numeric(KL(rbind(N_proportions/sum(N_proportions), FR_proportions/sum(FR_proportions)))) )
}
recover_mean_diff_from_neutral <- function(cat){
  temp_2 <- data_long %>% 
    filter(category == cat) %>%
    mutate(label = as.factor(label))
  
  A_vec <- ((temp_2)$sentiment)
  N_vec <- ((temp_2 %>% filter(label == 'N'))$sentiment)
  L_vec <- ((temp_2 %>% filter(label == 'L'))$sentiment)
  R_vec <- ((temp_2 %>% filter(label == 'R'))$sentiment)
  FR_vec <- ((temp_2 %>% filter(label == 'FR'))$sentiment)
  
  if(length(R_vec) == 0){
    R_vec <-  N_vec
  }
  
  A <- mean(A_vec)
  N <- mean(N_vec)
  L <- mean(L_vec)
  R <- mean(R_vec)
  FR <- mean(FR_vec)
  
  c(A = A-N,
    N = N-N,
    L = L-N,
    R = R-N,
    FR= FR-N)
}

recover_pairwise_significances <- function(cat){
  temp_2 <- data_long %>% 
    filter(category == cat) %>%
    mutate(label = as.factor(label))
  res <- compare_means(sentiment~label, data=temp_2, method = 'wilcox.test')
  res$kruskal.p.adj <- compare_means(sentiment~label, data=temp_2, method = 'kruskal.test')$p.adj
  res$kruskal.p.signif <- compare_means(sentiment~label, data=temp_2, method = 'kruskal.test')$p.signif
  res$category <- cat
  res$dataset <- dataset
  res
}