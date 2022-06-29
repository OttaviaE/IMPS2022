IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}
i_info <- function(b, a=1,c=0, theta = seq(-5,5,length.out=1000)){
  
  
  P <- NULL 
  Q <- NULL
  Ii <- NULL
  
  for(i in 1:1000){
    P[i] <- 1/(1+ exp (-a*(theta[i] - b)))
    Q[i]= 1-P[i]
    Ii[i] =(a*Q[i]*(P[i]-c)^2)/(P[i]*((1-c)^2)) # (3PL)
  }
  return(Ii)
}


# Function to get all item information
item_info <- function(b,a=1){
  item <- NULL
  for(i in 1:length(b)){
    item[[i]] <- i_info(b[i],a[i])
  }
  return(item)
}
difficulty <- c(0,0, 0)
disc <- c(0.20, .70, 1.90)
theta <- seq(-7, 7, .001)
par(mar = c(5,7,4,2) + 0.1) 

# item a
plot(theta, IRT(theta, b=difficulty[1], a = disc[1]),
     cex.lab= 3.5, 
     cex.axis =2.5,
     xlab = expression(theta), ylab = expression(paste("P(x = 1|", theta[p], ", ", b[s], 
                                                       ", ", a[s], ")")),
     xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 4, 
     col = "royalblue")
#abline(h = 0.50, lty=2)
abline(v = -2, lty = 2, lwd = 2)
abline(v = 2, lty = 2, lwd = 2)
segments(-7, exp(disc[1] * (-2 - difficulty[1])) / (1 + exp(disc[1] * (-2 - difficulty[1]))), 
         -2,  
         exp(disc[1] * (-2 - difficulty[1])) / (1 + exp(disc[1] * (-2 - difficulty[1]))), 
         col = "royalblue", lty = 2, lwd = 2)
segments(-7, 
         exp(disc[1] * (2 - difficulty[1])) / (1 + exp(disc[1] * (2 - difficulty[1]))), 2,  
         exp(disc[1] * (2 - difficulty[1])) / (1 + exp(disc[1] * (2 - difficulty[1]))), 
         col = "royalblue", lty = 2, lwd = 2)


# text(x = difficulty[2] -3.5, y =0.45, "item a", 
#      col = "royalblue",  cex = 2)
#item B
lines(theta, IRT(theta, b = difficulty[2], a =disc[2]), lty = 4, lwd=4, 
      col = "magenta")
# text(x = difficulty[2]-1 , y =0.6, "item b", 
#      col = "magenta",  cex = 2)
segments(-7, exp(disc[2] * (-2 - difficulty[2])) / (1 + exp(disc[2] * (-2 - difficulty[2]))), 
         -2,  
         exp(disc[2] * (-2 - difficulty[2])) / (1 + exp(disc[2] * (-2 - difficulty[2]))), 
         col = "magenta", lty = 2, lwd = 2)
segments(-7, 
         exp(disc[2] * (2 - difficulty[2])) / (1 + exp(disc[2] * (2 - difficulty[2]))), 2,  
         exp(disc[2] * (2 - difficulty[2])) / (1 + exp(disc[2] * (2 - difficulty[2]))), 
         col = "magenta", lty = 2, lwd = 2)
# item C
lines(theta, IRT(theta, b = difficulty[3], a = disc[3]), lty = 2, lwd=4, col = "seagreen")
# text(x = difficulty[3]+1.5 , y =0.9, "item c", 
#    col = "seagreen",  cex = 2)
segments(-7, exp(disc[3] * (-2 - difficulty[3])) / (1 + exp(disc[3] * (-2 - difficulty[3]))), 
         -2,  
         exp(disc[3] * (-2 - difficulty[3])) / (1 + exp(disc[3] * (-2 - difficulty[3]))), 
         col = "seagreen", lty = 2, lwd = 2)
segments(-7, 
         exp(disc[3] * (2 - difficulty[3])) / (1 + exp(disc[3] * (2 - difficulty[3]))), 2,  
         exp(disc[3] * (2 - difficulty[3])) / (1 + exp(disc[3] * (2 - difficulty[]))), 
         col = "seagreen", lty = 2, lwd = 2)


b <- difficulty
a <- disc

c <- item_info(b,a)


Theta <- matrix(seq(-4,4, length.out=1000))
check <- data.frame(Theta,
                    item_info = c[[1]],
                    item_info2 = c[[2]],
                    item_info3 = c[[3]])




d1 <- do.call('cbind',c)
sum_info2 <- rowSums(d1)


plot(check$Theta, check$item_info, ylim= c(0, 0.8), cex.lab= 2,
     cex.axis =2.5,
     cex.lab = 3.5,     xlab = expression(theta), ylab = expression(paste("I(",theta, ")")),
     type = "l", lwd =4,
     col = "royalblue")
lines(check$Theta, check$item_info2, lwd =4,
      col = "magenta", lty = 4)
lines(check$Theta, check$item_info3, lwd =4,
      col = "seagreen", lty = 2)

# TIF -----

b <- difficulty
a <- disc

c <- item_info(b,a)


Theta <- matrix(seq(-4,4, length.out=1000))
check <- data.frame(Theta, 
                    item_info = c[[1]], 
                    item_info2 = c[[2]], 
                    item_info3 = c[[3]])





d <- do.call('cbind',c)
sum_info1 <- rowSums(d)




d1 <- do.call('cbind',c)
sum_info2 <- rowSums(d1)
Theta <- matrix(seq(-4,4, length.out=1000))
check <- data.frame(Theta, sum_info1, sum_info2)

plot(check$Theta, check$sum_info2, 
     type = "l", lwd =4, 
     col = "red",
     xlab = expression(theta), ylab = expression(paste("I(", theta, ")")), 
     ylim = c(0, 0.8), 
     cex.lab = 3.5, 
     cex.axis = 2.5)

text(x = 1.5, y = 0.50, "TIF", cex = 2, col = "red")  

## results -----

rm(list = ls())
load("C:/Users/huawei/OneDrive/Desktop/IRTtemp/UNIsummary.RData")
load("C:/Users/huawei/OneDrive/Desktop/IRTtemp/SKsummary.RData")
load("C:/Users/huawei/OneDrive/Desktop/IRTtemp/NORMALsummary.RData")
set.seed(666)

all_small_norm = all_data_theta[!all_data_theta$selection %in%"guidedTheta", ]
all_small_norm$distribution = "Normal"
all_small_norm$selection  = plyr::revalue(all_small_norm$selection, 
                                          c("cluster_theta" = "UIP", 
                                            "guidedNew" = "EIP", 
                                            "random" = "RP", 
                                            "smart_theta" = "BP"))
all_small_sk = all_data_sk_theta[!all_data_sk_theta$selection %in%"rangeSK", ]
all_small_sk$selection  = plyr::revalue(all_small_sk$selection, 
                                        c("clustersk" = "UIP", 
                                          "range_newSK" = "EIP", 
                                          "random" = "RP", 
                                          "smart" = "BP"))
all_small_sk$distribution = "Skewed" 
all_small_uni = all_data_uni_theta[!all_data_uni_theta$selection %in%"rangeuni", ]
all_small_uni$selection  = plyr::revalue(all_small_uni$selection, 
                                         c("clusteruni" = "UIP", 
                                           "range_newuni" = "EIP", 
                                           "random" = "RP", 
                                           "smart" = "BP"))
all_small_uni$num_item = gsub("number", "", all_small_uni$num_item)
all_small_uni$distribution = "Uniform" 

all_small = rbind(all_small_norm, all_small_sk, all_small_uni)
all_small$num_item = gsub("number", "", all_small$num_item)
dummy = data.frame(all_small[all_small$num_item %in% "all", ])



 ggplot(all_small[!all_small$num_item %in%"all", ],
       aes(x=as.factor(item_temp), y=mean_info,
           group=selection, color=selection)) + theme_minimal() +
  geom_line(aes(linetype = selection), lwd = 1.5)  + 
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info),
                width=.2,
                position=position_dodge(0.05)) +
  theme(axis.text.x = element_text(size = 20), 
        legend.position = "top", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 32, face = "italic"),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28)) + ylab("Information") +
  scale_x_discrete(labels =  unique(all_small[!all_small$num_item %in%"all", "num_item"])) +
  geom_hline(data = dummy, aes(yintercept = mean_info)) + 
  facet_wrap(~distribution) + ylab("Information")

 # information 10 item -----

temp_graph_normal_theta_all10 <- data_info_theta_all[data_info_theta_all$num_item %in% "number10", ]
temp_graph_normal_theta_all10$distribution = "Normal"

temp_graph_sk_theta_all10 <- data_info_sk_theta_all[data_info_sk_theta_all$num_item %in% "number10", ]
temp_graph_sk_theta_all10$distribution = "Skewed"

temp_graph_uni_theta_all10 <- data_info_uni_theta_all[data_info_uni_theta_all$num_item %in% "number10", ]
temp_graph_uni_theta_all10$distribution = "Uniform"

graph_tif10 = rbind(temp_graph_normal_theta_all10, 
                    temp_graph_sk_theta_all10, 
                    temp_graph_uni_theta_all10)
graph_tif10 = graph_tif10[!graph_tif10$sel %in% "guided", ]

graph_tif10$sel  = plyr::revalue(graph_tif10$sel, 
                                 c("cluster" = "UIP", 
                                   "guidedNEW" = "EIP", 
                                   "random" = "RP", 
                                   "smart" = "BP"))

ggplot(graph_tif10, 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16) ,
        legend.position = "top", 
        legend.title = element_blank(), 
        axis.title = element_text(size = 32, , face = "italic"),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28)) +xlab(expression(theta)) + 
  facet_wrap(~distribution) + ylab("Information")

# Bias -----
sk_groups = sk_bias_groups_theta[sk_bias_groups_theta$num_item %in% "number10", ]
uni_groups = uni_bias_groups_theta[uni_bias_groups_theta$num_item %in% "number10", ]
normal_groups = normal_bias_groups_theta[normal_bias_groups_theta$num_item %in% "number10", ]

sk_groups = sk_groups[!sk_groups$selection %in% "range_new", ]
sk_groups$distr = "Skewed"
uni_groups = uni_groups[!uni_groups$selection %in% "range_new", ]
uni_groups$distr = "Uniform"
norm_groups = normal_groups[!normal_groups$selection %in% "range_new", ]
norm_groups$distr = "Normal"

bias_groups = rbind(uni_groups, sk_groups, norm_groups)
bias_groups$group = plyr::revalue(bias_groups$group,
                                  c("a" = "< -2.5",
                                    "b" = "[-2.5,-1.25]",
                                    "c" = "(-1.25,0]",
                                    "d" = "(0,1.25]",
                                    "e" = "(1.25,2.5]",
                                    "f" = "> 2.5"))
bias_groups$group = factor(bias_groups$group, levels = c("< -2.5",  "[-2.5,-1.25]", "(-1.25,0]", "(0,1.25]", "(1.25,2.5]", "> 2.5" ))
bias_groups$selection = plyr::revalue(bias_groups$selection,
                                      c("cluster" = "UIP",
                                        "random" = "RP",
                                        "range" = "EIP",
                                        "smart" = "BP"))

ggplot(bias_groups,
       aes(x=group, y = bias_obs, group = selection,
           color = selection)) + geom_line(aes(linetype = selection),
                                           lwd =1.3) +  theme_minimal() +
  theme(legend.position = "top") + xlab(expression(theta))+
  ylab("bias") +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1),
        legend.title = element_blank(),
        axis.title = element_text(size = 32),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28), 
        axis.title.y = element_text(size = 32, face = "italic")) +
  facet_grid(~distr)
