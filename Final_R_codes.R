library(haven)
library(dplyr)
data1 = read_dta(file.choose())
data2 = read_dta(file.choose())
data3 = read_dta(file.choose())
data4 = read_dta(file.choose())
data5 = read_dta(file.choose())
data6 = read_dta(file.choose())



data1_sr1 = na.omit(data1[, c(6, 19, 23, 28, 30, 31)])
data2_sr1 = na.omit(data2[, c(6, 19, 21, 25, 27, 28)])
data3_sr1 = na.omit(data3[, c(6, 20, 28, 32, 34, 35)])
data4_sr1 = na.omit(data4[, c(6, 19, 20, 24, 26, 27)])
data5_sr1 = na.omit(data5[, c(6, 19, 20, 24, 26, 27)])
summary = na.omit(data6[, c(6, 19, 20, 24, 26, 27)])

colname = colnames(data1_sr1)
colname[3] = "TCV"
colnames(data1_sr1) = colnames(data2_sr1) = colnames(data3_sr1) = colnames(data4_sr1) = colnames(data5_sr1) = colnames(summary) = colname

rep = c(6, 19, 20, 31, 38, 39, 40, 41, 42, 43)

cond <- !as.numeric(summary$Item_Code) %in% rep

sum_final = summary[cond,]

sum_rur = sum_final[which(as.numeric(sum_final$Sector) == 1),-1]
sum_urb = sum_final[which(as.numeric(sum_final$Sector) == 2),-1]


sum_rur = sum_rur %>% arrange(HHID)
sum_urb = sum_urb %>% arrange(HHID)




weight = function(final) {
  household = sort(as.numeric(unique(final$HHID)))
  item_list = sort(as.numeric(unique(final$Item_Code)))
  combmult = numeric(length(household))
  k = 0
  
  for (i in household) {
    k = k + 1
    combmult[k] = final$Combined_Multiplier[which(as.numeric(final$HHID) == i)[1]]
  }
  
  R = matrix(0,
             nrow = length(household),
             ncol = length(item_list))
  row = 0
  
  for (i in household) {
    col = 0
    row = row + 1
    h = final[which(as.numeric(final$HHID) == i),]
    for (j in item_list) {
      col = col + 1
      if (j %in% as.numeric(h$Item_Code) == TRUE) {
        R[row, col] = h$TCV[which(as.numeric(h$Item_Code) == j)] /
          sum(h$TCV)
      }
    }
  }
  
  weight = numeric(length(item_list))
  col = 0
  for (j in item_list) {
    col = col + 1
    weight[col] = sum(R[, col] * combmult) / sum(combmult)
  }
  return(cbind(item_list, weight))
}


basket = function(final, alpha){
  item_list = sort(as.numeric(unique(final$Item_Code)))
  district = split(final, final$District_Code)
  weight1 = vector("list", length(district))
  
  for (i in 1:length(district)) {
    weight1[[i]] = weight(district[[i]])
    #print(i)
  }
  
  weight_rank = vector("list", length(weight1))
  
  for (i in 1:length(weight1)) {
    x = as.data.frame(weight1[[i]])
    weight_rank[[i]] = x[order(x$weight, decreasing = TRUE), ]
    y = as.data.frame(weight_rank[[i]])
    weight_rank[[i]] = cbind(weight_rank[[i]], rank(y$weight))
  }
  
  dist_rank = matrix(0,
                     nrow = length(item_list),
                     ncol = length(weight1) + 1)
  dist_rank[, 1] = item_list
  
  for (i in 1:nrow(dist_rank)) {
    for (j in 1:18) {
      h = as.data.frame(weight_rank[[j]])
      if (dist_rank[i, 1] %in% h$item_list == FALSE)
        dist_rank[i, j + 1] = 0
      else{
        index = which(h$item_list == dist_rank[i, 1])
        dist_rank[i, j + 1] = h[index, 3]
      }
    }
  }
  
  m1 = 0.4*rowSums(dist_rank[, c(2:19)]) / 18 + 0.6*apply(dist_rank[,c(2:19)],1,median)
  dist_rank = cbind(dist_rank, m1)
  dist_rank = as.data.frame(dist_rank)
  dist_rank = dist_rank[order(dist_rank$m1, decreasing = TRUE), ]
  s1 = quantile(dist_rank$m1, alpha)
  basket = dist_rank$V1[which(dist_rank$m1 > s1)]
  
  return(basket)
  
}




group_rur = basket(sum_rur, 0.80)
group_urb = basket(sum_urb, 0.80)



final_0 = bind_rows(data1_sr1, data2_sr1, data3_sr1, data4_sr1, data5_sr1)

repeatation = c(
  129,
  139,
  159,
  169,
  179,
  189,
  199,
  219,
  239,
  249,
  269,
  279,
  289,
  299,
  309,
  319,
  329,
  349,
  379,
  389,
  399,
  409,
  419,
  429,
  439,
  449,
  459,
  479,
  499,
  519,
  529,
  549,
  559,
  569,
  579,
  599,
  609,
  619,
  629,
  639,
  649,
  659
)




condition <- !as.numeric(final_0$Item_Code) %in% repeatation

final = final_0[condition, ]

final_rur = final[which(as.numeric(final$Sector) == 1), -1]
final_urb = final[which(as.numeric(final$Sector) == 2), -1]


final_rur <- final_rur %>% arrange(HHID)
final_urb <- final_urb %>% arrange(HHID)


rur1 = urb1 = c(101:122)
rur2 = c(140:152)
rur3 = urb2 = c(330:345)
rur4 = urb3 = c(350:376)
rur5 = urb4 = c(390:395)
rur6 = urb5 = c(400:408)
urb6 = c(410:414)
rur7 = urb7 = c(550:643)




cu1 <- as.numeric(final_rur$Item_Code) %in% rur1

dfrur1  = final_rur[cu1, ]

cu2 <- as.numeric(final_rur$Item_Code) %in% rur2

dfrur2  = final_rur[cu2, ]

cu3 <- as.numeric(final_rur$Item_Code) %in% rur3

dfrur3  = final_rur[cu3, ]

cu4 <- as.numeric(final_rur$Item_Code) %in% rur4

dfrur4  = final_rur[cu4, ]

cu5 <- as.numeric(final_rur$Item_Code) %in% rur5

dfrur5  = final_rur[cu5, ]

cu6 <- as.numeric(final_rur$Item_Code) %in% rur6

dfrur6  = final_rur[cu6, ]

cu7 <- as.numeric(final_rur$Item_Code) %in% rur7

dfrur7  = final_rur[cu7, ]




cu1 <- as.numeric(final_urb$Item_Code) %in% urb1

dfurb1  = final_urb[cu1, ]

cu2 <- as.numeric(final_urb$Item_Code) %in% urb2

dfurb2  = final_urb[cu2, ]

cu3 <- as.numeric(final_urb$Item_Code) %in% urb3

dfurb3  = final_urb[cu3, ]

cu4 <- as.numeric(final_urb$Item_Code) %in% urb4

dfurb4  = final_urb[cu4, ]

cu5 <- as.numeric(final_urb$Item_Code) %in% urb5

dfurb5  = final_urb[cu5, ]

cu6 <- as.numeric(final_urb$Item_Code) %in% urb6

dfurb6  = final_urb[cu6, ]

cu7 <- as.numeric(final_urb$Item_Code) %in% urb7

dfurb7  = final_urb[cu7, ]


basket_rur1 = basket(dfrur1,0.90)
basket_rur2 = basket(dfrur2,0.90)
basket_rur3 = basket(dfrur3,0.90)
basket_rur4 = basket(dfrur4,0.90)
basket_rur5 = basket(dfrur5,0.90)
basket_rur6 = basket(dfrur6,0.90)
basket_rur7 = basket(dfrur7,0.90)

basket_urb1 = basket(dfurb1,0.90)
basket_urb2 = basket(dfurb2,0.90)
basket_urb3 = basket(dfurb3,0.90)
basket_urb4 = basket(dfurb4,0.90)
basket_urb5 = basket(dfurb5,0.90)
basket_urb6 = basket(dfurb6,0.90)
basket_urb7 = basket(dfurb7,0.90)



library(ggplot2)
Exp_share = function(item,final){
  household = sort(as.numeric(unique(final$HHID)))
  R = numeric(length(household))
  k = 0
  for(i in household){
    k = k+1
    h = final[which(as.numeric(final$HHID) == i),]
    if (item %in% as.numeric(h$Item_Code) == TRUE) {
      R[k] = h$TCV[which(as.numeric(h$Item_Code) == item)] /
        sum(h$TCV)
    }
  }
  return(R)
}



x1 = final_rur$TCV[which(final_rur$Item_Code == 101)]
x2 = final_urb$TCV[which(final_urb$Item_Code == 101)]

ggplot() +
  geom_histogram(data = data.frame(value = x1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = x2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  #geom_density(data = data.frame(value = x1), aes(x = value), alpha = 0.5) +
  #geom_density(data = data.frame(value = x2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Rice(Public Distribution Systems) TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()



a1 = final_rur$TCV[which(final_rur$Item_Code == 108)]
a2 = final_urb$TCV[which(final_urb$Item_Code == 108)]

ggplot() +
  geom_histogram(data = data.frame(value = a1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = a2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_density(data = data.frame(value = a1), aes(x = value), alpha = 0.5) +
  geom_density(data = data.frame(value = a2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Wheat/Atta (non PDS) TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()


b1 = final_rur$TCV[which(final_rur$Item_Code == 140)]
b2 = final_urb$TCV[which(final_urb$Item_Code == 140)]

ggplot() +
  geom_histogram(data = data.frame(value = b1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = b2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_density(data = data.frame(value = b1), aes(x = value), alpha = 0.5) +
  geom_density(data = data.frame(value = b2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Arhar, Tur TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()


c1 = final_rur$TCV[which(final_rur$Item_Code == 147)]
c2 = final_urb$TCV[which(final_urb$Item_Code == 147)]

ggplot() +
  geom_histogram(data = data.frame(value = c1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = c2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_density(data = data.frame(value = c1), aes(x = value), alpha = 0.5) +
  geom_density(data = data.frame(value = c2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Khesari TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()


d1 = final_rur$TCV[which(final_rur$Item_Code == 351)]
d2 = final_urb$TCV[which(final_urb$Item_Code == 351)]

ggplot() +
  geom_histogram(data = data.frame(value = d1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = d2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_density(data = data.frame(value = d1), aes(x = value), alpha = 0.5) +
  geom_density(data = data.frame(value = d2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Saree TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()


e1 = final_rur$TCV[which(final_rur$Item_Code == 364)]
e2 = final_urb$TCV[which(final_urb$Item_Code == 364)]

ggplot() +
  geom_histogram(data = data.frame(value = e1), aes(x = value, y = after_stat(density),fill = "Rural"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_histogram(data = data.frame(value = e2), aes(x = value, y = after_stat(density), fill = "Urban"), 
                 binwidth = 10, alpha = 0.5, position = "identity") +
  geom_density(data = data.frame(value = e1), aes(x = value), alpha = 0.5) +
  geom_density(data = data.frame(value = e2), aes(x = value), alpha = 0.5) +
  labs(
    title = "Shorts, Trousers, Bermuda etc. TCV for rural and Urban",
    x = "TCV",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Rural" = "red", "Urban" = "blue")) +
  theme_gray()
