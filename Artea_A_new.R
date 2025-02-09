

rm(list=ls())
library(stargazer)

Artea=read.csv("Artea.csv")

## assign acquisition channel as factor
##  " Google", " Facebook"," Instagram", ' Referral'," Other"
Artea$channel_acq[Artea$channel_acq == 1] <- 'Google'
Artea$channel_acq[Artea$channel_acq == 2] <- 'Facebook'
Artea$channel_acq[Artea$channel_acq == 3] <- 'Instagram'
Artea$channel_acq[Artea$channel_acq == 4] <- 'Referral'
Artea$channel_acq[Artea$channel_acq == 5] <- 'Other'

Artea$channel_acq=as.factor(Artea$channel_acq)

# because most people are acquired from Google, 
# we set it as the reference group
Artea$channel_acq <- relevel(Artea$channel_acq, ref = "Google")

# compare mean transaction & revenue of two groups (coupon 0/1)
# Calculate the mean and standard deviation 
# of numeric_column based on binary_column
aggregate(trans_after ~ test_coupon, 
          data = Artea, 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))

aggregate(revenue_after ~ test_coupon, 
          data = Artea, 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Print the result
print(result)


## Next, we can do a regression analysis to investigate 
# which types of customers respond more strongly to coupons

# DV: Transaction after
# IV: test coupn, channel_acq, num_past_purch,  ...

fmla = as.formula(paste("trans_after ~ test_coupon"))
lm_trans_coup = lm(formula=fmla,data=Artea)


# control for other covariates
feature_cols = c(colnames(Artea)[4],colnames(Artea)[6:10])

fmla = as.formula(paste("trans_after ~ test_coupon +",
                        paste(feature_cols, collapse= "+")
))
lm_trans_coup2 = lm(formula=fmla,data=Artea)
stargazer(lm_trans_coup, lm_trans_coup2, type = "text",single.row = TRUE)

# DV: Revenue after
# IV: test coupn, channel_acq, num_past_purch,  ...
fmla = as.formula(paste("revenue_after ~ test_coupon +",
                        paste(feature_cols, collapse= "+")
))
lm_trans_coup2 = lm(formula=fmla,data=Artea)
stargazer(lm_trans_coup, lm_trans_coup2, type = "text",single.row = TRUE)



## Heterogenity analysis

# Interaction with channels

# Transaction_after
fmla = as.formula(paste("trans_after ~ test_coupon*channel_acq"))
lm_trans_channel = lm(formula=fmla,data=Artea)

fmla = as.formula(paste("trans_after ~ test_coupon*channel_acq + ",
                        paste(feature_cols[-1], collapse = '+')
))
lm_trans_channel2 = lm(fmla, data=Artea)

stargazer(lm_trans_channel,lm_trans_channel2,  type = "text",
          single.row = TRUE)

# Revenue
fmla =  as.formula(paste("revenue_after ~ test_coupon*channel_acq"))
lm_reven_channel = lm(formula=fmla,data=dat)

fmla = as.formula(paste("revenue_after ~ test_coupon* channel_acq + ",
                        paste(feature_cols[-1], collapse = '+')))
lm_reven_channel2=  lm(formula=fmla,data=dat)

stargazer(lm_reven_channel, lm_reven_channel2, type = "text",
          single.row = TRUE)

# Explore other interactions