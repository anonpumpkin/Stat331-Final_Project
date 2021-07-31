train.set <- pollution[1:600,]
test.set <- pollution[601:1000,]
M0 <- lm(e3_bw ~ 1, data = train.set)
Mfull <- lm(e3_bw ~ ., data = train.set)
Mstep <- step(object = M0,
              scope = list(lower = M0, upper = Mfull), 
              direction = "both", trace = 1)
M <- Mstep
n <- nobs(M)
p <- length(M$coef)-1
dffits_m <- dffits(M) 

## plot DFFITS
plot(dffits_m,ylab="DFFITS") 
abline(h=2*sqrt((p+1)/n),lty=2)  ## add thresholds
abline(h=-2*sqrt((p+1)/n),lty=2)
## highlight influential points
dff_ind <- which(abs(dffits_m)>2*sqrt((p+1)/n))
length(as.vector(dff_ind)) #36
points(dffits_m[dff_ind]~dff_ind,col="red",pch=19) ## add red points
text(y=dffits_m[dff_ind],x=dff_ind, labels=dff_ind, pos=2) ## label high influence points





#new set
new_set <- train.set[-dff_ind,]

#Model with updated observations
Mstep2 <- lm(e3_bw ~ e3_gac_None + h_bro_preg_Log + e3_sex_None + 
              h_mbmi_None + hs_wgtgain_None + e3_asmokcigd_p_None + h_pm10_ratio_preg_None + 
              hs_pfoa_m_Log2 + hs_mepa_madj_Log2 + hs_dmtp_madj_Log2 + 
              hs_pb_m_Log2 + h_edumc_None + hs_pbde153_madj_Log2 + h_dairy_preg_Ter + 
              hs_hg_m_Log2 + hs_dep_madj_Log2 + hs_etpa_madj_Log2 + hs_trcs_madj_Log2 + 
              e3_alcpreg_yn_None, data = new_set) #this formula is obtained from first model fit
M2 <- Mstep2
n <- nobs(M2)
p <- length(M2$coef)-1
dffits_m <- dffits(M2) 

## plot DFFITS
plot(dffits_m,ylab="DFFITS") 
abline(h=2*sqrt((p+1)/n),lty=2)  ## add thresholds
abline(h=-2*sqrt((p+1)/n),lty=2)
## highlight influential points
dff_ind <- which(abs(dffits_m)>2*sqrt((p+1)/n))
length(as.vector(dff_ind)) #36
points(dffits_m[dff_ind]~dff_ind,col="red",pch=19) ## add red points
text(y=dffits_m[dff_ind],x=dff_ind, labels=dff_ind, pos=2) ## label high influence points



M1.res <- test.set$e3_bw - # test observations
  predict(M, newdata = test.set) # prediction with training data
M2.res <- test.set$e3_bw - predict(M2, newdata = test.set)
mean(M1.res^2) #189595.1
mean(M2.res^2) #186339.6
#Here, M1.res > M2.res, this is what we expected