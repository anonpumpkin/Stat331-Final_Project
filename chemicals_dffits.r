#splitting data
train.set <- pollution[1:600,]
test.set <- pollution[601:1000,]

#First model with only chemicals covariates
M_chem <- lm(e3_bw ~ hs_as_m_Log2 +
               hs_cd_m_Log2 +
               hs_co_m_Log2 +
               hs_cs_m_Log2 +
               hs_cu_m_Log2 +
               hs_hg_m_Log2 +
               hs_mn_m_Log2 +
               hs_mo_m_Log2 +
               hs_pb_m_Log2 +
               hs_dde_madj_Log2 +
               hs_ddt_madj_Log2 +
               hs_hcb_madj_Log2 +
               hs_pcb118_madj_Log2 +
               hs_pcb138_madj_Log2 +
               hs_pcb153_madj_Log2 +
               hs_pcb170_madj_Log2 +
               hs_pcb180_madj_Log2 +
               hs_sumPCBs5_madj_Log2 +
               hs_dep_madj_Log2 +
               hs_detp_madj_Log2 +
               hs_dmp_madj_Log2 +
               hs_dmtp_madj_Log2 +
               hs_pbde153_madj_Log2 +
               hs_pbde47_madj_Log2 +
               hs_pfhxs_m_Log2 +
               hs_pfna_m_Log2 +
               hs_pfoa_m_Log2 +
               hs_pfos_m_Log2 +
               hs_pfunda_m_Log2 +
               hs_bpa_madj_Log2 +
               hs_bupa_madj_Log2 +
               hs_etpa_madj_Log2 +
               hs_mepa_madj_Log2 +
               hs_oxbe_madj_Log2 +
               hs_prpa_madj_Log2 +
               hs_trcs_madj_Log2 +
               hs_mbzp_madj_Log2 +
               hs_mecpp_madj_Log2 +
               hs_mehhp_madj_Log2 +
               hs_mehp_madj_Log2 +
               hs_meohp_madj_Log2 +
               hs_mep_madj_Log2 +
               hs_mibp_madj_Log2 +
               hs_mnbp_madj_Log2 +
               hs_ohminp_madj_Log2 +
               hs_oxominp_madj_Log2 +
               hs_sumDEHP_madj_Log2 +
               e3_asmokcigd_p_None +
               hs_cotinine_mcat_None, data = train.set)

n <- nobs(M_chem)
p <- length(M_chem$coef)-1
dffits_m <- dffits(M_chem) 

## plot DFFITS
plot(dffits_m,ylab="DFFITS") 
abline(h=2*sqrt((p+1)/n),lty=2)  ## add thresholds
abline(h=-2*sqrt((p+1)/n),lty=2)
## highlight influential points
dff_ind <- which(abs(dffits_m)>2*sqrt((p+1)/n))
length(as.vector(dff_ind)) #36
points(dffits_m[dff_ind]~dff_ind,col="red",pch=19) ## add red points
text(y=dffits_m[dff_ind],x=dff_ind, labels=dff_ind, pos=2) ## label high influence points


#new data set after removing outliers
new_set <- train.set[-dff_ind,]


#New model with updated observations
M_chem_2 <- lm(e3_bw ~ hs_as_m_Log2 +
               hs_cd_m_Log2 +
               hs_co_m_Log2 +
               hs_cs_m_Log2 +
               hs_cu_m_Log2 +
               hs_hg_m_Log2 +
               hs_mn_m_Log2 +
               hs_mo_m_Log2 +
               hs_pb_m_Log2 +
               hs_dde_madj_Log2 +
               hs_ddt_madj_Log2 +
               hs_hcb_madj_Log2 +
               hs_pcb118_madj_Log2 +
               hs_pcb138_madj_Log2 +
               hs_pcb153_madj_Log2 +
               hs_pcb170_madj_Log2 +
               hs_pcb180_madj_Log2 +
               hs_sumPCBs5_madj_Log2 +
               hs_dep_madj_Log2 +
               hs_detp_madj_Log2 +
               hs_dmp_madj_Log2 +
               hs_dmtp_madj_Log2 +
               hs_pbde153_madj_Log2 +
               hs_pbde47_madj_Log2 +
               hs_pfhxs_m_Log2 +
               hs_pfna_m_Log2 +
               hs_pfoa_m_Log2 +
               hs_pfos_m_Log2 +
               hs_pfunda_m_Log2 +
               hs_bpa_madj_Log2 +
               hs_bupa_madj_Log2 +
               hs_etpa_madj_Log2 +
               hs_mepa_madj_Log2 +
               hs_oxbe_madj_Log2 +
               hs_prpa_madj_Log2 +
               hs_trcs_madj_Log2 +
               hs_mbzp_madj_Log2 +
               hs_mecpp_madj_Log2 +
               hs_mehhp_madj_Log2 +
               hs_mehp_madj_Log2 +
               hs_meohp_madj_Log2 +
               hs_mep_madj_Log2 +
               hs_mibp_madj_Log2 +
               hs_mnbp_madj_Log2 +
               hs_ohminp_madj_Log2 +
               hs_oxominp_madj_Log2 +
               hs_sumDEHP_madj_Log2 +
               e3_asmokcigd_p_None +
               hs_cotinine_mcat_None, data = new_set)

n <- nobs(M_chem_2)
p <- length(M_chem_2$coef)-1
dffits_m <- dffits(M_chem_2) 

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
  predict(M_chem, newdata = test.set) # prediction with training data
M2.res <- test.set$e3_bw - predict(M_chem_2, newdata = test.set)
mean(M1.res^2) #MSPE for First model
mean(M2.res^2) #MSPE for Second model
#We see that M1.res > M2.res
