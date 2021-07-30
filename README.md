# Stat331-Final_Project

# VANESSA

```{r}
# splitting training set and test set
training_set <- pollution [c(1:600),]
test_set <- pollution [-c(1:600),]
```
**FORWARD**

```{r}
# forward selection based on AIF
M_base <- lm(e3_bw ~ 1, training_set)
M_full <- lm(e3_bw ~ ., training_set)
M_forward <- step(object=M_base, scope=list(lower=M_base,upper=M_full),
                  direction="forward", trace=0)
D <- cooks.distance(M_forward)
plot(M_forward, which=4)
# outliers defined by first rule of thumb (compare to F value)
n <- nobs(M_forward)
p <- length(M_forward$coef)-1
inf_ind <- which(pf(D,p+1,n-p-1,lower.tail=TRUE)>0.1) # from lec20 code
plot(D,ylab="Cook's Distance")
points(D[inf_ind]~inf_ind,col="red",pch=19) ## add red points
text(y=D[inf_ind],x=inf_ind, labels=inf_ind, pos=4) ## label high influence points

# no outliers using cook's distance

# DFFITS
dffits_fwd <- dffits(M_forward) 

## plot DFFITS
plot(dffits_fwd,ylab="DFFITS") 
abline(h=2*sqrt((p+1)/n),lty=2)  ## add thresholds
abline(h=-2*sqrt((p+1)/n),lty=2)
## highlight influential points
dff_ind_fwd <- which(abs(dffits_fwd)>2*sqrt((p+1)/n)) # 20 outliers
points(dffits_fwd[dff_ind_fwd]~dff_ind_fwd,col="red",pch=19) ## add red points
text(y=dffits_fwd[dff_ind_fwd],x=dff_ind_fwd, labels=dff_ind_fwd, pos=2) ## label high influence points
```
```{r}
# create training data removing outliers found using DFFITS above
training_set_no_outlier_fwd <- training_set[-dff_ind_fwd,]

# fit model on this training set
M_base_no_outlier <- lm(e3_bw ~ 1, training_set_no_outlier_fwd)
M_full_no_outlier <- lm(e3_bw ~ ., training_set_no_outlier_fwd)
M_forward_no_outlier <- step(object=M_base_no_outlier,
                             scope=list(lower=M_base_no_outlier,
                                        upper=M_full_no_outlier),
                             direction="forward", trace=0)

# we use MSPE to assess the prediction accuracy of of the models
MSPE <- function(yi_new, yi_new_hat){
  mean((yi_new - yi_new_hat)^2)
  }
yi_new <- test_set$e3_bw

# MSPE on model 1
M1.pred <- predict(M_forward, newdata = test_set[,-1])
MSPE_M1 <- MSPE(yi_new,M1.pred)
MSPE_M1

# MSPE on model 2
M2.pred <- predict(M_forward_no_outlier, newdata = test_set[,-1])
MSPE_M2 <- MSPE(yi_new,M2.pred)
MSPE_M2

# MSPE using second model lower than that using first model
```
MSPE_M1 = 189595.1
MSPE_M2 = 187941.4







**BACKWARD**

```{r}
# backward selection based on AIF
M_base <- lm(e3_bw ~ 1, training_set)
M_full <- lm(e3_bw ~ ., training_set)
M_backward <- step(object=M_full, scope=list(lower=M_base,upper=M_full),
                  direction="backward", trace=0)
D_b <- cooks.distance(M_backward)
plot(M_backward, which=4)
# outliers defined by first rule of thumb (compare to F value)
n <- nobs(M_backward)
p <- length(M_backward$coef)-1
inf_ind <- which(pf(D_b,p+1,n-p-1,lower.tail=TRUE)>0.1) # from lec20 code
plot(D_b,ylab="Cook's Distance")
points(D_b[inf_ind]~inf_ind,col="red",pch=19) ## add red points
text(y=D_b[inf_ind],x=inf_ind, labels=inf_ind, pos=4) ## label high influence points

# no outliers using cook's distance


# DFFITS
dffits_back <- dffits(M_backward) 

## plot DFFITS
plot(dffits_back,ylab="DFFITS") 
abline(h=2*sqrt((p+1)/n),lty=2)  ## add thresholds
abline(h=-2*sqrt((p+1)/n),lty=2)
## highlight influential points
dff_ind_back <- which(abs(dffits_back)>2*sqrt((p+1)/n)) # 35 outliers
points(dffits_back[dff_ind_back]~dff_ind_back,col="red",pch=19) ## add red points
text(y=dffits_back[dff_ind_back],x=dff_ind_back, labels=dff_ind_back, pos=2) ## label high influence points
```
```{r}
# create training data removing outliers found using DFFITS above
training_set_no_outlier_back <- training_set[-dff_ind_back,]

# fit model on this training set
M_base_no_outlier <- lm(e3_bw ~ 1, training_set_no_outlier_back)
M_full_no_outlier <- lm(e3_bw ~ ., training_set_no_outlier_back)
M_backward_no_outlier <- step(object=M_full_no_outlier,
                             scope=list(lower=M_base_no_outlier,
                                        upper=M_full_no_outlier),
                             direction="backward", trace=0)

# we use MSPE to assess the prediction accuracy of of the models
MSPE <- function(yi_new, yi_new_hat){
  mean((yi_new - yi_new_hat)^2)
  }
yi_new <- test_set$e3_bw

# MSPE on model 1
M1.pred <- predict(M_backward, newdata = test_set[,-1])
MSPE_M1 <- MSPE(yi_new,M1.pred)
MSPE_M1

# MSPE on model 2
M2.pred <- predict(M_backward_no_outlier, newdata = test_set[,-1])
MSPE_M2 <- MSPE(yi_new,M2.pred)
MSPE_M2

# MSPE using second model slightly higher than that using first model
```
MSPE_M1 = 196500.4
MSPE_M2 = 200789.4
