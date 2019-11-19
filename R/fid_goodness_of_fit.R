

pacman::p_load(arm,coin,tidyverse)

# ---------------------------------------------------------------------------
## this function takes:
## md:              a lm model,
## modelequation:   a string that describe the model(if not specified, will be the $call of the lm model)
## linecolor:       specify a color for the line occurs in the plot
## The two plots are in list, and it is a ggplot so
## one can modify the plot by adding theme to the output plots

plotmodel <- function(md,modelequation = md$call,linecolor = "#CE8891"){
  p1 <- ggplot(md)+
    aes(x = .fitted,y = .stdresid)+
    geom_point()+geom_smooth(method = "loess",se = F)+
    geom_abline(intercept = 2,slope = 0,linetype = "dashed",color = linecolor)+
    geom_abline(intercept = -2,slope = 0,linetype = "dashed",color = linecolor)+
    xlab("Fitted value: y_hat")+
    ylab("Standardized Residuals")+
    ggtitle(label = "Residual plot",subtitle = modelequation)

  p2 <- ggplot(fit.benchmark)+
    aes(sample = .stdresid)+
    stat_qq(color = linecolor)+
    stat_qq_line(linetype = "dashed")+
    xlab("Theoretical Normal distribution")+
    ylab("Standardized Residuals")+
    ggtitle(label = "QQ plot for residuals")

  return(list(redisualplot = p1,
              qqplot = p2))
}

# ---------------------------------------------------------------------------

## generate SD for each observed datapoint
md_std <- function(md){
  X_ <- md$model[,2]
  left <- diag(x = X_ )
  mid <- as.numeric(vcov(md))*diag(x = 1,nrow = length(X_))
  sig <- summary(md)$sigma
  ## the original sd estimation for a specific observation x should be
  ## sqrt(1+x %*% vcov(model) %*% x.T)  *   sigma_y
  ## However, this only allows for point wise multiplication. For our purpose,
  ## because we need to calculate for all the adjusted standard error for each predicted y
  ## the left X is transform into a diagnal matrix with ith entry is X_i,
  ## the vcov(model) is also transformed into a diagnal matrix such that ensure each X_i will
  ## independently times a single vcov(model).
  ## The right is just the feature matrix X with each row is one observation
  return(sqrt(1+left %*% mid %*% X_)*sig)
}


## using predicted y value as mean and calculated sd as sd simulate y
md_y_sim <- function(md){
  y_pred <- predict(md)
  y_sd <- md_std(md)
  return(rnorm(n = length(y_pred),mean = y_pred,sd = y_sd))
}

## Use the above function to simulate y_sim and compare to the original y_obs
md_test <- function(md,method = "pearson",modelequation = md$call){
  ## Right now only work for a lm model with only 1 predictor, but with modification
  ## it is easy to have it work for multiple predictors.
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  ## KS test
  md.ks.result <- ks.test(y_obs,y_sim)
  ## permutation test, use lib coin
  md.perm.result <- coin::independence_test(y_obs ~ y_sim)
  ## t test
  md.t.result <- t.test(y_obs,y_sim)
  ## Histogram of y_obs & y_sim
  Y_ <- data.frame(cbind(y_obs,y_sim))%>%
    tidyr::pivot_longer(cols = 1:2,names_to = "Y",values_to = "value")
  p1 <- ggplot(Y_)+
    geom_histogram(aes(x = value,fill = Y,color = Y),position = "identity",alpha = .2)+
    xlab("")+
    ggtitle(label = "Histogram of y_obs vs y_sim",subtitle = modelequation)
  ## correlation test
  md.corr.result <- cor(y_obs,y_sim,use = "everything",method = method)
  ## F test for variance
  md.f.result <- var.test(y_sim,y_obs)

  return(
    list(KS_test = md.ks.result,
         Permutation_test = md.perm.result,
         t_test = md.t.result,
         correlation_test = md.corr.result,
         variance_F_test = md.f.result,
         histogram = p1
    )
  )
}

# ---------------------------------------------------------------------------
## functions below are taking individual tests separately such that one can choose what
## is interested

## test y_sim and actual y using ks test,low p-value will reject the hypo-
## thesis that they are from the same distribution
md_ks_test <- function(md){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  return(ks.test(y_obs,y_sim))
}

## test y_sim and actual y using permutation test, low p-value will rej-
## ect the hypothesis that they are independent

md_perm_test <- function(md){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  return(coin::independence_test(y_obs ~ y_sim))
}

## test y_sim and actual y using t-statistics

md_t_test <- function(md){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  return(t.test(y_obs,y_sim))
}
## plot the observed data and simulated data as histogram on the same graph
plot_md_hist <- function(md,modelequation = md$call){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  Y_ <- data.frame(cbind(y_obs,y_sim))%>%tidyr::pivot_longer(cols = 1:2,names_to = "Y",values_to = "value")
  p1 <- ggplot(Y_)+
    geom_histogram(aes(x = value,fill = Y,color = Y),position = "identity",alpha = .2)+
    xlab("")+
    ggtitle(label = "Histogram of y_obs vs y_sim",subtitle = modelequation)
  return(p1)
}

## correlation test, pearson or spearman
md_corr_test <- function(md,method = "pearson"){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  return(cor(y_obs,y_sim,use = "everything",method = method))
}

## F-test for two sample sd

md_f_test <- function(md){
  y_obs <- md$model[,1]
  y_sim <- md_y_sim(md)
  return(var.test(y_sim,y_obs))
}
