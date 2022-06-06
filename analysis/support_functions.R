
#Function to subset data
df_subset <- function(rts){
  
  if (min(rts %in% c("air_federal","otsa","tdsa","air_state","sdtsa"))){
    df2_rts <- df2 %>% 
      filter(rt %in% rts)
    df1_rts <- df1 %>% 
      filter(tribe %in% df2_rts$tribe)
    
    data_long_rts_0 <- bind_rows(df1_rts,df2_rts) %>% 
      select(tribe,GEOID,UID,time,everything()) %>% 
      filter(!is.na(tribe)) %>% 
      mutate(tribe = factor(tribe),
             time = factor(time),
             whp = ifelse(is.na(whp),whp_mean,whp)) 
    
    data_t1and2_long_rts_0 <- filter(data_long_rts_0, 
                                     tribe %in% tribes_time1and2_lst$tribe)
    
    data_long_rts <- data_long_rts_0 %>%
      group_by(tribe,time) %>%
      summarise(wgt=1/n()) %>%
      ungroup() %>%
      left_join(data_long_rts_0,
                .,
                by=c("tribe","time")) %>% 
      group_by(time, tribe) %>% 
      summarise(across(c(heatdays_mean,drought_mean,precip,
                         whp,elevation_mean,tri_mean,SOC_perc,
                         BasinPortion,AllArea_OilPortion,
                         AllArea_GasPortion), ~ mean(.x, na.rm = TRUE))) %>% 
      ungroup() %>%
      left_join(PAD,by=c("tribe","time"))
    
    data_t1and2_long_rts <- data_t1and2_long_rts_0 %>%
      group_by(tribe,time) %>%
      summarise(wgt=1/n()) %>%
      ungroup() %>%
      left_join(data_t1and2_long_rts_0,
                .,
                by=c("tribe","time")) %>% 
      group_by(time, tribe) %>% 
      summarise(across(c(heatdays_mean,drought_mean,precip,
                         whp,elevation_mean,tri_mean,SOC_perc,
                         BasinPortion,AllArea_OilPortion,
                         AllArea_GasPortion), ~ mean(.x, na.rm = TRUE))) %>% 
      ungroup() %>%
      left_join(PAD,by=c("tribe","time"))
    
    return(data_t1and2_long_rts)
  } else {
    
    message("Pick a valid rt: air_federal, otsa, tdsa, air_state, sdtsa")
    
  }
  
}

# data_long_rts <- df_subset(c("tdsa","sdtsa"))[[1]]
# data_t1and2_long_rts <- df_subset("otsa")[[2]]


#Function to coerce fixest object into stargazer table
fixest_to_coeftest <- function(fixest.object,ds,clust.var){
  require(fixest)
  #clust.var should be a vector of variable names
  #Check if arg1 is fixest class
  if(!class(fixest.object)=="fixest"){
    stop("Not a fixest object")
  }
  
  #Extract the coefficient table
  df <- fixest.object$coeftable 
  
  if(length(fixest.object$obsRemoved)>0){
    cluster.var <- ds[-fixest.object$obsRemoved,clust.var]
  } else {
    cluster.var <- ds[,clust.var]
  }
  
  
  df.mod <- df %>%
    mutate(se = as.numeric(sqrt(diag(vcov(fixest.object,cluster=cluster.var)))),
           stat = Estimate/se,
           p = 2*pt(-abs(stat),df=nobs(fixest.object)-fixest.object$nparams)) %>%
    dplyr::select(Estimate,se,stat,p)
  
  
  #Coerce dataframe into matrix
  coeftest.object <- as.matrix(df.mod)
  
  #Capture variable names from df and rename columns to conform to coeftest
  new_names <- list(rownames(df),
                    c("Estimate","Std. Error","t value","Pr(>|t|)"))
  
  #Assign names
  dimnames(coeftest.object) <- new_names
  
  #re-class to coeftest
  class(coeftest.object) <- 'coeftest'
  
  return(coeftest.object)
  
}

#Function to coerce lme object into stargazer table
lme_to_coeftest <- function(lme.object){
  require(clubSandwich)
  #Check if arg1 is fixest class
  if(!class(lme.object)=="lme"){
    stop("Not an lme object")
  }
  
  #Extract the coefficient table
  df <- coef_test(lme.object,
                  vcov = vcovCR(lme.object,
                                type="CR2"))
  
  #Coerce dataframe into matrix
  coeftest.object <- as.matrix(df[,c(1,2,3,5)])
  
  #Capture variable names from df and rename columns to conform to coeftest
  new_names <- list(rownames(df),
                    c("Estimate","Std. Error","t value","Pr(>|t|)"))
  
  #Assign names
  dimnames(coeftest.object) <- new_names
  
  #re-class to coeftest
  class(coeftest.object) <- 'coeftest'
  
  return(coeftest.object)
  
}

tidy_to_coeftest <- function(tidy.object){
  #This function takes an object created with broom::tidy() and converts it 
  #to a coeftest object that can be directly input into stargazer to produce
  #regression output tables
  
  #Grab the non-variable name columns and coerce to matrix
  coeftest.object <- as.matrix(tidy.object[,2:5])
  
  #Define the dimension names of the matrix
  dimnames(coeftest.object) <- list(tidy.object$term,
                                    c("Estimate","Std. Error","t value","Pr(>|t|)"))
  
  #Coerce matrix to class coeftest
  class(coeftest.object) <- 'coeftest'
  
  return(coeftest.object)
}

gamlss_clustered_vcov <- function(gamlss.object,cluster=NA){
  require(gamlss)
  # if(is.na(cluster)){
  #   #set cluster to be index
  #   #fc <- index(gamlss.object$mu.x)
  # } else {
  #   #check length conforms
  #   if(!length(cluster)==length(gamlss.object$y)){
  #     stop("Clustering vector provided does not conform; check if there are NAs dropped in estimation.")
  #   }
  # }
  esample <- as.numeric(rownames(as.matrix(gamlss.object$mu.qr$qr)))
  fc <- cluster  #isolates the groups used in estimation
  
  # Calculates the new Meat portion of our covariance matrix
  m <- length(unique(fc))
  k <- length(gamlss.object$mu.coefficients)
  u <- gamlss.object$mu.qr$qr
  u.clust <- matrix(NA, nrow=m, ncol=k)
  for(j in 1:k){
    u.clust[,j] <- tapply(u[,j], as.numeric(fc), sum)
  }
  u.clust <- u.clust[!rowSums(u.clust,dims = 1)==0,] #dropping all clusters with all zeros
  cl.vcov <- vcov(gamlss.object)[1:k,1:k]%*%( t(u.clust) %*% (u.clust))%*%vcov(gamlss.object)[1:k,1:k]
  return(cl.vcov)
}



ols_clustered <- function(dep.var,df,clust.var){
  require(broom)
  m.temp <- feols(formula(str_c(dep.var, " ~ time")),
                  data = df) 
  
  model.out <- fixest_to_coeftest(m.temp,df,clust.var)
  
  obs <- nobs(m.temp)
  
  cov.out <- vcov(m.temp,cluster="tribe")
  
  return(list(results=model.out,obs=obs,cov=cov.out))
}

# dep.var = "heatdays_mean"
# df = data_long_m1

ols_robust <- function(dep.var,df){
  require(broom)
  m.temp <- lm(formula(str_c(dep.var, " ~ time")),
                  data = df) 
  coef <- lmtest::coeftest(m.temp, vcov = sandwich::vcovHC(m.temp, type = "HC0"))
  
  model.out <- tidy_to_coeftest(tidy(coef))
  
  obs <- nobs(m.temp)
  
  return(list(results=model.out,obs=obs))
}

#dep.var <- "precip"
#df <- svy_data_t1andt2_long
ols_weighted <- function(dep.var,df){
  require(survey)
  
  m.temp <- svyglm(formula(str_c(dep.var, " ~ time")),
                   design = df) 
  
  model.out <- tidy_to_coeftest(tidy(m.temp))
  
  obs <- nobs(m.temp)
  
  
  return(list(results=model.out,obs=obs))
}

feols_clustered <- function(dep.var,df){
  
  m.temp <- feols(formula(str_c(dep.var, " ~ time | tribe")),
                  data = df) 
  
  model.out <- fixest_to_coeftest(m.temp,df,clust.var = c("tribe"))
  
  obs <- nobs(m.temp)
  
  
  return(list(results=model.out,obs=obs))
}

glm_clustered <- function(dep.var,df,family){
  
  m.temp <- feglm(formula(str_c(dep.var, " ~ time")),
                  family = family,
                  data = df) 
  
  model.out <- fixest_to_coeftest(m.temp,df,clust.var = "tribe")
  
  obs <- nobs(m.temp)
  
  return(list(results=model.out,obs=obs))
}

glm_qb <- function(dep.var,df,family){
  
  df_qb <- df %>% 
    mutate(dep.var = as.factor(dep.var))
  
  m.temp <- glm(formula(str_c(dep.var, " ~ time")),
                  family = family,
                  data = df_qb) 
  
  model.out <- tidy_to_coeftest(tidy(m.temp))
  
  obs <- nobs(m.temp)
  
  return(list(results=model.out,obs=obs))
}


sg_table <- function(model.shell,dep.name,...){
  require(stargazer)
  require(stringr)
  
  stargazer(map(model.shell,"results"),type="latex",
            ci=T,
            intercept.top = T,
            intercept.bottom = F,
            digits = 2,
            dep.var.caption = str_c("Dependent Variable: ",dep.name),
            #column.labels = col.names,
            model.numbers = T,
            font.size = "footnotesize",
            column.sep.width = "0pt",
            covariate.labels = c("Historical (Intercept)","Present-day Change"),
            #title = "Extent of Area Occupied",
            add.lines = list(c("Obs",comma(map_dbl(model.shell,"obs")))),
            # float.env = "sidewaystable",
            notes = c("0.0005952381 is the Bonferroni adjustment to alpha of 0.05.",
                      "95\\% confidence intervals in parentheses, based on",
                      "clustered standard errors (tribe)."),
            # notes = c("0.0005952381 is the Bonferroni adjustment to alpha of 0.05.",
            #           "95\\% confidence intervals in parentheses, based on",
            #           "robust standard errors."),
            notes.align = "l",
            header = F,
            star.char = c("*","**","***"),
            star.cutoffs = c(0.1,0.05,0.0005952381),
            ...)
}
