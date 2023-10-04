aicbic_plm <- function(object, criterion) {

  # object is "plm", "panelmodel" 
  # Lets panel data has index :index = c("Country", "Time")
  
  sp = summary(object) 
  
  if(class(object)[1]=="plm"){
    u.hat <- residuals(sp) # extract residuals
    df <- cbind(as.vector(u.hat), attr(u.hat, "index"))
    names(df) <- c("resid", "Country", "Time")
    c = length(levels(df$Country)) # extract country dimension 
    t = length(levels(df$Time)) # extract time dimension 
    np = length(sp$coefficients[,1]) # number of parameters
    n.N = nrow(sp$model) # number of data
    s.sq  <- log( (sum(u.hat^2)/(n.N))) # log sum of squares
    
    # effect = c("individual", "time", "twoways", "nested"),
    # model = c("within", "random", "ht", "between", "pooling", "fd")
    
    # I am making example only with some of the versions:
    
    if (sp$args$model == "within" & sp$args$effect == "individual"){
      n = c
      np = np+n+1 # update number of parameters
    }
    
    if (sp$args$model == "within" & sp$args$effect == "time"){
      T = t
      np = np+T+1 # update number of parameters
    }
    
    if (sp$args$model == "within" & sp$args$effect == "twoways"){
      n = c
      T = t
      np = np+n+T # update number of parameters
    }
    aic <- round(       2*np  +  n.N * (  log(2*pi) + s.sq  + 1 ),1)
    bic <- round(log(n.N)*np  +  n.N * (  log(2*pi) + s.sq  + 1 ),1)
    
    if(criterion=="AIC"){
      names(aic) = "AIC"
      return(aic)
    }
    if(criterion=="BIC"){
      names(bic) = "BIC"
      return(bic)
    }
  }
}
