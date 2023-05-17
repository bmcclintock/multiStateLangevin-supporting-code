modelName <- nbStates <- DM <- formula <- userBounds <- stateNames <- inputUD <- fixPar <- kappa <- list()

##### 1 state models #####

# add additional habitat covariates
modelName[[1]] <- "S=1 sigma(.) beta(depth*slope+d2site^2)"
nbStates[[1]] <- 1
DM[[1]] <- list(mu= matrix(c("mu.x_tm1","langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",0,0,
                            "mu.y_tm1","langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",0,0,
                            0,                  0,                  0,                       0,                   0,                    0,1,0,
                            0,                  0,                  0,                       0,                   0,                    0,1,0,
                            0,                  0,                  0,                       0,                   0,                    0,0,1),nrow=5,byrow=TRUE,dimnames = list(c("mean.x_1","mean.y_1","sd.x_1","sd.y_1","corr.xy_1"),
                                                                                                                                                                                 c("mean:mu_tm1","depth:outbound","slope:outbound","depthslope:outbound","d2site:outbound","d2site2:outbound","sd:(outbound)","corr.xy:(Intercept)"))),
               dry=matrix(1,1,1))
formula[[1]] <- ~1
userBounds[[1]] <- list(dry=matrix(c(0,0.5),1,2))
fixPar[[1]] <- getFixPar(DM[[1]])
kappa[[1]] <- Inf
stateNames[[1]] <- "outbound"
inputUD[[1]] <- list(nbUD=1,parIndex=list(2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2")),UDnames="outbound",UDstates=list(1),sign=list(1))

DM1 <- DM[[1]]
DM1$mu <- DM[[1]]$mu[,-c(4,6)]

# add ID effects on speed parameter
modelName[[2]] <- "S=1 sigma(ID) beta(depth*slope+d2site^2)"
nbStates[[2]] <- 1
DM[[2]] <- DM[[1]]
DM[[2]]$mu <- cbind(DM[[1]]$mu[,1:6],c(0,0,"ID35224","ID35224",0),c(0,0,"ID61080","ID61080",0),c(0,0,"ID61089","ID61089",0),DM[[1]]$mu[,-(1:7),drop=FALSE])
colnames(DM[[2]]$mu)[7:9] <- paste0("sd:(outbound)",c("ID35224","ID61080","ID61089"))
formula[[2]] <- ~1
userBounds[[2]] <- list(dry=matrix(c(0,0.5),1,2))
fixPar[[2]] <- getFixPar(DM[[2]])
kappa[[2]] <- Inf
stateNames[[2]] <- "outbound"
inputUD[[2]] <- list(nbUD=1,parIndex=list(2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2")),UDnames="outbound",UDstates=list(1),sign=list(1))

##### 2 state models #####

modelName[[3]] <- "S=2 sigma(.) beta(depth*slope+d2site^2) gamma(d2site)"
nbStates[[3]] <- 2
DM[[3]] <- list(mu=matrix(c("mu.x_tm1","langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",0,0,0,
                           "mu.x_tm1",                  0,                  0,                       0,                   0,                    0,0,0,0,
                           "mu.y_tm1","langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",0,0,0,
                           "mu.y_tm1",                  0,                  0,                       0,                   0,                    0,0,0,0,
                           0,                  0,                  0,                       0,                   0,                    0,1,0,0,
                           0,                  0,                  0,                       0,                   0,                    0,1,1,0,
                           0,                  0,                  0,                       0,                   0,                    0,1,0,0,
                           0,                  0,                  0,                       0,                   0,                    0,1,1,0,
                           0,                  0,                  0,                       0,                   0,                    0,0,0,1,
                           0,                  0,                  0,                       0,                   0,                    0,0,0,1),nrow=10,byrow=TRUE,dimnames=list(c("mean.x_1","mean.x_2","mean.y_1","mean.y_2","sd.x_1","sd.x_2","sd.y_1","sd.y_2","corr.xy_1","corr.xy_2"),
                                                                                                                                                                                 c("mean:mu_tm1","depth:outbound","slope:outbound","depthslope:outbound","d2site:outbound","d2site2:outbound","sd:(outbound)","sd:(haulout)","corr.xy:(Intercept)"))),
               dry=matrix(c(1,0,
                            0,1),2,2))
formula[[3]] <- ~state1(d2site)
userBounds[[3]] <- list(dry=matrix(c(0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[3]] <- getFixPar(DM[[3]])
kappa[[3]] <- 4
stateNames[[3]] <- c("outbound","haulout")
inputUD[[3]] <- list(nbUD=1,parIndex=list(2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2")),UDnames="outbound",UDstates=list(1),sign=list(1))

# add ID effects on speed parameter for "at sea" state
modelName[[4]] <- "S=2 sigma(ID) beta(depth*slope+d2site^2) gamma(d2site)"
nbStates[[4]] <- 2
DM[[4]] <- DM[[3]]
DM[[4]]$mu <- cbind(DM[[3]]$mu[,1:6],c(rep(0,4),"ID35224",0,"ID35224",0,0,0),c(rep(0,4),"ID61080",0,"ID61080",0,0,0),c(rep(0,4),"ID61089",0,"ID61089",0,0,0),DM[[3]]$mu[,-(1:7)])
colnames(DM[[4]]$mu)[7:9] <- paste0("sd:(outbound)",c("ID35224","ID61080","ID61089"))
formula[[4]] <- ~state1(d2site)
userBounds[[4]] <- list(dry=matrix(c(0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[4]] <- getFixPar(DM[[4]])
kappa[[4]] <- 4
stateNames[[4]] <- c("outbound","haulout")
inputUD[[4]] <- list(nbUD=1,parIndex=list(2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2")),UDnames="outbound",UDstates=list(1),sign=list(1))

##### 3 state models #####

modelName[[5]] <- "S=3 sigma(ID,1=2) beta(depth*slope+d2site^2) p(1=2) gamma(d2site)"
nbStates[[5]] <- 3
DM[[5]] <- list(mu=matrix(c("mu.x_tm1","langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",                 0,                  0,                       0,                   0,                    0,        0,        0,        0,0,0,
                               "mu.x_tm1",                  0,                  0,                       0,                   0,                   0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",        0,        0,        0,0,0,
                               "mu.x_tm1",                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        0,        0,        0,0,0,
                               "mu.y_tm1","langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",                 0,                  0,                       0,                   0,                    0,        0,        0,        0,0,0,
                               "mu.y_tm1",                  0,                  0,                       0,                   0,                   0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",        0,        0,        0,0,0,
                               "mu.y_tm1",                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        0,        0,        0,0,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,"ID35224","ID61080","ID61089",0,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,"ID35224","ID61080","ID61089",0,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        1,        0,        0,1,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,"ID35224","ID61080","ID61089",0,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,"ID35224","ID61080","ID61089",0,0,
                               0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        1,        0,        0,1,0,
                            0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        0,        0,        0,0,1,
                            0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        0,        0,        0,0,1,
                            0,                  0,                  0,                       0,                   0,                   0,                  0,                  0,                       0,                   0,                    0,        0,        0,        0,0,1),nrow=15,byrow=TRUE,dimnames=list(c("mean.x_1","mean.x_2","mean.x_3","mean.y_1","mean.y_2","mean.y_3","sd.x_1","sd.x_2","sd.x_3","sd.y_1","sd.y_2","sd.y_3","corr.xy_1","corr.xy_2","corr.xy_3"),
                                                                                                                                                                                                                                                                                                                            c("mean:mu_tm1","depth:outbound","slope:outbound","depthslope:outbound","d2site:outbound","d2site2:outbound","depth:inbound","slope:inbound","depthslope:inbound","d2site:inbound","d2site2:inbound","sd:(outbound)ID35224","sd:(outbound)ID61080","sd:(outbound)ID61089","sd:(haulout)","corr.xy:(Intercept)"))),
                   dry=matrix(c(1,0,
                                1,0,
                                0,1),3,2,byrow = TRUE))
formula[[5]] <- ~state1(d2site)+state2(d2site)
userBounds[[5]] <- list(dry=matrix(c(0,0.5,
                                 0,0.5,
                                 0.5,1),ncol=2,byrow=TRUE))
fixPar[[5]] <- getFixPar(DM[[5]])
kappa[[5]] <- 4
stateNames[[5]] <- c("outbound","inbound","haulout")
inputUD[[5]] <- list(nbUD=2,parIndex=list(2:6,7:11),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","inbound"),UDstates=list(1,2),sign=list(1,1))

# allow "inbound" state to have own speed
modelName[[6]] <- "S=3 sigma(ID) beta(depth*slope+d2site^2) gamma(d2site)"
nbStates[[6]] <- 3
DM[[6]] <- DM[[5]]
DM[[6]]$mu <- cbind(DM[[5]]$mu[,c(1:12)],c(rep(0,6),0,"ID35224",1,0,"ID35224",1,rep(0,3)),
                    DM[[5]]$mu[,13,drop=FALSE],c(rep(0,6),0,"ID61080",0,0,"ID61080",0,rep(0,3)),
                    DM[[5]]$mu[,14,drop=FALSE],c(rep(0,6),0,"ID61089",0,0,"ID61089",0,rep(0,3)),
                    DM[[5]]$mu[,-(1:14)])
colnames(DM[[6]]$mu)[c(13,15,17)] <- paste0("sd:(inbound)ID",c("35224","61080","61089"))
DM[[6]]$dry <- cbind(DM[[5]]$dry,c(0,1,0))
formula[[6]] <- ~state1(d2site)+state2(d2site)
userBounds[[6]] <- list(dry=matrix(c(0,0.5,
                                     0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[6]] <- getFixPar(DM[[6]])
fixPar[[6]]$beta[,nbStates[[6]]-1] <- NA
fixPar[[6]]$beta[1,nbStates[[6]]*(nbStates[[6]]-1)] <- NA
kappa[[6]] <- 4
stateNames[[6]] <- c("outbound","inbound","haulout")
inputUD[[6]] <- list(nbUD=2,parIndex=list(2:6,7:11),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","inbound"),UDstates=list(1,2),sign=list(1,1))

##### 4 state models #####

DM4 <- list(mu=matrix(c( "mu.x_tm1","langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)", "langevin(d2site2.x)",                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,        
                                "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,               
                                "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)", "langevin(d2site2.x)",        0,        0,        0,        0,        0,        0,0,0,               
                                "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,                
                                "mu.y_tm1","langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)", "langevin(d2site2.y)",                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,              
                                "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,                
                                "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)", "langevin(d2site2.y)",        0,        0,        0,        0,        0,        0,0,0,                 
                                "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,               
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,               
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224","ID35224","ID61080","ID61080","ID61089","ID61089",0,0,                 
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        1,        1,        0,        0,        0,        0,1,0,                 
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224","ID35224","ID61080","ID61080","ID61089","ID61089",0,0,                
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        1,        1,        0,        0,        0,        0,1,0,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1),nrow=20,byrow=TRUE,
                             dimnames = list(c("mean.x_1","mean.x_2","mean.x_3","mean.x_4","mean.y_1","mean.y_2","mean.y_3","mean.y_4","sd.x_1","sd.x_2","sd.x_3","sd.x_4","sd.y_1","sd.y_2","sd.y_3","sd.y_4","corr.xy_1","corr.xy_2","corr.xy_3","corr.xy_4"),
                                             c("mean:mu_tm1","depth:outbound","slope:outbound","depthslope:outbound","d2site:outbound","d2site2:outbound","depth:foraging1","slope:foraging1","depthslope:foraging1","d2site:foraging1","d2site2:foraging1","depth:inbound","slope:inbound","depthslope:inbound","d2site:inbound","d2site2:inbound","sd:(outbound)ID35224","sd:(foraging1)ID35224","sd:(outbound)ID61080","sd:(foraging1)ID61080","sd:(outbound)ID61089","sd:(foraging1)ID61089","sd:(haulout)","corr.xy:(Intercept)"))),
                   dry=matrix(c(1,0,0,
                                1,0,1,
                                1,0,0,
                                0,1,0),4,3,byrow=TRUE))

# "outbound", "foraging1", and "inbound" have same speed; "outbound" and "inbound" have opposite potential surfaces
modelName[[7]] <- "S=4 sigma(ID,1=2=3) beta(depth*slope+d2site^2,1=-3) p(1=2=3) gamma(d2site)"
nbStates[[7]] <- 4
DM[[7]] <- DM4
DM[[7]]$mu <- DM4$mu[,-c(11:16,18,20,22)]
DM[[7]]$mu[c(3,7),c(2:6)] <- gsub("langevin(","langevin(-",DM[[7]]$mu[c(1,5),c(2:6)],fixed=TRUE)
DM[[7]]$dry <- DM[[7]]$dry[,-3]
formula[[7]] <- ~state1(d2site)+state2(d2site)+state3(d2site)
userBounds[[7]] <- list(dry=matrix(c(0,0.5,
                                 0,0.5,
                                 0,0.5,
                                 0.5,1),ncol=2,byrow=TRUE))
fixPar[[7]] <- getFixPar(DM[[7]])
kappa[[7]] <- 4
stateNames[[7]] <- c("outbound","foraging","inbound","haulout")
inputUD[[7]] <- list(nbUD=3,parIndex=list(2:6,7:10,2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging","inbound"),UDstates=list(1,2,3),sign=list(1,1,-1))

# allow "foraging1" state to have own speed
modelName[[8]] <- "S=4 sigma(ID,1=3) beta(depth*slope+d2site^2,1=-3) p(1=3) gamma(d2site)"
nbStates[[8]] <- 4
DM[[8]] <- DM[[7]]
DM[[8]]$mu <- cbind(DM[[7]]$mu[,1:10],DM4$mu[,-c(1:11,12:16)])
DM[[8]]$dry <- cbind(DM[[7]]$dry,c(0,1,0,0))
formula[[8]] <- ~state1(d2site)+state2(d2site)+state3(d2site)
userBounds[[8]] <- list(dry=matrix(c(0,0.5,
                                     0,0.5,
                                     0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[8]] <- getFixPar(DM[[8]])
kappa[[8]] <- 4
stateNames[[8]] <- c("outbound","foraging","inbound","haulout")
inputUD[[8]] <- list(nbUD=3,parIndex=list(2:6,7:10,2:6),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging","inbound"),UDstates=list(1,2,3),sign=list(1,1,-1))

# "outbound", "foraging1", and "inbound" have same speed but each state has own potential surface
modelName[[9]] <- "S=4 sigma(ID,1=2=3) beta(depth*slope+d2site^2) p(1=2=3) gamma(d2site)"
nbStates[[9]] <- 4
DM[[9]] <- DM4
DM[[9]]$mu <- DM4$mu[,-c(11,18,20,22)]
DM[[9]]$dry <- DM4$dry[,-3]
formula[[9]] <- ~state1(d2site)+state2(d2site)+state3(d2site)
userBounds[[9]] <- list(dry=matrix(c(0,0.5,
                                     0,0.5,
                                     0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[9]] <- getFixPar(DM[[9]])
kappa[[9]] <- 4
stateNames[[9]] <- c("outbound","foraging","inbound","haulout")
inputUD[[9]] <- list(nbUD=3,parIndex=list(2:6,7:10,11:15),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging","inbound"),UDstates=list(1,2,3),sign=list(1,1,1))

# allow "foraging1" state to have own speed, "outbound" and "inbound" have own potential surfaces
modelName[[10]] <- "S=4 sigma(ID,1=3) beta(depth*slope+d2site^2) p(1=3) gamma(d2site)"
nbStates[[10]] <- 4
DM[[10]] <- DM4
DM[[10]]$mu <- DM4$mu[,-11]
formula[[10]] <- ~state1(d2site)+state2(d2site)+state3(d2site)
userBounds[[10]] <- list(dry=matrix(c(0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0.5,1),ncol=2,byrow=TRUE))
fixPar[[10]] <- getFixPar(DM[[10]])
kappa[[10]] <- 4
stateNames[[10]] <- c("outbound","foraging","inbound","haulout")
inputUD[[10]] <- list(nbUD=3,parIndex=list(2:6,7:10,11:15),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging","inbound"),UDstates=list(1,2,3),sign=list(1,1,1))

# "outbound", "foraging1", and "inbound" have own speed, dry, and potential surface
modelName[[11]] <- "S=4 sigma(ID) beta(depth*slope+d2site^2) gamma(d2site)"
nbStates[[11]] <- 4
DM[[11]] <- DM4
DM[[11]]$mu <- cbind(DM4$mu[,c(1:10,12:18)],c(rep(0,8),0,0,"ID35224",1,0,0,"ID35224",1,rep(0,4)),
                     DM4$mu[,19:20],c(rep(0,8),0,0,"ID61080",0,0,0,"ID61080",0,rep(0,4)),
                     DM4$mu[,21:22],c(rep(0,8),0,0,"ID61089",0,0,0,"ID61089",0,rep(0,4)),
                     DM4$mu[,-(1:22)])
colnames(DM[[11]]$mu)[c(18,21,24)] <- paste0("sd:(inbound)ID",c("35224","61080","61089"))
DM[[11]]$dry <- cbind(DM4$dry,c(0,0,1,0))
formula[[11]] <- ~state1(d2site)+state2(d2site)+state3(d2site)
userBounds[[11]] <- list(dry=matrix(c(0,0.5,
                                     0,0.5,
                                     0,0.5,
                                     0.5,1),ncol=2,byrow=TRUE))
fixPar[[11]] <- getFixPar(DM[[11]])
fixPar[[11]]$beta[,nbStates[[11]]-1] <- NA
fixPar[[11]]$beta[1,nbStates[[11]]*(nbStates[[11]]-1)] <- NA
kappa[[11]] <- 4
stateNames[[11]] <- c("outbound","foraging","inbound","haulout")
inputUD[[11]] <- list(nbUD=3,parIndex=list(2:6,7:10,11:15),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging","inbound"),UDstates=list(1,2,3),sign=list(1,1,1))

##### 5 state models #####
DM5 <- list(mu=matrix(c( "mu.x_tm1","langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)", "langevin(d2site2.x)",                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,        
                                    "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,      
                                    "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)","langevin(d2site2.x)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,               
                                    "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,"langevin(depth.x)","langevin(slope.x)","langevin(depthslope.x)","langevin(d2site.x)", "langevin(d2site2.x)",        0,        0,        0,        0,        0,        0,0,0,               
                                    "mu.x_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,                
                                    "mu.y_tm1","langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)", "langevin(d2site2.y)",                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,              
                                    "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0, 
                                    "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)","langevin(d2site2.y)",                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,                
                                    "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,"langevin(depth.y)","langevin(slope.y)","langevin(depthslope.y)","langevin(d2site.y)", "langevin(d2site2.y)",        0,        0,        0,        0,        0,        0,0,0,                 
                                    "mu.y_tm1",                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,0,               
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,               
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224","ID35224","ID61080","ID61080","ID61089","ID61089",0,0,   
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,  
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        1,        1,        0,        0,        0,        0,1,0,                 
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224","ID35224","ID61080","ID61080","ID61089","ID61089",0,0,  
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,  
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,"ID35224",        0,"ID61080",        0,"ID61089",        0,0,0,                 
                                    0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        1,        1,        0,        0,        0,        0,1,0,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,  
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1,                 
                         0,                  0,                  0,                       0,                   0,                     0,                  0,                  0,                       0,                   0,                    0,                  0,                  0,                       0,                   0,                     0,        0,        0,        0,        0,        0,        0,0,1),nrow=25,byrow=TRUE,
                                 dimnames = list(c("mean.x_1","mean.x_2","mean.x_3","mean.x_4","mean.x_5","mean.y_1","mean.y_2","mean.y_3","mean.y_4","mean.y_5","sd.x_1","sd.x_2","sd.x_3","sd.x_4","sd.x_5","sd.y_1","sd.y_2","sd.y_3","sd.y_4","sd.y_5","corr.xy_1","corr.xy_2","corr.xy_3","corr.xy_4","corr.xy_5"),
                                                 c("mean:mu_tm1","depth:outbound","slope:outbound","depthslope:outbound","d2site:outbound","d2site2:outbound","depth:foraging1","slope:foraging1","depthslope:foraging1","d2site:foraging1","d2site2:foraging1","depth:inbound","slope:inbound","depthslope:inbound","d2site:inbound","d2site2:inbound","sd:(outbound)ID35224","sd:(foraging1)ID35224","sd:(outbound)ID61080","sd:(foraging1)ID61080","sd:(outbound)ID61089","sd:(foraging1)ID61089","sd:(haulout)","corr.xy:(Intercept)"))),
                       dry=matrix(c(1,0,0,
                                    1,0,1,
                                    1,0,0,
                                    1,0,0,
                                    0,1,0),5,3,byrow=TRUE))

# 5 state model where two "foraging" states share same potential surface
modelName[[12]] <- "S=5 sigma(ID,1=3=4) beta(depth*slope+d2site^2,2=3) p(1=3=4) gamma(d2site)"
nbStates[[12]] <- 5
DM[[12]] <- DM5
DM[[12]]$mu <- DM5$mu[,-11]
formula[[12]] <- ~state1(d2site)+state2(d2site)+state3(d2site)+state4(d2site)
userBounds[[12]] <- list(dry=matrix(c(0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0.5,1),ncol=2,byrow=TRUE))
fixPar[[12]] <- getFixPar(DM[[12]])
kappa[[12]] <- 4
stateNames[[12]] <- c("outbound","foraging1","foraging2","inbound","haulout")
inputUD[[12]] <- list(nbUD=4,parIndex=list(2:6,7:10,7:10,11:15),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging1","foraging2","inbound"),UDstates=list(1,2,3,4),sign=c(1,1,1,1))

# two "foraging" states have own speeds
modelName[[13]] <- "S=5 sigma(ID,1=4) beta(depth*slope+d2site^2,2=3) p(1=4) gamma(d2site)"
nbStates[[13]] <- 5
DM[[13]] <- DM[[12]] 
DM[[13]]$mu <- cbind(DM[[12]]$mu[,1:17],c(rep(0,10),0,0,"ID35224",0,1,0,0,"ID35224",0,1,rep(0,5)),
                          DM[[12]]$mu[,18:19],c(rep(0,10),0,0,"ID61080",0,0,0,0,"ID61080",0,0,rep(0,5)),
                          DM[[12]]$mu[,20:21],c(rep(0,10),0,0,"ID61089",0,0,0,0,"ID61089",0,0,rep(0,5)),
                          DM[[12]]$mu[,-(1:21)])
colnames(DM[[13]]$mu)[c(18,21,24)] <- paste0("sd:(foraging2)ID",c("35224","61080","61089"))
DM[[13]]$dry <- cbind(DM[[13]]$dry,c(0,0,1,0,0))
formula[[13]] <- ~state1(d2site)+state2(d2site)+state3(d2site)+state4(d2site)
userBounds[[13]] <- list(dry=matrix(c(0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0.5,1),ncol=2,byrow=TRUE))
fixPar[[13]] <- getFixPar(DM[[13]])
kappa[[13]] <- 4
stateNames[[13]] <- c("outbound","foraging1","foraging2","inbound","haulout")
inputUD[[13]] <- list(nbUD=4,parIndex=list(2:6,7:10,7:10,11:15),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging1","foraging2","inbound"),UDstates=list(1,2,3,4),sign=c(1,1,1,1))

# two "foraging" states have own potential surface
modelName[[14]] <- "S=5 sigma(ID,1=3=4) beta(depth*slope+d2site^2) p(1=3=4) gamma(d2site)"
nbStates[[14]] <- 5
DM[[14]] <- DM[[12]]
DM[[14]]$mu <- cbind(DM[[12]]$mu[,c(1:6,7:10,7:10,11:18)],
                     DM[[12]]$mu[,-(1:18)])
colnames(DM[[14]]$mu)[11:14] <- gsub("foraging1","foraging2",colnames(DM[[12]]$mu)[7:10])
DM[[14]]$mu[c(3,8),7:10] <- 0
DM[[14]]$mu[c(2,7),11:14] <- 0
formula[[14]] <- ~state1(d2site)+state2(d2site)+state3(d2site)+state4(d2site)
userBounds[[14]] <- list(dry=matrix(c(0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0.5,1),ncol=2,byrow=TRUE))
fixPar[[14]] <- getFixPar(DM[[14]])
kappa[[14]] <- 4
stateNames[[14]] <- c("outbound","foraging1","foraging2","inbound","haulout")
inputUD[[14]] <- list(nbUD=4,parIndex=list(2:6,7:10,11:14,15:19),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging1","foraging2","inbound"),UDstates=list(1,2,3,4),sign=c(1,1,1,1))

# kitchen sink model where all 5 states have own speed, potential surface, and dry parameters
modelName[[15]] <- "S=5 sigma(ID) beta(depth*slope+d2site^2) gamma(d2site)"
nbStates[[15]] <- 5
DM[[15]] <- DM[[13]]
DM[[15]]$mu <- cbind(DM[[13]]$mu[,c(1:6,7:10,7:10,11:18)],c(rep(0,10),0,0,0,"ID35224",1,0,0,0,"ID35224",1,rep(0,5)),
                        DM[[13]]$mu[,19:21],c(rep(0,10),0,0,0,"ID61080",0,0,0,0,"ID61080",0,rep(0,5)),
                        DM[[13]]$mu[,22:24],c(rep(0,10),0,0,0,"ID61089",0,0,0,0,"ID61089",0,rep(0,5)),
                        DM[[13]]$mu[,-(1:24)])
colnames(DM[[15]]$mu)[c(23,27,31)] <- paste0("sd:(inbound)ID",c("35224","61080","61089"))
colnames(DM[[15]]$mu)[11:14] <- gsub("foraging1","foraging2",colnames(DM[[15]]$mu)[11:14])
DM[[15]]$mu[c(3,8),7:10] <- 0
DM[[15]]$mu[c(2,7),11:14] <- 0
DM[[15]]$dry <- cbind(DM[[15]]$dry,c(0,0,0,1,0))
formula[[15]] <- ~state1(d2site)+state2(d2site)+state3(d2site)+state4(d2site)
userBounds[[15]] <- list(dry=matrix(c(0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0,0.5,
                                      0.5,1),ncol=2,byrow=TRUE))
fixPar[[15]] <- getFixPar(DM[[15]])
kappa[[15]] <- 4
stateNames[[15]] <- c("outbound","foraging1","foraging2","inbound","haulout")
inputUD[[15]] <- list(nbUD=4,parIndex=list(2:6,7:10,11:14,15:19),covNames=list(c("depth","slope","depthslope","d2site","d2site2"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site"),c("depth","slope","depthslope","d2site","d2site2")),UDnames=c("outbound","foraging1","foraging2","inbound"),UDstates=list(1,2,3,4),sign=c(1,1,1,1))

workBounds <- lapply(DM,getWorkBounds)
prior <- mapply(function(x) getPrior(fixPar[[x]]$beta,DM[[x]],sd=10),1:length(DM))
