# libraries and functions
#library('ggplot2')


# compute approximate experiment sizes
# see http://www.win-vector.com/blog/2013/12/sample-size-and-power-for-rare-events/
# valueTab: table with at least columns (Probability,ValueSuccess)
heuristicPowerPlan <- function(valueTab,errorProbability=0.05,relativeError=0.2) {
  meanValues <- valueTab$Probability*valueTab$ValueSuccess
  big <- max(meanValues)
  slop <- pmin(relativeError*big/valueTab$ValueSuccess,1)
  ceiling(-log(errorProbability)*valueTab$Probability/(slop^2))
}


# build a typical table (assume parameters are dead-on)
# valueTab: table with at least columns (Label,Probability,ValueSuccess)
typicalTable <- function(valueTab,sizes) {
  n <- nrow(valueTab)
  sizes <- round(sizes)
  Successes <- vapply(seq_len(n),
                      function(i) {rbinom(1,
                                          size=sizes[i],
                                          prob=valueTab$Probability[i])},
                      numeric(1))
  data.frame(Label=valueTab$Label,
             Actions=sizes,
             Successes=Successes,
             ValueSuccess=valueTab$ValueSuccess)
}


# compute probilities of observations given parameters
# valueTab: table with at least columns (Label,Probability,ValueSuccess)
sampleGraph <- function(tab,sizes,epsilon=1.0e-3) {
  sList <- lapply(seq_len(nrow(tab)),
    function(i) {
      lowWater <- max(0,floor(qbinom(epsilon,
                                     size=sizes[i],
                                     prob=tab$Probability[i])))
      highWater <- min(sizes[i],ceiling(qbinom(1.0-epsilon,
                                               size=sizes[i],
                                               prob=tab$Probability[i])))
      if(highWater-lowWater>500) {
        counts <- sort(unique(round(seq(lowWater,highWater,length.out=401))))
      } else {
        counts <- lowWater:highWater
      }
      nc <- length(counts)
      # successes observed
      probs <- dbinom(counts,
                      size=sizes[i],
                      prob=tab$Probability[i])
      data.frame(density=sizes[i]*probs/tab$ValueSuccess[i],
                 value=counts*tab$ValueSuccess[i]/sizes[i],
                 Label=tab$Label[i])
    })
  do.call('rbind',sList)
}

# given a single intensity plot possible observations
# planTab: table with at least columns (Label,Probability,ValueSuccess)
# p: sampleGraph(planTab)
computeProbsGES <- function(planTab,p,computeLoss=FALSE) {
  # compute some odds
  planTab$e <- planTab$Probability*planTab$ValueSuccess
  deals <- sort(unique(p$Label))
  combs <- combn(deals,2)
  pG <- c()
  for(j in seq_len(ncol(combs))) {
    d1 <- combs[1,j]
    d2 <- combs[2,j]
    g1 <- p[p$Label==d1,,drop=FALSE]
    g2 <- p[p$Label==d2,,drop=FALSE]
    e1 <- planTab$e[planTab$Label==d1]
    e2 <- planTab$e[planTab$Label==d2]
    oneBest <- e1>=e2
    absE <- abs(e1-e2)
    totProb <- 0
    totInd <- 0
    totLoss <- 0
    for(i1 in 1:nrow(g1)) {
      probii <- g1[i1,'density']*g2[,'density']
      condii <- g1[i1,'value']>=g2[,'value']
      # idea for loass is: each time the wrong campaign seems best
      # we lose the ideal difference in expecations
      lossii <- ifelse(condii==oneBest,0,absE)
      totLoss <- totLoss + sum(lossii*probii)
      totProb <- totProb + sum(probii)
      totInd <- totInd + sum(ifelse(condii,probii,0))
    }
    p1Greater2 <- totInd/totProb
    campaignSwitchExposure <- totLoss/totProb
    pG <- rbind(pG,data.frame(Deal1=d1,
                              Deal2=d2,
                              p1Greater2=p1Greater2
    ))
    if(computeLoss) {
      pG$campaignSwitchExposure=campaignSwitchExposure
    }
  }
  pG
}


wQuantile <- function(values,weights,cut) {
  if(cut<=0) {
    return(min(values))
  }
  if(cut>=1) {
    return(max(values))
  }
  badIndices <- is.na(weights) | !is.finite(weights)
  values <- values[!badIndices]
  weights <- weights[!badIndices]
  perm <- order(values)
  values <- values[perm]
  weights <- weights[perm]
  total <- sum(weights)
  running <- cumsum(weights)
  idx <- match(TRUE,running>=cut*total)
  if(is.na(idx)) {
    return(max(values))
  }
  values[idx]
}

plotSample <- function(g) {
  ggplot() +
    geom_line(data=g,aes(x=value,y=density,color=Label)) +
    xlab('Expected Value per Action (dollars)') +
    scale_x_continuous(limits = c(wQuantile(g$value,g$density,0.001),
                                  wQuantile(g$value,g$density,0.999))) +
    ggtitle('distribution of possible outcomes')
}

# build detailed curves of posterior intensity estimates
# tab: an experiment table with our standard columns (Label,Actions,Successes,ValueSuccess)
posteriorGraph <- function(tab,epsilon=1.0e-4) {
  p <- c()
  s <- c()
  for(i in seq_len(nrow(tab))) {
    # ai, bi posterior beta paremers assume prior a0=0.5,b0=0.5 (Jeffreys)
    ai <- 0.5 + tab$Successes[i]
    bi <- 0.5 + tab$Actions[i] - tab$Successes[i]
    lowWater <- qbeta(epsilon,shape1=ai,shape2=bi)
    highWater <- qbeta(1-epsilon,shape1=ai,shape2=bi)
    # if boundaries near 0/1, push them to 0/1
    width <- highWater-lowWater
    if(lowWater-width/4<=0) {
      lowWater <- 0
    }
    if(highWater+width/4>=1) {
      highWater <- 1
    }
    intensities <- seq(lowWater,highWater,(highWater-lowWater)/201)
    pi <- data.frame(density=dbeta(intensities,shape1=ai,shape2=bi)/tab$ValueSuccess[i],
                     value=intensities*tab$ValueSuccess[i],
                     Label=tab$Label[i])
    # replace infinities with notionally large numbers (makes certain graphs not degenerate)
    infiniteIndices <- is.infinite(pi$density)
    maxFinite <- max(pi$density[!infiniteIndices])
    pi$density[infiniteIndices] <- 2*maxFinite + 1
    # replace zero probabilities with very low value (Cromwell's rule type smoothing idea, 
    # with the Jeffreys prior should only happen at intensity = 0,1
    minPos <- min(pi$density[pi$density>0])
    pi$density <- pmax(pi$density,1.0e-8*minPos)
    p <- rbind(p,pi)
    medIntensity <- qbeta(0.5,shape1=ai,shape2=bi)
    meanIntensity <- (ai/(ai+bi))
    if((ai>1)&&(bi>1)) {
      modeIntensity <- ((ai-1)/(ai+bi-2))
    } else {
      modeIntensity <- pi$value[which.max(pi$density)]
    }
    si <- data.frame(median=medIntensity*tab$ValueSuccess[i],
                     mean=meanIntensity*tab$ValueSuccess[i],
                     mode=modeIntensity*tab$ValueSuccess[i],
                     Label=tab$Label[i])
    si$medianY <- dbeta(medIntensity,shape1=ai,shape2=bi)/tab$ValueSuccess[i]
    si$meanY <- dbeta(meanIntensity,shape1=ai,shape2=bi)/tab$ValueSuccess[i]
    si$modeY <- dbeta(modeIntensity,shape1=ai,shape2=bi)/tab$ValueSuccess[i]
    s <- rbind(s,si)
  }
  list(graph=p,summary=s)
}

plotPosterior <- function(p) {
  ggplot() +
    geom_line(data=p$graph,aes(x=value,y=density,color=Label)) +
    xlab('Action To Success Value (dollars)') +
    geom_vline(data=p$summary,aes(xintercept=median,color=Label)) +
    geom_point(data=p$summary,aes(x=mean,y=meanY,color=Label),shape=3) +
    geom_point(data=p$summary,aes(x=mode,y=modeY,color=Label),sahpe=2) +
    scale_x_continuous(limits = c(wQuantile(p$graph$value,p$graph$density,0.001),
                                  wQuantile(p$graph$value,p$graph$density,0.999))) +
    ggtitle('Bayesian posterior action to success value estimates')
}

# given a single outcome plot posterior probabilities of unknown intensities
# tab: an experiment table with our standard columns (Label,Actions,Successes,ValueSuccess)
# p: posteriorGraph(tab)
computeProbsGEP <- function(tab,p,computeLoss=TRUE) {
  # compute some odds
  tab$e <- tab$Successes*tab$ValueSuccess/tab$Actions
  deals <- sort(unique(p$Label))
  combs <- combn(deals,2)
  pG <- c()
  for(j in seq_len(ncol(combs))) {
    d1 <- combs[1,j]
    d2 <- combs[2,j]
    g1 <- p[p$Label==d1,,drop=FALSE]
    g2 <- p[p$Label==d2,,drop=FALSE]
    e1 <- tab$e[tab$Label==d1]
    e2 <- tab$e[tab$Label==d2]
    oneBest <- e1>=e2
    totProb <- 0
    totInd <- 0
    totLoss <- 0
    for(i1 in 1:nrow(g1)) {
      probii <- g1[i1,'density']*g2[,'density']
      condii <- g1[i1,'value']>=g2[,'value']
      totProb <- totProb + sum(probii)
      totInd <- totInd + sum(ifelse(condii,probii,0))
      # idea for loss is: each time the unknown intensity of the unchosen
      # campaign is not the highest we lose the current difference.
      lossii <- ifelse(condii==oneBest,0,abs(g1[i1,'value']-g2[,'value']))
      totLoss <- totLoss + sum(lossii*probii)
    }
    p1Greater2 <- totInd/totProb
    expectedDecisionLoss <- totLoss/totProb
     pG <- rbind(pG,data.frame(Deal1=d1,
                              Deal2=d2,
                              p1Greater2=p1Greater2
                              ))
    if(computeLoss) {
      pG$expectedDecisionLoss=expectedDecisionLoss
    }
  }
  pG
}

# p: a posterior graph
# l1,l2: Labels
build2dProbGraph <- function(p,l1,l2) {
  g1 <- p[p$Label==l1,,drop=FALSE]
  g2 <- p[p$Label==l2,,drop=FALSE]
  n1 <- nrow(g1)
  n2 <- nrow(g2)
  d_i1=integer(n1*n2)
  d_i2=integer(n1*n2)
  d_v1=numeric(n1*n2)
  d_v2=numeric(n1*n2)
  d_d=numeric(n1*n2)
  idx <- 1
  for(i1 in 1:n1) {
    v1 <- g1[i1,'value']
    for(i2 in 1:n2) {
      v2 <- g2[i2,'value']
      d_i1[idx] <- i1
      d_i2[idx] <- i2
      d_v1[idx] <- v1
      d_v2[idx] <- v2
      d_d[idx] <- g1[i1,'density']*g2[i2,'density']
      idx <- idx + 1
    }
  }
  data.frame(i1=d_i1,
             i2=d_i2,
             v1=d_v1,
             v2=d_v2,
             d=d_d)
}

