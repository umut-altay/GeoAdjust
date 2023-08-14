# Calculating the range and the marginal variance from the estimated log_tau and log_kappa :

load("realData_Results_CR.RData")
log_tau = Results_CR[["fixed.par"]][["log_tau"]]
log_kappa = Results_CR[["fixed.par"]][["log_kappa"]]

rangetrue = sqrt(8.0)/exp(log_kappa)
sigmatrue = 1.0 / sqrt(4.0 * 3.14159265359 *
                         exp(2.0 * log_tau) * exp(2.0 * log_kappa))


# > coeffs = Results_CR[["mu"]][1:6]
# > coeffs
# beta        beta        beta        beta        beta        beta 
# -2.21515528  0.62318518 -0.43524190 -0.02390748  0.33002205 -1.35660048 

# betas scaled by 0.5, 1 and 1.5 :
betas = rbind(c(-1.10757764, 0.31159259, -0.21762095, -0.01195374,  0.16501102, -0.67830024),
              c(-2.215155, 0.6231852, -0.4352419, -0.02390748, 0.3300220, -1.3566005),
              c(-3.32273250, 0.93477780, -0.65286285, -0.03586122, 0.49503300, -2.03490075))


nBeta = 3 # of different sets of beta values that are used to simulate data
nBoundarySc = 1 # of boundary options that we used
nLikelihood = 1 # of different likelihoods that we used
nRange = 1 # of different range values that we used  
nScale = 1 # of different jittering scales that we used 
nSim = 50 # of simulations we have for each scenario


# the models that we want to extract the results for (on/off-->1/0) :
flagNN = 1
flagC = 0
flagR = 0
flagCR = 1
flagNNsmoothed = 1

if (flagNN ==1){
  crpsNN = list()
  rmseNN = list()
  logscoreNN = list()
  coverageNN = list()
  BIASNN = list()
  beta0BiasNN = list()
  beta1BiasNN = list()
  beta2BiasNN = list()
  beta3BiasNN = list()
  beta4BiasNN = list()
  beta5BiasNN = list()
  rangeBiasNN = list()
  sigmaBiasNN = list()
  averageCIlengthNN = list()
}

if (flagNNsmoothed ==1){
  crpsNNsmoothed = list()
  rmseNNsmoothed = list()
  logscoreNNsmoothed = list()
  coverageNNsmoothed = list()
  BIASNNsmoothed = list()
  beta0BiasNNsmoothed = list()
  beta1BiasNNsmoothed = list()
  beta2BiasNNsmoothed = list()
  beta3BiasNNsmoothed = list()
  beta4BiasNNsmoothed = list()
  beta5BiasNNsmoothed = list()
  rangeBiasNNsmoothed = list()
  sigmaBiasNNsmoothed = list()
  averageCIlengthNNsmoothed = list()
}
if (flagC ==1){
  crpsC = list()
  rmseC = list()
  logscoreC = list()
  coverageC = list()
  BIASC = list()
  beta0BiasC = list()
  beta1BiasC = list()
  beta2BiasC = list()
  beta3BiasC = list()
  beta4BiasC = list()
  beta5BiasC = list()
  rangeBiasC = list()
  sigmaBiasC = list()
  averageCIlengthC = list()
}
if (flagR ==1){
  crpsR = list()
  rmseR = list()
  logscoreR = list()
  coverageR = list()
  BIASR = list()
  beta0BiasR = list()
  beta1BiasR = list()
  beta2BiasR = list()
  beta3BiasR = list()
  beta4BiasR = list()
  beta5BiasR = list()
  rangeBiasR = list()
  sigmaBiasR = list()
  averageCIlengthR =list()
}
if (flagCR ==1){
  crpsCR = list()
  rmseCR = list()
  logscoreCR = list()
  coverageCR = list()
  BIASCR = list()
  beta0BiasCR = list()
  beta1BiasCR = list()
  beta2BiasCR = list()
  beta3BiasCR = list()
  beta4BiasCR = list()
  beta5BiasCR = list()
  rangeBiasCR = list()
  sigmaBiasCR = list()
  averageCIlengthCR = list()
}


for(k in 1:nBeta){
  if (flagNN ==1){
    crpsNN_temp1 = list()
    rmseNN_temp1 = list()
    logscoreNN_temp1 = list()
    coverageNN_temp1 = list()
    BIASNN_temp1 = list()
    beta0BiasNN_temp1 = list()
    beta1BiasNN_temp1 = list()
    beta2BiasNN_temp1 = list()
    beta3BiasNN_temp1 = list()
    beta4BiasNN_temp1 = list()
    beta5BiasNN_temp1 = list()
    rangeBiasNN_temp1 = list()
    sigmaBiasNN_temp1 = list()
    averageCIlengthNN_temp1 = list()
  }
  
  if (flagNNsmoothed ==1){
    crpsNNsmoothed_temp1 = list()
    rmseNNsmoothed_temp1 = list()
    logscoreNNsmoothed_temp1 = list()
    coverageNNsmoothed_temp1 = list()
    BIASNNsmoothed_temp1 = list()
    beta0BiasNNsmoothed_temp1 = list()
    beta1BiasNNsmoothed_temp1 = list()
    beta2BiasNNsmoothed_temp1 = list()
    beta3BiasNNsmoothed_temp1 = list()
    beta4BiasNNsmoothed_temp1 = list()
    beta5BiasNNsmoothed_temp1 = list()
    rangeBiasNNsmoothed_temp1 = list()
    sigmaBiasNNsmoothed_temp1 = list()
    averageCIlengthNNsmoothed_temp1 = list()
  }
  
  if (flagC ==1){
    crpsC_temp1 = list()
    rmseC_temp1 = list()
    logscoreC_temp1 = list()
    coverageC_temp1 = list()
    BIASC_temp1 = list()
    beta0BiasC_temp1 = list()
    beta1BiasC_temp1 = list()
    beta2BiasC_temp1 = list()
    beta3BiasC_temp1 = list()
    beta4BiasC_temp1 = list()
    beta5BiasC_temp1 = list()
    rangeBiasC_temp1 = list()
    sigmaBiasC_temp1 = list()
    averageCIlengthC_temp1 = list()
  }
  if (flagR ==1){
    crpsR_temp1 = list()
    rmseR_temp1 = list()
    logscoreR_temp1 = list()
    coverageR_temp1 = list()
    BIASR_temp1 = list()
    beta0BiasR_temp1 = list()
    beta1BiasR_temp1 = list()
    beta2BiasR_temp1 = list()
    beta3BiasR_temp1 = list()
    beta4BiasR_temp1 = list()
    beta5BiasR_temp1 = list()
    rangeBiasR_temp1 = list()
    sigmaBiasR_temp1 = list()
    averageCIlengthR_temp1 = list()
  }
  if (flagCR ==1){
    crpsCR_temp1 = list()
    rmseCR_temp1 = list()
    logscoreCR_temp1 = list()
    coverageCR_temp1 = list()
    BIASCR_temp1 = list()
    beta0BiasCR_temp1 = list()
    beta1BiasCR_temp1 = list()
    beta2BiasCR_temp1 = list()
    beta3BiasCR_temp1 = list()
    beta4BiasCR_temp1 = list()
    beta5BiasCR_temp1 = list()
    rangeBiasCR_temp1 = list()
    sigmaBiasCR_temp1 = list()
    averageCIlengthCR_temp1 = list()
  }
  
  for (g in 1:nBoundarySc){
    if (flagNN ==1){
      crpsNN_temp2 = list()
      rmseNN_temp2 = list()
      logscoreNN_temp2 = list()
      coverageNN_temp2 = list()
      BIASNN_temp2 = list()
      beta0BiasNN_temp2 = list()
      beta1BiasNN_temp2 = list()
      beta2BiasNN_temp2 = list()
      beta3BiasNN_temp2 = list()
      beta4BiasNN_temp2 = list()
      beta5BiasNN_temp2 = list()
      rangeBiasNN_temp2 = list()
      sigmaBiasNN_temp2 = list()
      averageCIlengthNN_temp2 = list()
    }
    
    if (flagNNsmoothed ==1){
      crpsNNsmoothed_temp2 = list()
      rmseNNsmoothed_temp2 = list()
      logscoreNNsmoothed_temp2 = list()
      coverageNNsmoothed_temp2 = list()
      BIASNNsmoothed_temp2 = list()
      beta0BiasNNsmoothed_temp2 = list()
      beta1BiasNNsmoothed_temp2 = list()
      beta2BiasNNsmoothed_temp2 = list()
      beta3BiasNNsmoothed_temp2 = list()
      beta4BiasNNsmoothed_temp2 = list()
      beta5BiasNNsmoothed_temp2 = list()
      rangeBiasNNsmoothed_temp2 = list()
      sigmaBiasNNsmoothed_temp2 = list()
      averageCIlengthNNsmoothed_temp2 = list()
    }
    
    if (flagC ==1){
      crpsC_temp2 = list()
      rmseC_temp2 = list()
      logscoreC_temp2 = list()
      coverageC_temp2 = list()
      BIASC_temp2 = list()
      beta0BiasC_temp2 = list()
      beta1BiasC_temp2 = list()
      beta2BiasC_temp2 = list()
      beta3BiasC_temp2 = list()
      beta4BiasC_temp2 = list()
      beta5BiasC_temp2 = list()
      rangeBiasC_temp2 = list()
      sigmaBiasC_temp2 = list()
      averageCIlengthC_temp2 = list()
    }
    if (flagR ==1){
      crpsR_temp2 = list()
      rmseR_temp2 = list()
      logscoreR_temp2 = list()
      coverageR_temp2 = list()
      BIASR_temp2 = list()
      beta0BiasR_temp2 = list()
      beta1BiasR_temp2 = list()
      beta2BiasR_temp2 = list()
      beta3BiasR_temp2 = list()
      beta4BiasR_temp2 = list()
      beta5BiasR_temp2 = list()
      rangeBiasR_temp2 = list()
      sigmaBiasR_temp2 = list()
      averageCIlengthR_temp2 = list()
    }
    if (flagCR ==1){
      crpsCR_temp2 = list()
      rmseCR_temp2 = list()
      logscoreCR_temp2 = list()
      coverageCR_temp2 = list()
      BIASCR_temp2 = list()
      beta0BiasCR_temp2 = list()
      beta1BiasCR_temp2 = list()
      beta2BiasCR_temp2 = list()
      beta3BiasCR_temp2 = list()
      beta4BiasCR_temp2 = list()
      beta5BiasCR_temp2 = list()
      rangeBiasCR_temp2 = list()
      sigmaBiasCR_temp2 = list()
      averageCIlengthCR_temp2 = list()
    }
    
    for (i in 1:nLikelihood){
      if (flagNN ==1){
        crpsNN_temp3 = list()
        rmseNN_temp3 = list()
        logscoreNN_temp3 = list()
        coverageNN_temp3 = list()
        BIASNN_temp3 = list()
        beta0BiasNN_temp3 = list()
        beta1BiasNN_temp3 = list()
        beta2BiasNN_temp3 = list()
        beta3BiasNN_temp3 = list()
        beta4BiasNN_temp3 = list()
        beta5BiasNN_temp3 = list()
        rangeBiasNN_temp3 = list()
        sigmaBiasNN_temp3 = list()
        averageCIlengthNN_temp3 = list()
      }
      
      
      if (flagNNsmoothed ==1){
        crpsNNsmoothed_temp3 = list()
        rmseNNsmoothed_temp3 = list()
        logscoreNNsmoothed_temp3 = list()
        coverageNNsmoothed_temp3 = list()
        BIASNNsmoothed_temp3 = list()
        beta0BiasNNsmoothed_temp3 = list()
        beta1BiasNNsmoothed_temp3 = list()
        beta2BiasNNsmoothed_temp3 = list()
        beta3BiasNNsmoothed_temp3 = list()
        beta4BiasNNsmoothed_temp3 = list()
        beta5BiasNNsmoothed_temp3 = list()
        rangeBiasNNsmoothed_temp3 = list()
        sigmaBiasNNsmoothed_temp3 = list()
        averageCIlengthNNsmoothed_temp3 = list()
      }
      
      if (flagC ==1){
        crpsC_temp3 = list()
        rmseC_temp3 = list()
        logscoreC_temp3 = list()
        coverageC_temp3 = list()
        BIASC_temp3 = list()
        beta0BiasC_temp3 = list()
        beta1BiasC_temp3 = list()
        beta2BiasC_temp3 = list()
        beta3BiasC_temp3 = list()
        beta4BiasC_temp3 = list()
        beta5BiasC_temp3 = list()
        rangeBiasC_temp3 = list()
        sigmaBiasC_temp3 = list()
        averageCIlengthC_temp3 = list()
      }
      if (flagR ==1){
        crpsR_temp3 = list()
        rmseR_temp3 = list()
        logscoreR_temp3 = list()
        coverageR_temp3 = list()
        BIASR_temp3 = list()
        beta0BiasR_temp3 = list()
        beta1BiasR_temp3 = list()
        beta2BiasR_temp3 = list()
        beta3BiasR_temp3 = list()
        beta4BiasR_temp3 = list()
        beta5BiasR_temp3 = list()
        rangeBiasR_temp3 = list()
        sigmaBiasR_temp3 = list()
        averageCIlengthR_temp3 = list()
      }
      if (flagCR ==1){
        crpsCR_temp3 = list()
        rmseCR_temp3 = list()
        logscoreCR_temp3 = list()
        coverageCR_temp3 = list()
        BIASCR_temp3 = list()
        beta0BiasCR_temp3 = list()
        beta1BiasCR_temp3 = list()
        beta2BiasCR_temp3 = list()
        beta3BiasCR_temp3 = list()
        beta4BiasCR_temp3 = list()
        beta5BiasCR_temp3 = list()
        rangeBiasCR_temp3 = list()
        sigmaBiasCR_temp3 = list()
        averageCIlengthCR_temp3 = list()
      }
      
      for (j in 1:nRange){
        if (flagNN ==1){
          crpsNN_temp4 = list()
          rmseNN_temp4 = list()
          logscoreNN_temp4 = list()
          coverageNN_temp4 = list()
          BIASNN_temp4 = list()
          beta0BiasNN_temp4 = list()
          beta1BiasNN_temp4 = list()
          beta2BiasNN_temp4 = list()
          beta3BiasNN_temp4 = list()
          beta4BiasNN_temp4 = list()
          beta5BiasNN_temp4 = list()
          rangeBiasNN_temp4 = list()
          sigmaBiasNN_temp4 = list()
          averageCIlengthNN_temp4 = list()
        }
        
        if (flagNNsmoothed ==1){
          crpsNNsmoothed_temp4 = list()
          rmseNNsmoothed_temp4 = list()
          logscoreNNsmoothed_temp4 = list()
          coverageNNsmoothed_temp4 = list()
          BIASNNsmoothed_temp4 = list()
          beta0BiasNNsmoothed_temp4 = list()
          beta1BiasNNsmoothed_temp4 = list()
          beta2BiasNNsmoothed_temp4 = list()
          beta3BiasNNsmoothed_temp4 = list()
          beta4BiasNNsmoothed_temp4 = list()
          beta5BiasNNsmoothed_temp4 = list()
          rangeBiasNNsmoothed_temp4 = list()
          sigmaBiasNNsmoothed_temp4 = list()
          averageCIlengthNNsmoothed_temp4 = list()
        }
        
        if (flagC ==1){
          crpsC_temp4 = list()
          rmseC_temp4 = list()
          logscoreC_temp4 = list()
          coverageC_temp4 = list()
          BIASC_temp4 = list()
          beta0BiasC_temp4 = list()
          beta1BiasC_temp4 = list()
          beta2BiasC_temp4 = list()
          beta3BiasC_temp4 = list()
          beta4BiasC_temp4 = list()
          beta5BiasC_temp4 = list()
          rangeBiasC_temp4 = list()
          sigmaBiasC_temp4 = list()
          averageCIlengthC_temp4 = list()
        }
        if (flagR ==1){
          crpsR_temp4 = list()
          rmseR_temp4 = list()
          logscoreR_temp4 = list()
          coverageR_temp4 = list()
          BIASR_temp4 = list()
          beta0BiasR_temp4 = list()
          beta1BiasR_temp4 = list()
          beta2BiasR_temp4 = list()
          beta3BiasR_temp4 = list()
          beta4BiasR_temp4 = list()
          beta5BiasR_temp4 = list()
          rangeBiasR_temp4 = list()
          sigmaBiasR_temp4 = list()
          averageCIlengthR_temp4 = list()
        }
        if (flagCR ==1){
          crpsCR_temp4 = list()
          rmseCR_temp4 = list()
          logscoreCR_temp4 = list()
          coverageCR_temp4 = list()
          BIASCR_temp4 = list()
          beta0BiasCR_temp4 = list()
          beta1BiasCR_temp4 = list()
          beta2BiasCR_temp4 = list()
          beta3BiasCR_temp4 = list()
          beta4BiasCR_temp4 = list()
          beta5BiasCR_temp4 = list()
          rangeBiasCR_temp4 = list()
          sigmaBiasCR_temp4 = list()
          averageCIlengthCR_temp4 = list()
        }
        for (h in 1:nScale){
          if (flagNN ==1){
            crpsNN_temp5 = list()
            rmseNN_temp5 = list()
            logscoreNN_temp5 = list()
            coverageNN_temp5 = list()
            BIASNN_temp5 = list()
            beta0BiasNN_temp5 = list()
            beta1BiasNN_temp5 = list()
            beta2BiasNN_temp5 = list()
            beta3BiasNN_temp5 = list()
            beta4BiasNN_temp5 = list()
            beta5BiasNN_temp5 = list()
            rangeBiasNN_temp5 = list()
            sigmaBiasNN_temp5 = list()
            averageCIlengthNN_temp5 = list()
          }
          
          if (flagNNsmoothed ==1){
            crpsNNsmoothed_temp5 = list()
            rmseNNsmoothed_temp5 = list()
            logscoreNNsmoothed_temp5 = list()
            coverageNNsmoothed_temp5 = list()
            BIASNNsmoothed_temp5 = list()
            beta0BiasNNsmoothed_temp5 = list()
            beta1BiasNNsmoothed_temp5 = list()
            beta2BiasNNsmoothed_temp5 = list()
            beta3BiasNNsmoothed_temp5 = list()
            beta4BiasNNsmoothed_temp5 = list()
            beta5BiasNNsmoothed_temp5 = list()
            rangeBiasNNsmoothed_temp5 = list()
            sigmaBiasNNsmoothed_temp5 = list()
            averageCIlengthNNsmoothed_temp5 = list()
          }
          
          if (flagC ==1){
            crpsC_temp5 = list()
            rmseC_temp5 = list()
            logscoreC_temp5 = list()
            coverageC_temp5 = list()
            BIASC_temp5 = list()
            beta0BiasC_temp5 = list()
            beta1BiasC_temp5 = list()
            beta2BiasC_temp5 = list()
            beta3BiasC_temp5 = list()
            beta4BiasC_temp5 = list()
            beta5BiasC_temp5 = list()
            rangeBiasC_temp5 = list()
            sigmaBiasC_temp5 = list()
            averageCIlengthC_temp5 = list()
          }
          if (flagR ==1){
            crpsR_temp5 = list()
            rmseR_temp5 = list()
            logscoreR_temp5 = list()
            coverageR_temp5 = list()
            BIASR_temp5 = list()
            beta0BiasR_temp5 = list()
            beta1BiasR_temp5 = list()
            beta2BiasR_temp5 = list()
            beta3BiasR_temp5 = list()
            beta4BiasR_temp5 = list()
            beta5BiasR_temp5 = list()
            rangeBiasR_temp5 = list()
            sigmaBiasR_temp5 = list()
            averageCIlengthR_temp5 = list()
          }
          if (flagCR ==1){
            crpsCR_temp5 = list()
            rmseCR_temp5 = list()
            logscoreCR_temp5 = list()
            coverageCR_temp5 = list()
            BIASCR_temp5 = list()
            beta0BiasCR_temp5 = list()
            beta1BiasCR_temp5 = list()
            beta2BiasCR_temp5 = list()
            beta3BiasCR_temp5 = list()
            beta4BiasCR_temp5 = list()
            beta5BiasCR_temp5 = list()
            rangeBiasCR_temp5 = list()
            sigmaBiasCR_temp5 = list()
            averageCIlengthCR_temp5 = list()
          }
          
          for(l in 1:nSim){
            
            ####################################################################
            beta0true = betas[k,][[1]]
            beta1true = betas[k,][[2]]
            beta2true = betas[k,][[3]]
            beta3true = betas[k,][[4]]
            beta4true = betas[k,][[5]]
            beta5true = betas[k,][[6]]
            
            
            # if ((class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]]) != "try-error") &
            #     (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]]) != "try-error") &
            #     (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]]) != "try-error") &
            #     (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]]) != "try-error") &
            #     (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]]) != "try-error"))
            # {
            # 
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]]) != "try-error"){
              if (flagNN ==1){
                #if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]])) != "try-error") ){
                log_tau = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["fixed.par"]][["log_tau"]]
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["fixed.par"]][["log_kappa"]]
                
                sp_range = sqrt(8.0)/exp(log_kappa)
                sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                        exp(2.0 * log_tau) * exp(2.0 * log_kappa))
                crpsNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["CRPS"]]
                rmseNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["RMSE"]]
                logscoreNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["Logscores"]]
                coverageNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["coverage"]]
                BIASNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["BIAS"]]
                beta0BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][1] - beta0true
                beta1BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][2] - beta1true
                beta2BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][3] - beta2true
                beta3BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][4] - beta3true
                beta4BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][5] - beta4true
                beta5BiasNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][6] - beta5true
                rangeBiasNN_temp5[[l]] = sp_range - rangetrue
                sigmaBiasNN_temp5[[l]] = sp_sigma - sigmatrue
                averageCIlengthNN_temp5[[l]] = mean(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["PredictedResponses"]][,5]-
                                                      outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["PredictedResponses"]][,4])  
                #}
              }}
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]]) != "try-error"){
              if (flagNNsmoothed ==1){
                #if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]])) != "try-error") ){
                log_tau = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["fixed.par"]][["log_tau"]]
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["fixed.par"]][["log_kappa"]]
                
                sp_range = sqrt(8.0)/exp(log_kappa)
                sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                        exp(2.0 * log_tau) * exp(2.0 * log_kappa))
                crpsNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["CRPS"]]
                rmseNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["RMSE"]]
                logscoreNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["Logscores"]]
                coverageNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["coverage"]]
                BIASNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["BIAS"]]
                beta0BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][1] - beta0true
                beta1BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][2] - beta1true
                beta2BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][3] - beta2true
                beta3BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][4] - beta3true
                beta4BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][5] - beta4true
                beta5BiasNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][6] - beta5true
                rangeBiasNNsmoothed_temp5[[l]] = sp_range - rangetrue
                sigmaBiasNNsmoothed_temp5[[l]] = sp_sigma - sigmatrue
                averageCIlengthNNsmoothed_temp5[[l]] = mean(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["PredictedResponses"]][,5]-
                                                              outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["PredictedResponses"]][,4])
                #}
              }}
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]]) != "try-error"){
              if (flagC ==1){
                log_tau = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["fixed.par"]][["log_tau"]]
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["fixed.par"]][["log_kappa"]]
                
                sp_range = sqrt(8.0)/exp(log_kappa)
                sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                        exp(2.0 * log_tau) * exp(2.0 * log_kappa))
                
                crpsC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["CRPS"]]
                rmseC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["RMSE"]]
                logscoreC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["Logscores"]]
                coverageC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["coverage"]]
                BIASC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["BIAS"]]
                beta0BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta0true
                beta1BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][2] - beta1true
                beta2BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][3] - beta2true
                beta3BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][4] - beta3true
                beta4BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][5] - beta4true
                beta5BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][6] - beta5true
                rangeBiasC_temp5[[l]] = sp_range - rangetrue
                sigmaBiasC_temp5[[l]] = sp_sigma - sigmatrue
                averageCIlengthC_temp5[[l]] = mean(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["PredictedResponses"]][,5]-
                                                     outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["PredictedResponses"]][,4])
                #}
                #}
              }}
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]]) != "try-error"){
              if (flagR ==1){
                #if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]])) != "try-error") ){
                log_tau = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["fixed.par"]][["log_tau"]]
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["fixed.par"]][["log_kappa"]]
                
                sp_range = sqrt(8.0)/exp(log_kappa)
                sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                        exp(2.0 * log_tau) * exp(2.0 * log_kappa))
                
                crpsR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["CRPS"]]
                rmseR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["RMSE"]]
                logscoreR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["Logscores"]]
                coverageR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["coverage"]]
                BIASR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["BIAS"]]
                beta0BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][1] - beta0true
                beta1BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][2] - beta1true
                beta2BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][3] - beta2true
                beta3BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][4] - beta3true
                beta4BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][5] - beta4true
                beta5BiasR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["mu"]][6] - beta5true
                rangeBiasR_temp5[[l]] = sp_range - rangetrue
                sigmaBiasR_temp5[[l]] = sp_sigma - sigmatrue
                averageCIlengthR_temp5[[l]] = mean(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["PredictedResponses"]][,5]-
                                                     outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_R"]][["PredictedResponses"]][,4])
                #}
                #}
              }}
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]]) != "try-error"){  
              if (flagCR ==1){
                log_tau = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["fixed.par"]][["log_tau"]]
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["fixed.par"]][["log_kappa"]]
                
                sp_range = sqrt(8.0)/exp(log_kappa)
                sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                        exp(2.0 * log_tau) * exp(2.0 * log_kappa))
                
                crpsCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["CRPS"]]
                rmseCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["RMSE"]]
                logscoreCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["Logscores"]]
                coverageCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["coverage"]]
                #if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]])) != "try-error") ){
                BIASCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["BIAS"]]
                beta0BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][1] - beta0true
                beta1BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][2] - beta1true
                beta2BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][3] - beta2true
                beta3BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][4] - beta3true
                beta4BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][5] - beta4true
                beta5BiasCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][6] - beta5true
                rangeBiasCR_temp5[[l]] = sp_range - rangetrue
                sigmaBiasCR_temp5[[l]] = sp_sigma - sigmatrue
                averageCIlengthCR_temp5[[l]] = mean(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["PredictedResponses"]][,5]-
                                                      outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["PredictedResponses"]][,4])
                # }
              }}
            #}#else{next}
            ####################################################################
            
          }    
          
          
          if (flagNN ==1){
            crpsNN_temp4[[h]] = mean(unlist(crpsNN_temp5))
            rmseNN_temp4[[h]] = mean(unlist(rmseNN_temp5))
            logscoreNN_temp4[[h]] = mean(unlist(logscoreNN_temp5))
            coverageNN_temp4[[h]] = mean(unlist(coverageNN_temp5))
            BIASNN_temp4[[h]] = mean(unlist(BIASNN_temp5))
            beta0BiasNN_temp4[[h]] = mean(unlist(beta0BiasNN_temp5))
            beta1BiasNN_temp4[[h]] = mean(unlist(beta1BiasNN_temp5))
            beta2BiasNN_temp4[[h]] = mean(unlist(beta2BiasNN_temp5))
            beta3BiasNN_temp4[[h]] = mean(unlist(beta3BiasNN_temp5))
            beta4BiasNN_temp4[[h]] = mean(unlist(beta4BiasNN_temp5))
            beta5BiasNN_temp4[[h]] = mean(unlist(beta5BiasNN_temp5))
            rangeBiasNN_temp4[[h]] = mean(unlist(rangeBiasNN_temp5))
            sigmaBiasNN_temp4[[h]] = mean(unlist(sigmaBiasNN_temp5))
            averageCIlengthNN_temp4[[h]] = mean(unlist(averageCIlengthNN_temp5))
          }
          
          if (flagNNsmoothed ==1){
            crpsNNsmoothed_temp4[[h]] = mean(unlist(crpsNNsmoothed_temp5))
            rmseNNsmoothed_temp4[[h]] = mean(unlist(rmseNNsmoothed_temp5))
            logscoreNNsmoothed_temp4[[h]] = mean(unlist(logscoreNNsmoothed_temp5))
            coverageNNsmoothed_temp4[[h]] = mean(unlist(coverageNNsmoothed_temp5))
            BIASNNsmoothed_temp4[[h]] = mean(unlist(BIASNNsmoothed_temp5))
            beta0BiasNNsmoothed_temp4[[h]] = mean(unlist(beta0BiasNNsmoothed_temp5))
            beta1BiasNNsmoothed_temp4[[h]] = mean(unlist(beta1BiasNNsmoothed_temp5))
            beta2BiasNNsmoothed_temp4[[h]] = mean(unlist(beta2BiasNNsmoothed_temp5))
            beta3BiasNNsmoothed_temp4[[h]] = mean(unlist(beta3BiasNNsmoothed_temp5))
            beta4BiasNNsmoothed_temp4[[h]] = mean(unlist(beta4BiasNNsmoothed_temp5))
            beta5BiasNNsmoothed_temp4[[h]] = mean(unlist(beta5BiasNNsmoothed_temp5))
            rangeBiasNNsmoothed_temp4[[h]] = mean(unlist(rangeBiasNNsmoothed_temp5))
            sigmaBiasNNsmoothed_temp4[[h]] = mean(unlist(sigmaBiasNNsmoothed_temp5))
            averageCIlengthNNsmoothed_temp4[[h]] = mean(unlist(averageCIlengthNNsmoothed_temp5))
          }
          
          
          if (flagC ==1){
            crpsC_temp4[[h]] = mean(unlist(crpsC_temp5))
            rmseC_temp4[[h]] = mean(unlist(rmseC_temp5))
            logscoreC_temp4[[h]] = mean(unlist(logscoreC_temp5))
            coverageC_temp4[[h]] = mean(unlist(coverageC_temp5))
            BIASC_temp4[[h]] = mean(unlist(BIASC_temp5))
            beta0BiasC_temp4[[h]] = mean(unlist(beta0BiasC_temp5))
            beta1BiasC_temp4[[h]] = mean(unlist(beta1BiasC_temp5))
            beta2BiasC_temp4[[h]] = mean(unlist(beta2BiasC_temp5))
            beta3BiasC_temp4[[h]] = mean(unlist(beta3BiasC_temp5))
            beta4BiasC_temp4[[h]] = mean(unlist(beta4BiasC_temp5))
            beta5BiasC_temp4[[h]] = mean(unlist(beta5BiasC_temp5))
            rangeBiasC_temp4[[h]] = mean(unlist(rangeBiasC_temp5))
            sigmaBiasC_temp4[[h]] = mean(unlist(sigmaBiasC_temp5))
            averageCIlengthC_temp4[[h]] = mean(unlist(averageCIlengthC_temp5))
          }
          
          
          
          if (flagR ==1){
            crpsR_temp4[[h]] = mean(unlist(crpsR_temp5))
            rmseR_temp4[[h]] = mean(unlist(rmseR_temp5))
            logscoreR_temp4[[h]] = mean(unlist(logscoreR_temp5))
            coverageR_temp4[[h]] = mean(unlist(coverageR_temp5))
            BIASR_temp4[[h]] = mean(unlist(BIASR_temp5))
            beta0BiasR_temp4[[h]] = mean(unlist(beta0BiasR_temp5))
            beta1BiasR_temp4[[h]] = mean(unlist(beta1BiasR_temp5))
            beta2BiasR_temp4[[h]] = mean(unlist(beta2BiasR_temp5))
            beta3BiasR_temp4[[h]] = mean(unlist(beta3BiasR_temp5))
            beta4BiasR_temp4[[h]] = mean(unlist(beta4BiasR_temp5))
            beta5BiasR_temp4[[h]] = mean(unlist(beta5BiasR_temp5))
            rangeBiasR_temp4[[h]] = mean(unlist(rangeBiasR_temp5))
            sigmaBiasR_temp4[[h]] = mean(unlist(sigmaBiasR_temp5))
            averageCIlengthR_temp4[[h]] = mean(unlist(averageCIlengthR_temp5))
          }
          if (flagCR ==1){
            
           
            crpsCR_temp4[[h]] = mean(unlist(crpsCR_temp5))
            rmseCR_temp4[[h]] = mean(unlist(rmseCR_temp5))
            logscoreCR_temp4[[h]] = mean(unlist(logscoreCR_temp5))
            coverageCR_temp4[[h]] = mean(unlist(coverageCR_temp5))
            BIASCR_temp4[[h]] = mean(unlist(BIASCR_temp5))
            beta0BiasCR_temp4[[h]] = mean(unlist(beta0BiasCR_temp5))
            beta1BiasCR_temp4[[h]] = mean(unlist(beta1BiasCR_temp5))
            beta2BiasCR_temp4[[h]] = mean(unlist(beta2BiasCR_temp5))
            beta3BiasCR_temp4[[h]] = mean(unlist(beta3BiasCR_temp5))
            beta4BiasCR_temp4[[h]] = mean(unlist(beta4BiasCR_temp5))
            beta5BiasCR_temp4[[h]] = mean(unlist(beta5BiasCR_temp5))
            rangeBiasCR_temp4[[h]] = mean(unlist(rangeBiasCR_temp5))
            sigmaBiasCR_temp4[[h]] = mean(unlist(sigmaBiasCR_temp5))
            averageCIlengthCR_temp4[[h]] = mean(unlist(averageCIlengthCR_temp5))
            
          
          
          }
          
          
          ##########################################
        }
        
        if (flagNN ==1){
          crpsNN_temp3[[j]] = crpsNN_temp4[[1]]
          rmseNN_temp3[[j]] = rmseNN_temp4[[1]]
          logscoreNN_temp3[[j]] = logscoreNN_temp4[[1]]
          coverageNN_temp3[[j]] = coverageNN_temp4[[1]]
          BIASNN_temp3[[j]] = BIASNN_temp4[[1]]
          beta0BiasNN_temp3[[j]] = beta0BiasNN_temp4[[1]]
          beta1BiasNN_temp3[[j]] = beta1BiasNN_temp4[[1]]
          beta2BiasNN_temp3[[j]] = beta2BiasNN_temp4[[1]]
          beta3BiasNN_temp3[[j]] = beta3BiasNN_temp4[[1]]
          beta4BiasNN_temp3[[j]] = beta4BiasNN_temp4[[1]]
          beta5BiasNN_temp3[[j]] = beta5BiasNN_temp4[[1]]
          rangeBiasNN_temp3[[j]] = rangeBiasNN_temp4[[1]]
          sigmaBiasNN_temp3[[j]] = sigmaBiasNN_temp4[[1]]
          averageCIlengthNN_temp3[[j]] = averageCIlengthNN_temp4[[1]]
        }
        
        
        if (flagNNsmoothed ==1){
          crpsNNsmoothed_temp3[[j]] = crpsNNsmoothed_temp4[[1]]
          rmseNNsmoothed_temp3[[j]] = rmseNNsmoothed_temp4[[1]]
          logscoreNNsmoothed_temp3[[j]] = logscoreNNsmoothed_temp4[[1]]
          coverageNNsmoothed_temp3[[j]] = coverageNNsmoothed_temp4[[1]]
          BIASNNsmoothed_temp3[[j]] = BIASNNsmoothed_temp4[[1]]
          beta0BiasNNsmoothed_temp3[[j]] = beta0BiasNNsmoothed_temp4[[1]]
          beta1BiasNNsmoothed_temp3[[j]] = beta1BiasNNsmoothed_temp4[[1]]
          beta2BiasNNsmoothed_temp3[[j]] = beta2BiasNNsmoothed_temp4[[1]]
          beta3BiasNNsmoothed_temp3[[j]] = beta3BiasNNsmoothed_temp4[[1]]
          beta4BiasNNsmoothed_temp3[[j]] = beta4BiasNNsmoothed_temp4[[1]]
          beta5BiasNNsmoothed_temp3[[j]] = beta5BiasNNsmoothed_temp4[[1]]
          rangeBiasNNsmoothed_temp3[[j]] = rangeBiasNNsmoothed_temp4[[1]]
          sigmaBiasNNsmoothed_temp3[[j]] = sigmaBiasNNsmoothed_temp4[[1]]
          averageCIlengthNNsmoothed_temp3[[j]] = averageCIlengthNNsmoothed_temp4[[1]]
        }
        
        
        
        if (flagC ==1){
          crpsC_temp3[[j]] = crpsC_temp4[[1]]
          rmseC_temp3[[j]] = rmseC_temp4[[1]]
          logscoreC_temp3[[j]] = logscoreC_temp4[[1]]
          coverageC_temp3[[j]] = coverageC_temp4[[1]]
          BIASC_temp3[[j]] = BIASC_temp4[[1]]
          beta0BiasC_temp3[[j]] = beta0BiasC_temp4[[1]]
          beta1BiasC_temp3[[j]] = beta1BiasC_temp4[[1]]
          beta2BiasC_temp3[[j]] = beta2BiasC_temp4[[1]]
          beta3BiasC_temp3[[j]] = beta3BiasC_temp4[[1]]
          beta4BiasC_temp3[[j]] = beta4BiasC_temp4[[1]]
          beta5BiasC_temp3[[j]] = beta5BiasC_temp4[[1]]
          rangeBiasC_temp3[[j]] = rangeBiasC_temp4[[1]]
          sigmaBiasC_temp3[[j]] = sigmaBiasC_temp4[[1]]
          averageCIlengthC_temp3[[j]] = averageCIlengthC_temp4[[1]]
        }
        if (flagR ==1){
          crpsR_temp3[[j]] = crpsR_temp4[[1]]
          rmseR_temp3[[j]] = rmseR_temp4[[1]]
          logscoreR_temp3[[j]] = logscoreR_temp4[[1]]
          coverageR_temp3[[j]] = coverageR_temp4[[1]]
          BIASR_temp3[[j]] = BIASR_temp4[[1]]
          beta0BiasR_temp3[[j]] = beta0BiasR_temp4[[1]]
          beta1BiasR_temp3[[j]] = beta1BiasR_temp4[[1]]
          beta2BiasR_temp3[[j]] = beta2BiasR_temp4[[1]]
          beta3BiasR_temp3[[j]] = beta3BiasR_temp4[[1]]
          beta4BiasR_temp3[[j]] = beta4BiasR_temp4[[1]]
          beta5BiasR_temp3[[j]] = beta5BiasR_temp4[[1]]
          rangeBiasR_temp3[[j]] = rangeBiasR_temp4[[1]]
          sigmaBiasR_temp3[[j]] = sigmaBiasR_temp4[[1]]
          averageCIlengthR_temp3[[j]] = averageCIlengthR_temp4[[1]]
        }
        if (flagCR ==1){
          crpsCR_temp3[[j]] = crpsCR_temp4[[1]]
          rmseCR_temp3[[j]] = rmseCR_temp4[[1]]
          logscoreCR_temp3[[j]] = logscoreCR_temp4[[1]]
          coverageCR_temp3[[j]] = coverageCR_temp4[[1]]
          BIASCR_temp3[[j]] = BIASCR_temp4[[1]]
          beta0BiasCR_temp3[[j]] = beta0BiasCR_temp4[[1]]
          beta1BiasCR_temp3[[j]] = beta1BiasCR_temp4[[1]]
          beta2BiasCR_temp3[[j]] = beta2BiasCR_temp4[[1]]
          beta3BiasCR_temp3[[j]] = beta3BiasCR_temp4[[1]]
          beta4BiasCR_temp3[[j]] = beta4BiasCR_temp4[[1]]
          beta5BiasCR_temp3[[j]] = beta5BiasCR_temp4[[1]]
          rangeBiasCR_temp3[[j]] = rangeBiasCR_temp4[[1]]
          sigmaBiasCR_temp3[[j]] = sigmaBiasCR_temp4[[1]]
          averageCIlengthCR_temp3[[j]] = averageCIlengthCR_temp4[[1]]
        }   
        ##############################
      }
      
      
      if (flagNN ==1){
        crpsNN_temp2[[i]] = crpsNN_temp3[[1]]
        rmseNN_temp2[[i]] = rmseNN_temp3[[1]]
        logscoreNN_temp2[[i]] = logscoreNN_temp3[[1]]
        coverageNN_temp2[[i]] = coverageNN_temp3[[1]]
        BIASNN_temp2[[i]] = BIASNN_temp3[[1]]
        beta0BiasNN_temp2[[i]] = beta0BiasNN_temp3[[1]]
        beta1BiasNN_temp2[[i]] = beta1BiasNN_temp3[[1]]
        beta2BiasNN_temp2[[i]] = beta2BiasNN_temp3[[1]]
        beta3BiasNN_temp2[[i]] = beta3BiasNN_temp3[[1]]
        beta4BiasNN_temp2[[i]] = beta4BiasNN_temp3[[1]]
        beta5BiasNN_temp2[[i]] = beta5BiasNN_temp3[[1]]
        rangeBiasNN_temp2[[i]] = rangeBiasNN_temp3[[1]]
        sigmaBiasNN_temp2[[i]] = sigmaBiasNN_temp3[[1]]
        averageCIlengthNN_temp2[[i]] = averageCIlengthNN_temp3[[1]]
      }
      
      if (flagNNsmoothed ==1){
        crpsNNsmoothed_temp2[[i]] = crpsNNsmoothed_temp3[[1]]
        rmseNNsmoothed_temp2[[i]] = rmseNNsmoothed_temp3[[1]]
        logscoreNNsmoothed_temp2[[i]] = logscoreNNsmoothed_temp3[[1]]
        coverageNNsmoothed_temp2[[i]] = coverageNNsmoothed_temp3[[1]]
        BIASNNsmoothed_temp2[[i]] = BIASNNsmoothed_temp3[[1]]
        beta0BiasNNsmoothed_temp2[[i]] = beta0BiasNNsmoothed_temp3[[1]]
        beta1BiasNNsmoothed_temp2[[i]] = beta1BiasNNsmoothed_temp3[[1]]
        beta2BiasNNsmoothed_temp2[[i]] = beta2BiasNNsmoothed_temp3[[1]]
        beta3BiasNNsmoothed_temp2[[i]] = beta3BiasNNsmoothed_temp3[[1]]
        beta4BiasNNsmoothed_temp2[[i]] = beta4BiasNNsmoothed_temp3[[1]]
        beta5BiasNNsmoothed_temp2[[i]] = beta5BiasNNsmoothed_temp3[[1]]
        rangeBiasNNsmoothed_temp2[[i]] = rangeBiasNNsmoothed_temp3[[1]]
        sigmaBiasNNsmoothed_temp2[[i]] = sigmaBiasNNsmoothed_temp3[[1]]
        averageCIlengthNNsmoothed_temp2[[i]] = averageCIlengthNNsmoothed_temp3[[1]]
      }
      
      
      if (flagC ==1){
        crpsC_temp2[[i]] = crpsC_temp3[[1]]
        rmseC_temp2[[i]] = rmseC_temp3[[1]]
        logscoreC_temp2[[i]] = logscoreC_temp3[[1]]
        coverageC_temp2[[i]] = coverageC_temp3[[1]]
        BIASC_temp2[[i]] = BIASC_temp3[[1]]
        beta0BiasC_temp2[[i]] = beta0BiasC_temp3[[1]]
        beta1BiasC_temp2[[i]] = beta1BiasC_temp3[[1]]
        beta2BiasC_temp2[[i]] = beta2BiasC_temp3[[1]]
        beta3BiasC_temp2[[i]] = beta3BiasC_temp3[[1]]
        beta4BiasC_temp2[[i]] = beta4BiasC_temp3[[1]]
        beta5BiasC_temp2[[i]] = beta5BiasC_temp3[[1]]
        rangeBiasC_temp2[[i]] = rangeBiasC_temp3[[1]]
        sigmaBiasC_temp2[[i]] = sigmaBiasC_temp3[[1]]
        averageCIlengthC_temp2[[i]] = averageCIlengthC_temp3[[1]]
      }
      if (flagR ==1){
        crpsR_temp2[[i]] = crpsR_temp3[[1]]
        rmseR_temp2[[i]] = rmseR_temp3[[1]]
        logscoreR_temp2[[i]] = logscoreR_temp3[[1]]
        coverageR_temp2[[i]] = coverageR_temp3[[1]]
        BIASR_temp2[[i]] = BIASR_temp3[[1]]
        beta0BiasR_temp2[[i]] = beta0BiasR_temp3[[1]]
        beta1BiasR_temp2[[i]] = beta1BiasR_temp3[[1]]
        beta2BiasR_temp2[[i]] = beta2BiasR_temp3[[1]]
        beta3BiasR_temp2[[i]] = beta3BiasR_temp3[[1]]
        beta4BiasR_temp2[[i]] = beta4BiasR_temp3[[1]]
        beta5BiasR_temp2[[i]] = beta5BiasR_temp3[[1]]
        rangeBiasR_temp2[[i]] = rangeBiasR_temp3[[1]]
        sigmaBiasR_temp2[[i]] = sigmaBiasR_temp3[[1]]
        averageCIlengthR_temp2[[i]] = averageCIlengthR_temp3[[1]]
      }
      if (flagCR ==1){
        crpsCR_temp2[[i]] = crpsCR_temp3[[1]]
        rmseCR_temp2[[i]] = rmseCR_temp3[[1]]
        logscoreCR_temp2[[i]] = logscoreCR_temp3[[1]]
        coverageCR_temp2[[i]] = coverageCR_temp3[[1]]
        BIASCR_temp2[[i]] = BIASCR_temp3[[1]]
        beta0BiasCR_temp2[[i]] = beta0BiasCR_temp3[[1]]
        beta1BiasCR_temp2[[i]] = beta1BiasCR_temp3[[1]]
        beta2BiasCR_temp2[[i]] = beta2BiasCR_temp3[[1]]
        beta3BiasCR_temp2[[i]] = beta3BiasCR_temp3[[1]]
        beta4BiasCR_temp2[[i]] = beta4BiasCR_temp3[[1]]
        beta5BiasCR_temp2[[i]] = beta5BiasCR_temp3[[1]]
        rangeBiasCR_temp2[[i]] = rangeBiasCR_temp3[[1]]
        sigmaBiasCR_temp2[[i]] = sigmaBiasCR_temp3[[1]]
        averageCIlengthCR_temp2[[i]] = averageCIlengthCR_temp3[[1]]
      } 
      ############################
    }
    
    
    if (flagNN ==1){
      crpsNN_temp1[[g]] = crpsNN_temp2[[1]]
      rmseNN_temp1[[g]] = rmseNN_temp2[[1]]
      logscoreNN_temp1[[g]] = logscoreNN_temp2[[1]]
      coverageNN_temp1[[g]] = coverageNN_temp2[[1]]
      BIASNN_temp1[[g]] = BIASNN_temp2[[1]]
      beta0BiasNN_temp1[[g]] = beta0BiasNN_temp2[[1]]
      beta1BiasNN_temp1[[g]] = beta1BiasNN_temp2[[1]]
      beta2BiasNN_temp1[[g]] = beta2BiasNN_temp2[[1]]
      beta3BiasNN_temp1[[g]] = beta3BiasNN_temp2[[1]]
      beta4BiasNN_temp1[[g]] = beta4BiasNN_temp2[[1]]
      beta5BiasNN_temp1[[g]] = beta5BiasNN_temp2[[1]]
      rangeBiasNN_temp1[[g]] = rangeBiasNN_temp2[[1]]
      sigmaBiasNN_temp1[[g]] = sigmaBiasNN_temp2[[1]]
      averageCIlengthNN_temp1[[g]] = averageCIlengthNN_temp2[[1]]
    }
    
    if (flagNNsmoothed ==1){
      crpsNNsmoothed_temp1[[g]] = crpsNNsmoothed_temp2[[1]]
      rmseNNsmoothed_temp1[[g]] = rmseNNsmoothed_temp2[[1]]
      logscoreNNsmoothed_temp1[[g]] = logscoreNNsmoothed_temp2[[1]]
      coverageNNsmoothed_temp1[[g]] = coverageNNsmoothed_temp2[[1]]
      BIASNNsmoothed_temp1[[g]] = BIASNNsmoothed_temp2[[1]]
      beta0BiasNNsmoothed_temp1[[g]] = beta0BiasNNsmoothed_temp2[[1]]
      beta1BiasNNsmoothed_temp1[[g]] = beta1BiasNNsmoothed_temp2[[1]]
      beta2BiasNNsmoothed_temp1[[g]] = beta2BiasNNsmoothed_temp2[[1]]
      beta3BiasNNsmoothed_temp1[[g]] = beta3BiasNNsmoothed_temp2[[1]]
      beta4BiasNNsmoothed_temp1[[g]] = beta4BiasNNsmoothed_temp2[[1]]
      beta5BiasNNsmoothed_temp1[[g]] = beta5BiasNNsmoothed_temp2[[1]]
      rangeBiasNNsmoothed_temp1[[g]] = rangeBiasNNsmoothed_temp2[[1]]
      sigmaBiasNNsmoothed_temp1[[g]] = sigmaBiasNNsmoothed_temp2[[1]]
      averageCIlengthNNsmoothed_temp1[[g]] = averageCIlengthNNsmoothed_temp2[[1]]
    }
    
    if (flagC ==1){
      crpsC_temp1[[g]] = crpsC_temp2[[1]]
      rmseC_temp1[[g]] = rmseC_temp2[[1]]
      logscoreC_temp1[[g]] = logscoreC_temp2[[1]]
      coverageC_temp1[[g]] = coverageC_temp2[[1]]
      BIASC_temp1[[g]] = BIASC_temp2[[1]]
      beta0BiasC_temp1[[g]] = beta0BiasC_temp2[[1]]
      beta1BiasC_temp1[[g]] = beta1BiasC_temp2[[1]]
      beta2BiasC_temp1[[g]] = beta2BiasC_temp2[[1]]
      beta3BiasC_temp1[[g]] = beta3BiasC_temp2[[1]]
      beta4BiasC_temp1[[g]] = beta4BiasC_temp2[[1]]
      beta5BiasC_temp1[[g]] = beta5BiasC_temp2[[1]]
      rangeBiasC_temp1[[g]] = rangeBiasC_temp2[[1]]
      sigmaBiasC_temp1[[g]] = sigmaBiasC_temp2[[1]]
      averageCIlengthC_temp1[[g]] = averageCIlengthC_temp2[[1]]
    }
    if (flagR ==1){
      crpsR_temp1[[g]] = crpsR_temp2[[1]]
      rmseR_temp1[[g]] = rmseR_temp2[[1]]
      logscoreR_temp1[[g]] = logscoreR_temp2[[1]]
      coverageR_temp1[[g]] = coverageR_temp2[[1]]
      BIASR_temp1[[g]] = BIASR_temp2[[1]]
      beta0BiasR_temp1[[g]] = beta0BiasR_temp2[[1]]
      beta1BiasR_temp1[[g]] = beta1BiasR_temp2[[1]]
      beta2BiasR_temp1[[g]] = beta2BiasR_temp2[[1]]
      beta3BiasR_temp1[[g]] = beta3BiasR_temp2[[1]]
      beta4BiasR_temp1[[g]] = beta4BiasR_temp2[[1]]
      beta5BiasR_temp1[[g]] = beta5BiasR_temp2[[1]]
      rangeBiasR_temp1[[g]] = rangeBiasR_temp2[[1]]
      sigmaBiasR_temp1[[g]] = sigmaBiasR_temp2[[1]]
      averageCIlengthR_temp1[[g]] = averageCIlengthR_temp2[[1]]
    }
    if (flagCR ==1){
      crpsCR_temp1[[g]] = crpsCR_temp2[[1]]
      rmseCR_temp1[[g]] = rmseCR_temp2[[1]]
      logscoreCR_temp1[[g]] = logscoreCR_temp2[[1]]
      coverageCR_temp1[[g]] = coverageCR_temp2[[1]]
      BIASCR_temp1[[g]] = BIASCR_temp2[[1]]
      beta0BiasCR_temp1[[g]] = beta0BiasCR_temp2[[1]]
      beta1BiasCR_temp1[[g]] = beta1BiasCR_temp2[[1]]
      beta2BiasCR_temp1[[g]] = beta2BiasCR_temp2[[1]]
      beta3BiasCR_temp1[[g]] = beta3BiasCR_temp2[[1]]
      beta4BiasCR_temp1[[g]] = beta4BiasCR_temp2[[1]]
      beta5BiasCR_temp1[[g]] = beta5BiasCR_temp2[[1]]
      rangeBiasCR_temp1[[g]] = rangeBiasCR_temp2[[1]]
      sigmaBiasCR_temp1[[g]] = sigmaBiasCR_temp2[[1]]
      averageCIlengthCR_temp1[[g]] = averageCIlengthCR_temp2[[1]]
    }       
    ################################
  }
  
  
  if (flagNN ==1){
    crpsNN[[k]] = crpsNN_temp1[[1]]
    rmseNN[[k]] = rmseNN_temp1[[1]]
    logscoreNN[[k]] = logscoreNN_temp1[[1]]
    coverageNN[[k]] = coverageNN_temp1[[1]]
    BIASNN[[k]] = BIASNN_temp1[[1]]
    beta0BiasNN[[k]] = beta0BiasNN_temp1[[1]]
    beta1BiasNN[[k]] = beta1BiasNN_temp1[[1]]
    beta2BiasNN[[k]] = beta2BiasNN_temp1[[1]]
    beta3BiasNN[[k]] = beta3BiasNN_temp1[[1]]
    beta4BiasNN[[k]] = beta4BiasNN_temp1[[1]]
    beta5BiasNN[[k]] = beta5BiasNN_temp1[[1]]
    rangeBiasNN[[k]] = rangeBiasNN_temp1[[1]]
    sigmaBiasNN[[k]] = sigmaBiasNN_temp1[[1]]
    averageCIlengthNN[[k]] = averageCIlengthNN_temp1[[1]]
  }
  
  if (flagNNsmoothed ==1){
    crpsNNsmoothed[[k]] = crpsNNsmoothed_temp1[[1]]
    rmseNNsmoothed[[k]] = rmseNNsmoothed_temp1[[1]]
    logscoreNNsmoothed[[k]] = logscoreNNsmoothed_temp1[[1]]
    coverageNNsmoothed[[k]] = coverageNNsmoothed_temp1[[1]]
    BIASNNsmoothed[[k]] = BIASNNsmoothed_temp1[[1]]
    beta0BiasNNsmoothed[[k]] = beta0BiasNNsmoothed_temp1[[1]]
    beta1BiasNNsmoothed[[k]] = beta1BiasNNsmoothed_temp1[[1]]
    beta2BiasNNsmoothed[[k]] = beta2BiasNNsmoothed_temp1[[1]]
    beta3BiasNNsmoothed[[k]] = beta3BiasNNsmoothed_temp1[[1]]
    beta4BiasNNsmoothed[[k]] = beta4BiasNNsmoothed_temp1[[1]]
    beta5BiasNNsmoothed[[k]] = beta5BiasNNsmoothed_temp1[[1]]
    rangeBiasNNsmoothed[[k]] = rangeBiasNNsmoothed_temp1[[1]]
    sigmaBiasNNsmoothed[[k]] = sigmaBiasNNsmoothed_temp1[[1]]
    averageCIlengthNNsmoothed[[k]] = averageCIlengthNNsmoothed_temp1[[1]]
  }
  
  
  if (flagC ==1){
    crpsC[[k]] = crpsC_temp1[[1]]
    rmseC[[k]] = rmseC_temp1[[1]]
    logscoreC[[k]] = logscoreC_temp1[[1]]
    coverageC[[k]] = coverageC_temp1[[1]]
    BIASC[[k]] = BIASC_temp1[[1]]
    beta0BiasC[[k]] = beta0BiasC_temp1[[1]]
    beta1BiasC[[k]] = beta1BiasC_temp1[[1]]
    beta2BiasC[[k]] = beta2BiasC_temp1[[1]]
    beta3BiasC[[k]] = beta3BiasC_temp1[[1]]
    beta4BiasC[[k]] = beta4BiasC_temp1[[1]]
    beta5BiasC[[k]] = beta5BiasC_temp1[[1]]
    rangeBiasC[[k]] = rangeBiasC_temp1[[1]]
    sigmaBiasC[[k]] = sigmaBiasC_temp1[[1]]
    averageCIlengthC[[k]] = averageCIlengthC_temp1[[1]]
  }
  if (flagR ==1){
    crpsR[[k]] = crpsR_temp1[[1]]
    rmseR[[k]] = rmseR_temp1[[1]]
    logscoreR[[k]] = logscoreR_temp1[[1]]
    coverageR[[k]] = coverageR_temp1[[1]]
    BIASR[[k]] = BIASR_temp1[[1]]
    beta0BiasR[[k]] = beta0BiasR_temp1[[1]]
    beta1BiasR[[k]] = beta1BiasR_temp1[[1]]
    beta2BiasR[[k]] = beta2BiasR_temp1[[1]]
    beta3BiasR[[k]] = beta3BiasR_temp1[[1]]
    beta4BiasR[[k]] = beta4BiasR_temp1[[1]]
    beta5BiasR[[k]] = beta5BiasR_temp1[[1]]
    rangeBiasR[[k]] = rangeBiasR_temp1[[1]]
    sigmaBiasR[[k]] = sigmaBiasR_temp1[[1]]
    averageCIlengthR[[k]] = averageCIlengthR_temp1[[1]]
  }
  if (flagCR ==1){
    crpsCR[[k]] = crpsCR_temp1[[1]]
    rmseCR[[k]] = rmseCR_temp1[[1]]
    logscoreCR[[k]] = logscoreCR_temp1[[1]]
    coverageCR[[k]] = coverageCR_temp1[[1]]
    BIASCR[[k]] = BIASCR_temp1[[1]]
    beta0BiasCR[[k]] = beta0BiasCR_temp1[[1]]
    beta1BiasCR[[k]] = beta1BiasCR_temp1[[1]]
    beta2BiasCR[[k]] = beta2BiasCR_temp1[[1]]
    beta3BiasCR[[k]] = beta3BiasCR_temp1[[1]]
    beta4BiasCR[[k]] = beta4BiasCR_temp1[[1]]
    beta5BiasCR[[k]] = beta5BiasCR_temp1[[1]]
    rangeBiasCR[[k]] = rangeBiasCR_temp1[[1]]
    sigmaBiasCR[[k]] = sigmaBiasCR_temp1[[1]]
    averageCIlengthCR[[k]] = averageCIlengthCR_temp1[[1]]
  }        
}


extractedResultsAll = list(NN = list(averageCIlengthNN = averageCIlengthNN,
                                     crpsNN = crpsNN,
                                     rmseNN = rmseNN,
                                     logscoreNN = logscoreNN,
                                     coverageNN = coverageNN,
                                     BIASNN = BIASNN,
                                     beta0BiasNN = beta0BiasNN,
                                     beta1BiasNN = beta1BiasNN,
                                     beta2BiasNN = beta2BiasNN,
                                     beta3BiasNN = beta3BiasNN,
                                     beta4BiasNN = beta4BiasNN,
                                     beta5BiasNN = beta5BiasNN,
                                     rangeBiasNN = rangeBiasNN,
                                     sigmaBiasNN = sigmaBiasNN
),
NNsmoothed = list(averageCIlengthNNsmoothed = averageCIlengthNNsmoothed,
                  crpsNNsmoothed = crpsNNsmoothed,
                  rmseNNsmoothed = rmseNNsmoothed,
                  logscoreNNsmoothed = logscoreNNsmoothed,
                  coverageNNsmoothed = coverageNNsmoothed,
                  BIASNNsmoothed = BIASNNsmoothed,
                  beta0BiasNNsmoothed = beta0BiasNNsmoothed,
                  beta1BiasNNsmoothed = beta1BiasNNsmoothed,
                  beta2BiasNNsmoothed = beta2BiasNNsmoothed,
                  beta3BiasNNsmoothed = beta3BiasNNsmoothed,
                  beta4BiasNNsmoothed = beta4BiasNNsmoothed,
                  beta5BiasNNsmoothed = beta5BiasNNsmoothed,
                  rangeBiasNNsmoothed = rangeBiasNNsmoothed,
                  sigmaBiasNNsmoothed = sigmaBiasNNsmoothed
),
# C = list(averageCIlengthC = averageCIlengthC,
#          crpsC = crpsC,
#          rmseC = rmseC,
#          logscoreC = logscoreC,
#          coverageC = coverageC,
#          BIASC = BIASC,
#          beta0BiasC = beta0BiasC,
#          beta1BiasC = beta1BiasC,
#          beta2BiasC = beta2BiasC,
#          beta3BiasC = beta3BiasC,
#          beta4BiasC = beta4BiasC,
#          beta5BiasC = beta5BiasC,
#          rangeBiasC = rangeBiasC,
#          sigmaBiasC = sigmaBiasC
# ),
# R = list(averageCIlengthR = averageCIlengthR,
#          crpsR = crpsR,
#          rmseR = rmseR,
#          logscoreR = logscoreR,
#          coverageR = coverageR,
#          BIASR = BIASR,
#          beta0BiasR = beta0BiasR,
#          beta1BiasR = beta1BiasR,
#          beta2BiasR = beta2BiasR,
#          beta3BiasR = beta3BiasR,
#          beta4BiasR = beta4BiasR,
#          beta5BiasR = beta5BiasR,
#          rangeBiasR = rangeBiasR,
#          sigmaBiasR = sigmaBiasR
# ),
CR = list(averageCIlengthCR = averageCIlengthCR,
          crpsCR = crpsCR,
          rmseCR = rmseCR,
          logscoreCR = logscoreCR,
          coverageCR = coverageCR,
          BIASCR = BIASCR,
          beta0BiasCR = beta0BiasCR,
          beta1BiasCR = beta1BiasCR,
          beta2BiasCR =beta2BiasCR,
          beta3BiasCR = beta3BiasCR,
          beta4BiasCR = beta4BiasCR,
          beta5BiasCR = beta5BiasCR,
          rangeBiasCR = rangeBiasCR,
          sigmaBiasCR = sigmaBiasCR
))

save(extractedResultsAll, file = "extractedResults.RData")


################################################################################

sims = list()
for (i in 1:nBeta){
  sims[[i]] = outputTMB[[i]][["Borders respected"]][["binomial likelihood"]][["range from RealData"]][["jitteringFactor = 1"]]
}


NNsmoothedinterceptRMSE = list()
NNsmoothedbeta_distRiversLakesRMSE = list()
NNsmoothedbeta_accessCitiesRMSE = list()
NNsmoothedbeta_elevationRMSE = list()
NNsmoothedbeta_populationRMSE = list()
NNsmoothedbeta_urbanicityRMSE = list()
NNsmoothedrangeRMSE = list()
NNsmoothedsigmaRMSE = list()


NNinterceptRMSE = list()
NNbeta_distRiversLakesRMSE = list()
NNbeta_accessCitiesRMSE = list()
NNbeta_elevationRMSE = list()
NNbeta_populationRMSE = list()
NNbeta_urbanicityRMSE = list()
NNrangeRMSE = list()
NNsigmaRMSE = list()

# CinterceptRMSE = list()
# Cbeta_distRiversLakesRMSE = list()
# Cbeta_accessCitiesRMSE = list()
# Cbeta_elevationRMSE = list()
# Cbeta_populationRMSE = list()
# Cbeta_urbanicityRMSE = list()
# CrangeRMSE = list()
# CsigmaRMSE = list()
# 
# RinterceptRMSE = list()
# Rbeta_distRiversLakesRMSE = list()
# Rbeta_accessCitiesRMSE = list()
# Rbeta_elevationRMSE = list()
# Rbeta_populationRMSE = list()
# Rbeta_urbanicityRMSE = list()
# RrangeRMSE = list()
# RsigmaRMSE = list()

CRinterceptRMSE = list()
CRbeta_distRiversLakesRMSE = list()
CRbeta_accessCitiesRMSE = list()
CRbeta_elevationRMSE = list()
CRbeta_populationRMSE = list()
CRbeta_urbanicityRMSE = list()
CRrangeRMSE = list()
CRsigmaRMSE = list()


for (i in 1:nBeta){
  
  beta0true = betas[i,][[1]]
  beta1true = betas[i,][[2]]
  beta2true = betas[i,][[3]]
  beta3true = betas[i,][[4]]
  beta4true = betas[i,][[5]]
  beta5true = betas[i,][[6]]
  
  NNsmoothedinterceptSqrDiff_temp1 = list()
  NNsmoothedbeta_distRiversLakesSqrDiff_temp1 = list()
  NNsmoothedbeta_accessCitiesSqrDiff_temp1 = list()
  NNsmoothedbeta_elevationSqrDiff_temp1 = list()
  NNsmoothedbeta_populationSqrDiff_temp1 = list()
  NNsmoothedbeta_urbanicitySqrDiff_temp1 = list()
  NNsmoothedrangeSqrDiff_temp1 = list()
  NNsmoothedsigmaSqrDiff_temp1 = list()
  
  NNinterceptSqrDiff_temp1 = list()
  NNbeta_distRiversLakesSqrDiff_temp1 = list()
  NNbeta_accessCitiesSqrDiff_temp1 = list()
  NNbeta_elevationSqrDiff_temp1 = list()
  NNbeta_populationSqrDiff_temp1 = list()
  NNbeta_urbanicitySqrDiff_temp1 = list()
  NNrangeSqrDiff_temp1 = list()
  NNsigmaSqrDiff_temp1 = list()
  
  # CinterceptSqrDiff_temp1 = list()
  # Cbeta_distRiversLakesSqrDiff_temp1 = list()
  # Cbeta_accessCitiesSqrDiff_temp1 = list()
  # Cbeta_elevationSqrDiff_temp1 = list()
  # Cbeta_populationSqrDiff_temp1 = list()
  # Cbeta_urbanicitySqrDiff_temp1 = list()
  # CrangeSqrDiff_temp1 = list()
  # CsigmaSqrDiff_temp1 = list()
  # 
  # RinterceptSqrDiff_temp1 = list()
  # Rbeta_distRiversLakesSqrDiff_temp1 = list()
  # Rbeta_accessCitiesSqrDiff_temp1 = list()
  # Rbeta_elevationSqrDiff_temp1 = list()
  # Rbeta_populationSqrDiff_temp1 = list()
  # Rbeta_urbanicitySqrDiff_temp1 = list()
  # RrangeSqrDiff_temp1 = list()
  # RsigmaSqrDiff_temp1 = list()
  
  CRinterceptSqrDiff_temp1 = list()
  CRbeta_distRiversLakesSqrDiff_temp1 = list()
  CRbeta_accessCitiesSqrDiff_temp1 = list()
  CRbeta_elevationSqrDiff_temp1 = list()
  CRbeta_populationSqrDiff_temp1 = list()
  CRbeta_urbanicitySqrDiff_temp1 = list()
  CRrangeSqrDiff_temp1 = list()
  CRsigmaSqrDiff_temp1 = list()
  
  
  for (l in 1:nSim){
    
    if (class(sims[[i]][[l]][["Results_NNsmoothed"]]) != "try-error"){
      if (flagNNsmoothed ==1){
        
        log_tau = sims[[i]][[l]][["Results_NNsmoothed"]][["fixed.par"]][["log_tau"]]
        log_kappa = sims[[i]][[l]][["Results_NNsmoothed"]][["fixed.par"]][["log_kappa"]]
        
        sp_range = sqrt(8.0)/exp(log_kappa)
        sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                exp(2.0 * log_tau) * exp(2.0 * log_kappa))
        
        NNsmoothedinterceptSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][1] - beta0true)^2
        NNsmoothedbeta_distRiversLakesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][2] - beta1true)^2
        NNsmoothedbeta_accessCitiesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][3] - beta2true)^2
        NNsmoothedbeta_elevationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][4] - beta3true)^2
        NNsmoothedbeta_populationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][5] - beta4true)^2
        NNsmoothedbeta_urbanicitySqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NNsmoothed"]][["mu"]][6] - beta5true)^2
        NNsmoothedrangeSqrDiff_temp1[[l]] = (sp_range - rangetrue)^2
        NNsmoothedsigmaSqrDiff_temp1[[l]] = (sp_sigma - sigmatrue)^2
      }}
    
    if (class(sims[[i]][[l]][["Results_NN"]]) != "try-error"){
      if (flagNN ==1){
        
        log_tau = sims[[i]][[l]][["Results_NN"]][["fixed.par"]][["log_tau"]]
        log_kappa = sims[[i]][[l]][["Results_NN"]][["fixed.par"]][["log_kappa"]]
        
        sp_range = sqrt(8.0)/exp(log_kappa)
        sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                exp(2.0 * log_tau) * exp(2.0 * log_kappa))
        
        NNinterceptSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][1] - beta0true)^2
        NNbeta_distRiversLakesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][2] - beta1true)^2
        NNbeta_accessCitiesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][3] - beta2true)^2
        NNbeta_elevationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][4] - beta3true)^2
        NNbeta_populationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][5] - beta4true)^2
        NNbeta_urbanicitySqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_NN"]][["mu"]][6] - beta5true)^2
        NNrangeSqrDiff_temp1[[l]] = (sp_range - rangetrue)^2
        NNsigmaSqrDiff_temp1[[l]] = (sp_sigma - sigmatrue)^2
      }}
    
    
    if (class(sims[[i]][[l]][["Results_C"]]) != "try-error"){
      if (flagC ==1){
        log_tau = sims[[i]][[l]][["Results_C"]][["fixed.par"]][["log_tau"]]
        log_kappa = sims[[i]][[l]][["Results_C"]][["fixed.par"]][["log_kappa"]]
        
        sp_range = sqrt(8.0)/exp(log_kappa)
        sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                exp(2.0 * log_tau) * exp(2.0 * log_kappa))
        
        CinterceptSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][1] - beta0true)^2
        Cbeta_distRiversLakesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][2] - beta1true)^2
        Cbeta_accessCitiesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][3] - beta2true)^2
        Cbeta_elevationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][4] - beta3true)^2
        Cbeta_populationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][5] - beta4true)^2
        Cbeta_urbanicitySqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_C"]][["mu"]][6] - beta5true)^2
        CrangeSqrDiff_temp1[[l]] = (sp_range - rangetrue)^2
        CsigmaSqrDiff_temp1[[l]] = (sp_sigma - sigmatrue)^2
      }}
    
    if (class(sims[[i]][[l]][["Results_R"]]) != "try-error"){
      if (flagR ==1){
        
        log_tau = sims[[i]][[l]][["Results_R"]][["fixed.par"]][["log_tau"]]
        log_kappa = sims[[i]][[l]][["Results_R"]][["fixed.par"]][["log_kappa"]]
        
        sp_range = sqrt(8.0)/exp(log_kappa)
        sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                exp(2.0 * log_tau) * exp(2.0 * log_kappa))
        
        RinterceptSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][1] - beta0true)^2
        Rbeta_distRiversLakesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][2] - beta1true)^2
        Rbeta_accessCitiesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][3] - beta2true)^2
        Rbeta_elevationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][4] - beta3true)^2
        Rbeta_populationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][5] - beta4true)^2
        Rbeta_urbanicitySqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_R"]][["mu"]][6] - beta5true)^2
        RrangeSqrDiff_temp1[[l]] = (sp_range - rangetrue)^2
        RsigmaSqrDiff_temp1[[l]] = (sp_sigma - sigmatrue)^2
      }}
    
    if (class(sims[[i]][[l]][["Results_CR"]]) != "try-error"){
      if (flagCR ==1){
        log_tau = sims[[i]][[l]][["Results_CR"]][["fixed.par"]][["log_tau"]]
        log_kappa = sims[[i]][[l]][["Results_CR"]][["fixed.par"]][["log_kappa"]]
        
        sp_range = sqrt(8.0)/exp(log_kappa)
        sp_sigma = 1.0 / sqrt(4.0 * 3.14159265359 *
                                exp(2.0 * log_tau) * exp(2.0 * log_kappa))
        
        CRinterceptSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][1] - beta0true)^2
        CRbeta_distRiversLakesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][2] - beta1true)^2
        CRbeta_accessCitiesSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][3] - beta2true)^2
        CRbeta_elevationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][4] - beta3true)^2
        CRbeta_populationSqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][5] - beta4true)^2
        CRbeta_urbanicitySqrDiff_temp1[[l]] = (sims[[i]][[l]][["Results_CR"]][["mu"]][6] - beta5true)^2
        CRrangeSqrDiff_temp1[[l]] = (sp_range - rangetrue)^2
        CRsigmaSqrDiff_temp1[[l]] = (sp_sigma - sigmatrue)^2
      }}
    
  }
  
  NNsmoothedinterceptRMSE[[i]] = sqrt(mean(unlist(NNsmoothedinterceptSqrDiff_temp1)))
  NNsmoothedbeta_distRiversLakesRMSE[[i]] = sqrt(mean(unlist(NNsmoothedbeta_distRiversLakesSqrDiff_temp1)))
  NNsmoothedbeta_accessCitiesRMSE[[i]] = sqrt(mean(unlist(NNsmoothedbeta_accessCitiesSqrDiff_temp1)))
  NNsmoothedbeta_elevationRMSE[[i]] = sqrt(mean(unlist(NNsmoothedbeta_elevationSqrDiff_temp1)))
  NNsmoothedbeta_populationRMSE[[i]] = sqrt(mean(unlist(NNsmoothedbeta_populationSqrDiff_temp1)))
  NNsmoothedbeta_urbanicityRMSE[[i]] = sqrt(mean(unlist(NNsmoothedbeta_urbanicitySqrDiff_temp1)))
  NNsmoothedrangeRMSE[[i]] = sqrt(mean(unlist(NNsmoothedrangeSqrDiff_temp1)))
  NNsmoothedsigmaRMSE[[i]] = sqrt(mean(unlist(NNsmoothedsigmaSqrDiff_temp1)))
  
  
  NNinterceptRMSE[[i]] = sqrt(mean(unlist(NNinterceptSqrDiff_temp1)))
  NNbeta_distRiversLakesRMSE[[i]] = sqrt(mean(unlist(NNbeta_distRiversLakesSqrDiff_temp1)))
  NNbeta_accessCitiesRMSE[[i]] = sqrt(mean(unlist(NNbeta_accessCitiesSqrDiff_temp1)))
  NNbeta_elevationRMSE[[i]] = sqrt(mean(unlist(NNbeta_elevationSqrDiff_temp1)))
  NNbeta_populationRMSE[[i]] = sqrt(mean(unlist(NNbeta_populationSqrDiff_temp1)))
  NNbeta_urbanicityRMSE[[i]] = sqrt(mean(unlist(NNbeta_urbanicitySqrDiff_temp1)))
  NNrangeRMSE[[i]] = sqrt(mean(unlist(NNrangeSqrDiff_temp1)))
  NNsigmaRMSE[[i]] = sqrt(mean(unlist(NNsigmaSqrDiff_temp1)))
  
  
  # CinterceptRMSE[[i]] = sqrt(mean(unlist(CinterceptSqrDiff_temp1)))
  # Cbeta_distRiversLakesRMSE[[i]] = sqrt(mean(unlist(Cbeta_distRiversLakesSqrDiff_temp1)))
  # Cbeta_accessCitiesRMSE[[i]] = sqrt(mean(unlist(Cbeta_accessCitiesSqrDiff_temp1)))
  # Cbeta_elevationRMSE[[i]] = sqrt(mean(unlist(Cbeta_elevationSqrDiff_temp1)))
  # Cbeta_populationRMSE[[i]] = sqrt(mean(unlist(Cbeta_populationSqrDiff_temp1)))
  # Cbeta_urbanicityRMSE[[i]] = sqrt(mean(unlist(Cbeta_urbanicitySqrDiff_temp1)))
  # CrangeRMSE[[i]] = sqrt(mean(unlist(CrangeSqrDiff_temp1)))
  # CsigmaRMSE[[i]] = sqrt(mean(unlist(CsigmaSqrDiff_temp1)))
  # 
  # RinterceptRMSE[[i]] = sqrt(mean(unlist(RinterceptSqrDiff_temp1)))
  # Rbeta_distRiversLakesRMSE[[i]] = sqrt(mean(unlist(Rbeta_distRiversLakesSqrDiff_temp1)))
  # Rbeta_accessCitiesRMSE[[i]] = sqrt(mean(unlist(Rbeta_accessCitiesSqrDiff_temp1)))
  # Rbeta_elevationRMSE[[i]] = sqrt(mean(unlist(Rbeta_elevationSqrDiff_temp1)))
  # Rbeta_populationRMSE[[i]]= sqrt(mean(unlist(Rbeta_populationSqrDiff_temp1)))
  # Rbeta_urbanicityRMSE[[i]] = sqrt(mean(unlist(Rbeta_urbanicitySqrDiff_temp1)))
  # RrangeRMSE[[i]] = sqrt(mean(unlist(RrangeSqrDiff_temp1)))
  # RsigmaRMSE[[i]] = sqrt(mean(unlist(RsigmaSqrDiff_temp1)))
  
  
    CRinterceptRMSE[[i]] = sqrt(mean(unlist(CRinterceptSqrDiff_temp1)))
    CRbeta_distRiversLakesRMSE[[i]] = sqrt(mean(unlist(CRbeta_distRiversLakesSqrDiff_temp1)))
    CRbeta_accessCitiesRMSE[[i]] = sqrt(mean(unlist(CRbeta_accessCitiesSqrDiff_temp1)))
    CRbeta_elevationRMSE[[i]] = sqrt(mean(unlist(CRbeta_elevationSqrDiff_temp1)))
    CRbeta_populationRMSE[[i]]= sqrt(mean(unlist(CRbeta_populationSqrDiff_temp1)))
    CRbeta_urbanicityRMSE[[i]] = sqrt(mean(unlist(CRbeta_urbanicitySqrDiff_temp1)))
    CRrangeRMSE[[i]] = sqrt(mean(unlist(CRrangeSqrDiff_temp1)))
    CRsigmaRMSE[[i]] = sqrt(mean(unlist(CRsigmaSqrDiff_temp1)))
  
}





rmseEducation = list(NN = list(NNinterceptRMSE = NNinterceptRMSE,
                               NNbeta_distRiversLakesRMSE = NNbeta_distRiversLakesRMSE,
                               NNbeta_accessCitiesRMSE = NNbeta_accessCitiesRMSE,
                               NNbeta_elevationRMSE = NNbeta_elevationRMSE,
                               NNbeta_populationRMSE = NNbeta_populationRMSE,
                               NNbeta_urbanicityRMSE = NNbeta_urbanicityRMSE,
                               NNrangeRMSE = NNrangeRMSE,
                               NNsigmaRMSE = NNsigmaRMSE
),
NNsmoothed = list(NNsmoothedinterceptRMSE = NNsmoothedinterceptRMSE,
                  NNsmoothedbeta_distRiversLakesRMSE = NNsmoothedbeta_distRiversLakesRMSE,
                  NNsmoothedbeta_accessCitiesRMSE = NNsmoothedbeta_accessCitiesRMSE,
                  NNsmoothedbeta_elevationRMSE = NNsmoothedbeta_elevationRMSE,
                  NNsmoothedbeta_populationRMSE = NNsmoothedbeta_populationRMSE,
                  NNsmoothedbeta_urbanicityRMSE = NNsmoothedbeta_urbanicityRMSE,
                  NNsmoothedrangeRMSE = NNsmoothedrangeRMSE,
                  NNsmoothedsigmaRMSE = NNsmoothedsigmaRMSE
),
# C = list(CinterceptRMSE = CinterceptRMSE,
#          Cbeta_distRiversLakesRMSE = Cbeta_distRiversLakesRMSE,
#          Cbeta_accessCitiesRMSE = Cbeta_accessCitiesRMSE,
#          Cbeta_elevationRMSE = Cbeta_elevationRMSE,
#          Cbeta_populationRMSE = Cbeta_populationRMSE,
#          Cbeta_urbanicityRMSE = Cbeta_urbanicityRMSE,
#          CrangeRMSE = CrangeRMSE,
#          CsigmaRMSE = CsigmaRMSE
# ),
# R = list(RinterceptRMSE = RinterceptRMSE,
#          Rbeta_distRiversLakesRMSE = Rbeta_distRiversLakesRMSE,
#          Rbeta_accessCitiesRMSE = Rbeta_accessCitiesRMSE,
#          Rbeta_elevationRMSE = Rbeta_elevationRMSE,
#          Rbeta_populationRMSE = Rbeta_populationRMSE,
#          Rbeta_urbanicityRMSE = Rbeta_urbanicityRMSE,
#          RrangeRMSE = RrangeRMSE,
#          RsigmaRMSE = RsigmaRMSE
# ),
CR = list(CRinterceptRMSE = CRinterceptRMSE,
          CRbeta_distRiversLakesRMSE = CRbeta_distRiversLakesRMSE,
          CRbeta_accessCitiesRMSE = CRbeta_accessCitiesRMSE,
          CRbeta_elevationRMSE = CRbeta_elevationRMSE,
          CRbeta_populationRMSE = CRbeta_populationRMSE,
          CRbeta_urbanicityRMSE = CRbeta_urbanicityRMSE,
          CRrangeRMSE = CRrangeRMSE,
          CRsigmaRMSE = CRsigmaRMSE
))


save(rmseEducation, file = "rmseEducation.RData")











