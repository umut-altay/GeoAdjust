# Box-plot friendly results

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
                beta1BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta1true
                beta2BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta2true
                beta3BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta3true
                beta4BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta4true
                beta5BiasC_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_C"]][["mu"]][1] - beta5true
                rangeBiasC_temp5[[l]] = sp_range - rangetrue
                sigmaBiasC_temp5[[l]] = sp_sigma - sigmatrue
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
                # }
              }}
            #}#else{next}
            ####################################################################
            
          }    
          
          if (flagNN ==1){
            crpsNN_temp4[[h]] = unlist(crpsNN_temp5)
            rmseNN_temp4[[h]] = unlist(rmseNN_temp5)
            logscoreNN_temp4[[h]] = unlist(logscoreNN_temp5)
            coverageNN_temp4[[h]] = unlist(coverageNN_temp5)
            BIASNN_temp4[[h]] = unlist(BIASNN_temp5)
            beta0BiasNN_temp4[[h]] = unlist(beta0BiasNN_temp5)
            beta1BiasNN_temp4[[h]] = unlist(beta1BiasNN_temp5)
            beta2BiasNN_temp4[[h]] = unlist(beta2BiasNN_temp5)
            beta3BiasNN_temp4[[h]] = unlist(beta3BiasNN_temp5)
            beta4BiasNN_temp4[[h]] = unlist(beta4BiasNN_temp5)
            beta5BiasNN_temp4[[h]] = unlist(beta5BiasNN_temp5)
            rangeBiasNN_temp4[[h]] = unlist(rangeBiasNN_temp5)
            sigmaBiasNN_temp4[[h]] = unlist(sigmaBiasNN_temp5)
          }
          
          if (flagNNsmoothed ==1){
            crpsNNsmoothed_temp4[[h]] = unlist(crpsNNsmoothed_temp5)
            rmseNNsmoothed_temp4[[h]] = unlist(rmseNNsmoothed_temp5)
            logscoreNNsmoothed_temp4[[h]] = unlist(logscoreNNsmoothed_temp5)
            coverageNNsmoothed_temp4[[h]] = unlist(coverageNNsmoothed_temp5)
            BIASNNsmoothed_temp4[[h]] = unlist(BIASNNsmoothed_temp5)
            beta0BiasNNsmoothed_temp4[[h]] = unlist(beta0BiasNNsmoothed_temp5)
            beta1BiasNNsmoothed_temp4[[h]] = unlist(beta1BiasNNsmoothed_temp5)
            beta2BiasNNsmoothed_temp4[[h]] = unlist(beta2BiasNNsmoothed_temp5)
            beta3BiasNNsmoothed_temp4[[h]] = unlist(beta3BiasNNsmoothed_temp5)
            beta4BiasNNsmoothed_temp4[[h]] = unlist(beta4BiasNNsmoothed_temp5)
            beta5BiasNNsmoothed_temp4[[h]] = unlist(beta5BiasNNsmoothed_temp5)
            rangeBiasNNsmoothed_temp4[[h]] = unlist(rangeBiasNNsmoothed_temp5)
            sigmaBiasNNsmoothed_temp4[[h]] = unlist(sigmaBiasNNsmoothed_temp5)
          }
          
          
          if (flagC ==1){
            crpsC_temp4[[h]] = unlist(crpsC_temp5)
            rmseC_temp4[[h]] = unlist(rmseC_temp5)
            logscoreC_temp4[[h]] = unlist(logscoreC_temp5)
            coverageC_temp4[[h]] = unlist(coverageC_temp5)
            BIASC_temp4[[h]] = unlist(BIASC_temp5)
            beta0BiasC_temp4[[h]] = unlist(beta0BiasC_temp5)
            beta1BiasC_temp4[[h]] = unlist(beta1BiasC_temp5)
            beta2BiasC_temp4[[h]] = unlist(beta2BiasC_temp5)
            beta3BiasC_temp4[[h]] = unlist(beta3BiasC_temp5)
            beta4BiasC_temp4[[h]] = unlist(beta4BiasC_temp5)
            beta5BiasC_temp4[[h]] = unlist(beta5BiasC_temp5)
            rangeBiasC_temp4[[h]] = unlist(rangeBiasC_temp5)
            sigmaBiasC_temp4[[h]] = unlist(sigmaBiasC_temp5)
            
          }
          if (flagR ==1){
            crpsR_temp4[[h]] = unlist(crpsR_temp5)
            rmseR_temp4[[h]] = unlist(rmseR_temp5)
            logscoreR_temp4[[h]] = unlist(logscoreR_temp5)
            coverageR_temp4[[h]] = unlist(coverageR_temp5)
            BIASR_temp4[[h]] = unlist(BIASR_temp5)
            beta0BiasR_temp4[[h]] = unlist(beta0BiasR_temp5)
            beta1BiasR_temp4[[h]] = unlist(beta1BiasR_temp5)
            beta2BiasR_temp4[[h]] = unlist(beta2BiasR_temp5)
            beta3BiasR_temp4[[h]] = unlist(beta3BiasR_temp5)
            beta4BiasR_temp4[[h]] = unlist(beta4BiasR_temp5)
            beta5BiasR_temp4[[h]] = unlist(beta5BiasR_temp5)
            rangeBiasR_temp4[[h]] = unlist(rangeBiasR_temp5)
            sigmaBiasR_temp4[[h]] = unlist(sigmaBiasR_temp5)
          }
          if (flagCR ==1){
            crpsCR_temp4[[h]] = unlist(crpsCR_temp5)
            rmseCR_temp4[[h]] = unlist(rmseCR_temp5)
            logscoreCR_temp4[[h]] = unlist(logscoreCR_temp5)
            coverageCR_temp4[[h]] = unlist(coverageCR_temp5)
            BIASCR_temp4[[h]] = unlist(BIASCR_temp5)
            beta0BiasCR_temp4[[h]] = unlist(beta0BiasCR_temp5)
            beta1BiasCR_temp4[[h]] = unlist(beta1BiasCR_temp5)
            beta2BiasCR_temp4[[h]] = unlist(beta2BiasCR_temp5)
            beta3BiasCR_temp4[[h]] = unlist(beta3BiasCR_temp5)
            beta4BiasCR_temp4[[h]] = unlist(beta4BiasCR_temp5)
            beta5BiasCR_temp4[[h]] = unlist(beta5BiasCR_temp5)
            rangeBiasCR_temp4[[h]] = unlist(rangeBiasCR_temp5)
            sigmaBiasCR_temp4[[h]] = unlist(sigmaBiasCR_temp5)
            
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
  }        
}


boxPlotFriendlyResults = list(NN = list(crpsNN = crpsNN,
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
NNsmoothed = list(crpsNNsmoothed = crpsNNsmoothed,
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
# C = list(crpsC = crpsC,
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
# R = list(crpsR = crpsR,
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
CR = list(crpsCR = crpsCR,
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


save(boxPlotFriendlyResults, file = "boxPlotFriendly.RData")


# Box-plots of CRPS and RMSE :

crpsNNbeta1 = boxPlotFriendlyResults[["NN"]][["crpsNN"]][[1]]
crpsNNbeta2 = boxPlotFriendlyResults[["NN"]][["crpsNN"]][[2]]
crpsNNbeta3 = boxPlotFriendlyResults[["NN"]][["crpsNN"]][[3]]

crpsNNsmoothedbeta1 = boxPlotFriendlyResults[["NNsmoothed"]][["crpsNNsmoothed"]][[1]]
crpsNNsmoothedbeta2 = boxPlotFriendlyResults[["NNsmoothed"]][["crpsNNsmoothed"]][[2]]
crpsNNsmoothedbeta3 = boxPlotFriendlyResults[["NNsmoothed"]][["crpsNNsmoothed"]][[3]]

crpsCRbeta1 = boxPlotFriendlyResults[["CR"]][["crpsCR"]][[1]]
crpsCRbeta2 = boxPlotFriendlyResults[["CR"]][["crpsCR"]][[2]]
crpsCRbeta3 = boxPlotFriendlyResults[["CR"]][["crpsCR"]][[3]]

rmseNNbeta1 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[1]]
rmseNNbeta2 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[2]]
rmseNNbeta3 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[3]]

rmseNNsmoothedbeta1 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[1]]
rmseNNsmoothedbeta2 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[2]]
rmseNNsmoothedbeta3 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[3]]

rmseCRbeta1 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[1]]
rmseCRbeta2 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[2]]
rmseCRbeta3 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[3]]


#CRPS

d_crpsBeta1 <- data.frame(betas = rep("SignalLow", 150),
                          model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                          crps = c(crpsNNsmoothedbeta1, crpsNNbeta1, crpsCRbeta1)
)            


d_crpsBeta1$model <- factor(d_crpsBeta1$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)



d_crpsBeta2 <- data.frame(betas = rep("SignalMed", 150),
                          model = c(rep("Smoothed", 50),rep("UnAdj", 50), rep("FullAdj", 50)),
                          crps = c(crpsNNsmoothedbeta2, crpsNNbeta2, crpsCRbeta2)
)               

d_crpsBeta2$model <- factor(d_crpsBeta2$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)


d_crpsBeta3 <- data.frame(betas = rep("SignalHigh", 150),
                          model = c(rep("Smoothed", 50),rep("UnAdj", 50), rep("FullAdj", 50)),
                          crps = c(crpsNNsmoothedbeta3, crpsNNbeta3, crpsCRbeta3)
)              

d_crpsBeta3$model <- factor(d_crpsBeta3$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)


# Plots
library(ggplot2)

g1 = ggplot(d_crpsBeta1, aes(x = model, y = crps)) + #theme_bw() +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) +# stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  #ggtitle("CRPS") + 
  ggtitle("SignalLow") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  #xlab("") + ylab("SignalLow") + #
  xlab("") +ylab("CRPS\n") + 
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))


g2 = ggplot(d_crpsBeta2, aes(x = model, y = crps)) + #theme_bw() +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + #stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  ggtitle("SignalMed") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))



g3 = ggplot(d_crpsBeta3, aes(x = model, y = crps)) + #theme_bw() + #scale_y_continuous(trans='log10')+
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + #stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  ggtitle("SignalHigh") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))



#RMSE
rmseNNbeta1 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[1]]
rmseNNbeta2 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[2]]
rmseNNbeta3 = boxPlotFriendlyResults[["NN"]][["rmseNN"]][[3]]

rmseNNsmoothedbeta1 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[1]]
rmseNNsmoothedbeta2 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[2]]
rmseNNsmoothedbeta3 = boxPlotFriendlyResults[["NNsmoothed"]][["rmseNNsmoothed"]][[3]]

rmseCRbeta1 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[1]]
rmseCRbeta2 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[2]]
rmseCRbeta3 = boxPlotFriendlyResults[["CR"]][["rmseCR"]][[3]]

d_rmseBeta1 <- data.frame(betas = rep("SignalLow", 150),
                          model = c(rep("Smoothed", 50),rep("UnAdj", 50), rep("FullAdj", 50)),
                          rmse = c(rmseNNsmoothedbeta1,rmseNNbeta1, rmseCRbeta1)
)            

d_rmseBeta1$model <- factor(d_rmseBeta1$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)



d_rmseBeta2 <- data.frame(betas = rep("SignalMed", 150),
                          model = c(rep("Smoothed", 50),rep("UnAdj", 50), rep("FullAdj", 50)),
                          rmse = c(rmseNNsmoothedbeta2,rmseNNbeta2, rmseCRbeta2)
)               

d_rmseBeta2$model <- factor(d_rmseBeta2$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)


d_rmseBeta3 <- data.frame(betas = rep("SignalHigh", 150),
                          model = c(rep("Smoothed", 50),rep("UnAdj", 50), rep("FullAdj", 50)),
                          rmse = c(rmseNNsmoothedbeta3,rmseNNbeta3, rmseCRbeta3)
)              

d_rmseBeta3$model <- factor(d_rmseBeta3$model,
                            levels = c("Smoothed","UnAdj", "FullAdj"),ordered = TRUE)

g4 = ggplot(d_rmseBeta1, aes(x = model, y = rmse)) +# theme_bw() +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + #stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  #ggtitle("RMSE") + #ggtitle("Scaled by 0.5") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + #ylab("") + #
  ylab("RMSE\n") + 
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))


g5 = ggplot(d_rmseBeta2, aes(x = model, y = rmse))  + #theme_bw() +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + #stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  #ggtitle("Non-scaled") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))



g6 = ggplot(d_rmseBeta3, aes(x = model, y = rmse))  +# theme_bw() + #scale_y_continuous(trans='log10')+
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + #stat_summary(fun.data = give.n, geom = "text", fun = median, size=7, color = "blue")  +
  #ggtitle("Scaled by 2") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))



library(gridExtra)
library(grid)
pdf("boxplotsScores.pdf", width = 25, height = 14) # Open a new pdf file
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2,ncol = 3) # Write the grid.arrange in the file
dev.off() # Close the file

################################################################################

# Box plots for urbanicity coefficient and range

nBeta = 3 # of different sets of beta values that are used to simulate data
nBoundarySc = 1 # of boundary options that we used
nLikelihood = 1 # of different likelihoods that we used
nRange = 1 # of different range values that we used  
nScale = 1 # of different jittering scales that we used 
nSim = 50 # of simulations we have for each scenario
#options(error=recover)

# Load the resulting object of the real data fitting and extract the true values:

# the models that we want to extract the results for (on/off-->1/0) :
flagNN = 1
flagCR = 1
flagNNsmoothed = 1

if (flagNN ==1){
  rangeNN = list()
  urbBetaNN = list()
}
if (flagNNsmoothed ==1){
  rangeNNsmoothed = list()
  urbBetaNNsmoothed = list()
}

if (flagCR ==1){
  rangeCR= list()
  urbBetaCR = list()
}


for(k in 1:nBeta){
  if (flagNN ==1){
    rangeNN_temp1 = list()
    urbBetaNN_temp1 = list()
    
  }
  
  if (flagNNsmoothed ==1){
    rangeNNsmoothed_temp1 = list()
    urbBetaNNsmoothed_temp1 = list()
  }
  
  if (flagCR ==1){
    rangeCR_temp1 = list()
    urbBetaCR_temp1 = list()
  }
  
  for (g in 1:nBoundarySc){
    if (flagNN ==1){
      rangeNN_temp2 = list()
      urbBetaNN_temp2 = list()
    }
    
    if (flagNNsmoothed ==1){
      rangeNNsmoothed_temp2 = list()
      urbBetaNNsmoothed_temp2 = list()
    }
  
    if (flagCR ==1){
      rangeCR_temp2 = list()
      urbBetaCR_temp2 = list()
    }
    
    for (i in 1:nLikelihood){
      if (flagNN ==1){
        rangeNN_temp3 = list()
        urbBetaNN_temp3 = list()
      }
      
      
      if (flagNNsmoothed ==1){
        rangeNNsmoothed_temp3 = list()
        urbBetaNNsmoothed_temp3 = list()
      }
      
      
      if (flagCR ==1){
        rangeCR_temp3 = list()
        urbBetaCR_temp3 = list()
      }
      
      for (j in 1:nRange){
        if (flagNN ==1){
          rangeNN_temp4 = list()
          urbBetaNN_temp4 = list()
        }
        
        if (flagNNsmoothed ==1){
          rangeNNsmoothed_temp4 = list()
          urbBetaNNsmoothed_temp4 = list()
        }
        
        
        if (flagCR ==1){
          rangeCR_temp4 = list()
          urbBetaCR_temp4 = list()
        }
        for (h in 1:nScale){
          if (flagNN ==1){
            rangeNN_temp5 = list()
            urbBetaNN_temp5 = list()
          }
          
          if (flagNNsmoothed ==1){
            rangeNNsmoothed_temp5 = list()
            urbBetaNNsmoothed_temp5 = list()
          }
          
          
          if (flagCR ==1){
            rangeCR_temp5 = list()
            urbBetaCR_temp5 = list()
          }
          
          for(l in 1:nSim){
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]]) != "try-error"){
              if (flagNN ==1){
                #if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]])) != "try-error") ){
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["fixed.par"]][["log_kappa"]]
                sp_range = sqrt(8.0)/exp(log_kappa)
                rangeNN_temp5[[l]] = sp_range
                urbBetaNN_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NN"]][["mu"]][6]
                
                #}
              }}
            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]]) != "try-error"){
              if (flagNNsmoothed ==1){
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["fixed.par"]][["log_kappa"]]
                sp_range = sqrt(8.0)/exp(log_kappa)
                rangeNNsmoothed_temp5[[l]] = sp_range
                urbBetaNNsmoothed_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_NNsmoothed"]][["mu"]][6]
              }}

            
            if (class(outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]]) != "try-error"){  
              if (flagCR ==1){
                
                log_kappa = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["fixed.par"]][["log_kappa"]]
                sp_range = sqrt(8.0)/exp(log_kappa)
                rangeCR_temp5[[l]] = sp_range
                urbBetaCR_temp5[[l]] = outputTMB[[k]][[g]][[i]][[j]][[h]][[l]][["Results_CR"]][["mu"]][6]
                
                # }
              }}
            
            ####################################################################
            
          }    
          
          if (flagNN ==1){
            
            rangeNN_temp4[[h]] = unlist(rangeNN_temp5)
            urbBetaNN_temp4[[h]] = unlist(urbBetaNN_temp5)
          }
          
          if (flagNNsmoothed ==1){
            rangeNNsmoothed_temp4[[h]] = unlist(rangeNNsmoothed_temp5)
            urbBetaNNsmoothed_temp4[[h]] = unlist(urbBetaNNsmoothed_temp5)
          }
          
          
          if (flagCR ==1){
            rangeCR_temp4[[h]] = unlist(rangeCR_temp5)
            urbBetaCR_temp4[[h]] = unlist(urbBetaCR_temp5)
            
            
          }
          ##########################################
        }
        
        if (flagNN ==1){
          
          rangeNN_temp3[[j]] = rangeNN_temp4[[1]]
          urbBetaNN_temp3[[j]] = urbBetaNN_temp4[[1]]
        }
        
        
        if (flagNNsmoothed ==1){
          rangeNNsmoothed_temp3[[j]] = rangeNNsmoothed_temp4[[1]]
          urbBetaNNsmoothed_temp3[[j]] = urbBetaNNsmoothed_temp4[[1]]
        }
     
        if (flagCR ==1){
          
          rangeCR_temp3[[j]] = rangeCR_temp4[[1]]
          urbBetaCR_temp3[[j]] = urbBetaCR_temp4[[1]]
        }   
        ##############################
      }
      
      
      if (flagNN ==1){
        
        rangeNN_temp2[[i]] = rangeNN_temp3[[1]]
        urbBetaNN_temp2[[i]] = urbBetaNN_temp3[[1]]
      }
      
      if (flagNNsmoothed ==1){
        rangeNNsmoothed_temp2[[i]] = rangeNNsmoothed_temp3[[1]]
        urbBetaNNsmoothed_temp2[[i]] = urbBetaNNsmoothed_temp3[[1]]
      }
     
      if (flagCR ==1){
        
        rangeCR_temp2[[i]] = rangeCR_temp3[[1]]
        urbBetaCR_temp2[[i]] = urbBetaCR_temp3[[1]]
      } 
      ############################
    }
    
    
    if (flagNN ==1){
      
      rangeNN_temp1[[g]] = rangeNN_temp2[[1]]
      urbBetaNN_temp1[[g]] = urbBetaNN_temp2[[1]]
    }
    
    if (flagNNsmoothed ==1){
      rangeNNsmoothed_temp1[[g]] = rangeNNsmoothed_temp2[[1]]
      urbBetaNNsmoothed_temp1[[g]] = urbBetaNNsmoothed_temp2[[1]]
    }

    if (flagCR ==1){
      
      rangeCR_temp1[[g]] = rangeCR_temp2[[1]]
      urbBetaCR_temp1[[g]] = urbBetaCR_temp2[[1]]
    }       
    ################################
  }
  
  
  if (flagNN ==1){
    
    rangeNN[[k]] = rangeNN_temp1[[1]]
    urbBetaNN[[k]] = urbBetaNN_temp1[[1]]
  }
  
  if (flagNNsmoothed ==1){
    rangeNNsmoothed[[k]] = rangeNNsmoothed_temp1[[1]]
    urbBetaNNsmoothed[[k]] = urbBetaNNsmoothed_temp1[[1]]
  }
  
 
  if (flagCR ==1){
    
    rangeCR[[k]] = rangeCR_temp1[[1]]
    urbBetaCR[[k]] = urbBetaCR_temp1[[1]]
  }        
}


rangeUrban = list(rangeNN = rangeNN,
                  urbBetaNN = urbBetaNN,
                  rangeNNsmoothed = rangeNNsmoothed,
                  urbBetaNNsmoothed = urbBetaNNsmoothed,
                  rangeCR = rangeCR,
                  urbBetaCR = urbBetaCR)


save(rangeUrban, file = "rangeUrban.RData")


rangeCR
urbBetaCR
rangeNN
urbBetaNN


rangeNN1 = rangeNN[[1]]
rangeNN2 = rangeNN[[2]]
rangeNN3 = rangeNN[[3]]

rangeCR1 = rangeCR[[1]]
rangeCR2 = rangeCR[[2]]
rangeCR3 = rangeCR[[3]]


urbBetaNN1 = urbBetaNN[[1]]
urbBetaNN2 = urbBetaNN[[2]]
urbBetaNN3 = urbBetaNN[[3]]

urbBetaCR1 = urbBetaCR[[1]]
urbBetaCR2 = urbBetaCR[[2]]
urbBetaCR3 = urbBetaCR[[3]]


rangeNNsmoothed1 = rangeNNsmoothed[[1]]
rangeNNsmoothed2 = rangeNNsmoothed[[2]]
rangeNNsmoothed3 = rangeNNsmoothed[[3]]

urbBetaNNsmoothed1 = urbBetaNNsmoothed[[1]]
urbBetaNNsmoothed2 = urbBetaNNsmoothed[[2]]
urbBetaNNsmoothed3 = urbBetaNNsmoothed[[3]]


# Plots

# urbanization Beta

d_urbBeta1 <- data.frame(betas = rep("SignalLow", 150),
                         model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                         urbBeta = c(urbBetaNNsmoothed1, urbBetaNN1, urbBetaCR1)
)            


d_urbBeta1$model <- factor(d_urbBeta1$model,
                           levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)



d_urbBeta2 <- data.frame(betas = rep("SignalMed", 150),
                         model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                         urbBeta = c(urbBetaNNsmoothed2, urbBetaNN2, urbBetaCR2)
)            
d_urbBeta2$model <- factor(d_urbBeta2$model,
                           levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)



d_urbBeta3 <- data.frame(betas = rep("SignalHigh", 150),
                         model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                         urbBeta = c(urbBetaNNsmoothed3, urbBetaNN3, urbBetaCR3)
)            
d_urbBeta3$model <- factor(d_urbBeta3$model,
                           levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)

 
library(ggplot2)


vals = c(urbBetaNNsmoothed1, urbBetaNN1, urbBetaCR1,
         urbBetaNNsmoothed2, urbBetaNN2, urbBetaCR2,
         urbBetaNNsmoothed3, urbBetaNN3, urbBetaCR3)

# > summary(vals)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-4.73325 -1.72363 -0.92606 -1.29552 -0.50663 -0.03052  

betas = rbind(c(-1.10757764, 0.31159259, -0.21762095, -0.01195374,  0.16501102, -0.67830024),
              c(-2.215155, 0.6231852, -0.4352419, -0.02390748, 0.3300220, -1.3566005),
              c(-3.32273250, 0.93477780, -0.65286285, -0.03586122, 0.49503300, -2.03490075))

> betas[1,6]
[1] -0.6783002
> betas[2,6]
[1] -1.356601
> betas[3,6]
[1] -2.034901

p1 = ggplot(d_urbBeta1, aes(x = model, y = urbBeta)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  #ggtitle("SignalLow") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab(expression(beta[UrbR])) + 
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(aes(yintercept= -0.6783002), color="#a50f15")+ 
  coord_cartesian(ylim = c(-4.73325, -0.03052))


p2 = ggplot(d_urbBeta2, aes(x = model, y = urbBeta)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  #ggtitle("SignalMed") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(aes(yintercept=-1.356601), color="#a50f15")+ 
  coord_cartesian(ylim = c(-4.73325, -0.03052))



p3 = ggplot(d_urbBeta3, aes(x = model, y = urbBeta)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  #ggtitle("SignalHigh") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(aes(yintercept=-2.034901), color="#a50f15")+ 
  coord_cartesian(ylim = c(-4.73325, -0.03052))


# Range

# real data estimate with adjusted model for the spatial range
log_tau = Results_CR[["fixed.par"]][["log_tau"]]
log_kappa = Results_CR[["fixed.par"]][["log_kappa"]]

# > log_tau
# [1] 1.872673
# > log_kappa
# [1] -3.639456

sp_range = sqrt(8.0)/exp(log_kappa)

# > sp_range
# [1] 107.6814


d_range1 <- data.frame(betas = rep("SignalLow", 150),
                       model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                       range = c(rangeNNsmoothed1, rangeNN1, rangeCR1)
)            

d_range1$model <- factor(d_range1$model,
                         levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)

d_range2 <- data.frame(betas = rep("SignalMed", 150),
                       model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                       range = c(rangeNNsmoothed2, rangeNN2, rangeCR2)
)            

d_range2$model <- factor(d_range2$model,
                         levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)

d_range3 <- data.frame(betas = rep("SignalHigh", 150),
                       model = c(rep("Smoothed", 50), rep("UnAdj", 50), rep("FullAdj", 50)),
                       range = c(rangeNNsmoothed3, rangeNN3, rangeCR3)
)            

d_range3$model <- factor(d_range3$model,
                         levels = c("Smoothed", "UnAdj","FullAdj"),ordered = TRUE)


vals2 = c(rangeNNsmoothed1, rangeNN1, rangeCR1,rangeNNsmoothed2, rangeNN2, rangeCR2, rangeNNsmoothed3, rangeNN3, rangeCR3)


> summary(vals2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
48.80   80.37   95.32   94.85  109.45  152.73  

library(ggplot2)      

p4 = ggplot(d_range1, aes(x = model, y = range)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  ggtitle("SignalLow") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab(expression(rho[S])) + #ylab("range\n") + 
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(aes(yintercept=107.6814), color="#a50f15")+ 
  coord_cartesian(ylim = c(48.80, 152.73))


p5 = ggplot(d_range2, aes(x = model, y = range)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  ggtitle("SignalMed") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") + # ylab("") + 
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(aes(yintercept=107.6814), color="#a50f15")+ 
  coord_cartesian(ylim = c(48.80, 152.73))



p6 = ggplot(d_range3, aes(x = model, y = range)) +
  geom_boxplot(position = "dodge2", outlier.alpha = 0.1) + 
  ggtitle("SignalHigh") +
  theme(axis.text.y = element_text(size = 30), axis.text.x = element_text(size = 30)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3))) +
  xlab("") + ylab("") +  
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(aes(yintercept=107.6814), color="#a50f15")+ 
  coord_cartesian(ylim = c(48.80, 152.73))


library(gridExtra)
library(grid)

pdf("boxplotsRangeUrban.pdf", width = 25, height = 14) # Open a new pdf file
grid.arrange(p4, p5, p6, p1, p2, p3, nrow = 2,ncol = 3) # Write the grid.arrange in the file
dev.off() # Close the file













