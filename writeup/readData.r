

weighted.sd = function(vec, weight) {
  if(length(vec) == 1) {
     return(NA)
  }
  return(sqrt(weighted.mean(vec*vec, weight, na.rm=TRUE) - weighted.mean(vec, weight, na.rm=TRUE)**2))
}

languages = c('Hebrew', 'Romanian', 'Finnish', 'Danish', 'Old_Church_Slavonic', 'Galician-TreeGal', 'Swedish-LinES', 'Marathi', 'Greek', 'Latin-PROIEL', 'Polish', 'Spanish-AnCora', 'Finnish-FTB', 'Kazakh', 'Arabic', 'Japanese', 'Slovenian', 'Ancient_Greek-PROIEL', 'Latvian', 'Swedish_Sign_Language', 'Coptic', 'Turkish', 'Ancient_Greek', 'Ukrainian', 'Hungarian', 'Russian-SynTagRus', 'Italian-ParTUT', 'Chinese', 'Dutch-LassySmall', 'Italian', 'Bulgarian', 'Irish', 'Romanian-Nonstandard', 'Norwegian-Nynorsk', 'Indonesian', 'Latin-ITTB', 'Tamil', 'French-Sequoia', 'Belarusian', 'Lithuanian', 'Afrikaans', 'Persian', 'Portuguese-BR', 'Croatian', 'Russian', 'English-ParTUT', 'Arabic-NYUAD', 'Estonian', 'Gothic', 'Telugu', 'Czech-CLTT', 'Catalan', 'Dutch', 'French-FTB', 'Spanish', 'English', 'French', 'Galician', 'Slovenian-SST', 'Korean', 'Portuguese', 'Basque', 'German', 'Urdu', 'Hindi', 'Slovak', 'Czech-CAC', 'Italian-PoSTWITA', 'Latin', 'Swedish', 'Vietnamese', 'French-ParTUT', 'Czech', 'Norwegian-Bokmaal', 'North_Sami', 'English-LinES', 'Serbian', 'Czech-FicTree')

library(dplyr)
library(tidyr)
library(ggplot2)



options(width=130) 

auto = read.csv("CS_SCR/deps/manual_output/auto-summary.tsv", sep="\t")# %>% rename(Quality=AverageLength)
auto$Direction = NA
auto$FileName = as.character(auto$FileName)
data = read.csv("CS_SCR/deps/manual_output/results.tsv", sep="\t") %>% rename(AverageLoss=Perplexity)
data$DH_Weight = NA
data$Counter = NA
data$Language = "English"
#data$Objective = NA
data$FileName = as.character(data$FileName)
#data$ObjectiveName = NA
data$EntropyWeight = NA
data$LR_POLICY=NA
data$Lagrange_Lambda=NA
data$Lagrange_B=NA
data$L2_Weight=NA
data = bind_rows(data, auto)


#aggregate(data["Counter"], by=c(data["Language"], data["FileName"], data["ModelName"], data["ObjectiveName"]), NROW)

# important that Counter is never NA

data = data %>% filter(Counter > 10)
################################

forAll = c("readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorpora.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopes.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUID.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVar.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarDepLBugfix.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDDepLBugfix.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDLogDepL.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarDepLBugfix.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDDepLBugfix_UIDs.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUID.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDDepLBugfix_Tight.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarDepLBugfix_Tight_Guide.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarDepLBugfix_Tight.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDLogDepL_Tight.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUIDDepLBugfix_LogExp.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarDepLBugfix_LogExp.py")

forSurp = c("readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurp.py",  "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurp_LogVar.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurp_LogVar_Tight.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurp_LogVar_Tight_Guide.py")

forDepL = c("readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepL.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepLOnly_Bugfix.py",  "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepLOnly_Bugfix_LogVar.py" , "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepLOnly_Bugfix_LogVar_TightVariancePrior.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepLOnly_Bugfix_LogExpVar.py")

forSurpWord = c("readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurpWord_LogVar.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurpWord_LogVar_Tight.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaSurpWord_LogExpVar.py")

data = data %>% mutate(RegType = ifelse(ModelName %in% forSurp, "Surp", 
                                 ifelse(ModelName %in% forDepL, "DepL", 
                                 ifelse(ModelName %in% forAll, "DL+Surp+ID", 
                                 ifelse(ModelName %in% forSurpWord, "SurpWord",
                                 ifelse(ModelName == "readDataRegressionDepLengthAndSurprisalRandomEffectsVariational.py", "All_NoSlope", "NONE"))))))

cat("\nAssigned RegType\n")
cat(unique((data %>% filter(RegType == "NONE"))$ModelName), sep="\n")
 
                                                          
badModels = c("readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorpora.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopes.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVarRateUID.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaLogVar.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUID.py", "readDataRegressionDepLengthAndSurprisalRandomEffectsVariationalUIDRandInterceptSlopesAllCorporaDepL.py" )
data = data %>% filter(!(ModelName %in% badModels))

















cat("Take into account (1) log variances, (2) UID rate vs UID")
data = data %>% mutate(Var_Slope_Surp_POS = ifelse(is.na(Var_Slope_Surp_POS), exp(LogVar_Slope_Surp_POS), Var_Slope_Surp_POS))
data = data %>% mutate(Var_Slope_Surp_Word = ifelse(is.na(Var_Slope_Surp_Word), exp(LogVar_Slope_Surp_Word), Var_Slope_Surp_Word))
data$Var_Slope_UIDRate = NA
data = data %>% mutate(Var_Slope_UIDRate = ifelse(is.na(Var_Slope_UIDRate), exp(LogVar_Slope_UIDRate), Var_Slope_UIDRate))
data = data %>% mutate(Var_Slope_UID = ifelse(is.na(Var_Slope_UID), exp(LogVar_Slope_UID), Var_Slope_UID))
data = data %>% mutate(DH_Sigma = ifelse(is.na(DH_Sigma), exp(DH_LogSigma), DH_Sigma))
data = data %>% mutate(Mean_Slope_DepLength = ifelse(is.na(Mean_Slope_DepLength), exp(Mean_Slope_DepLogLength), Mean_Slope_DepLength))
data = data %>% mutate(Distance_Sigma = ifelse(is.na(Distance_Sigma), exp(Distance_LogSigma), Distance_Sigma))
data = data %>% mutate(Var_Slope_DepLength = ifelse(is.na(Var_Slope_DepLength), exp(LogVar_Slope_DepLogLength), Var_Slope_DepLength))
data = data %>% mutate(Var_Slope_DepLength = ifelse(is.na(Var_Slope_DepLength), exp(LogVar_Slope_DepLength), Var_Slope_DepLength))
data = data %>% mutate(Var_Slope_Surp_POS    = ifelse(is.na(  Var_Slope_Surp_POS  ), log(1+exp(LogExpVar_Slope_Surp_POS     )),     Var_Slope_Surp_POS       ))
data = data %>% mutate(DH_Sigma    = ifelse(is.na(  DH_Sigma  ), log(1+exp(DH_LogExpSigma     )),     DH_Sigma       ))
data = data %>% mutate(Var_Slope_DepLength    = ifelse(is.na(  Var_Slope_DepLength  ), log(1+exp(LogExpVar_Slope_DepLength     )),   Var_Slope_DepLength         ))
data = data %>% mutate(Var_Slope_UID    = ifelse(is.na(  Var_Slope_UID  ), log(1+exp(LogExpVar_Slope_UID     )),    Var_Slope_UID        ))
data = data %>% mutate(Var_Slope_Surp_Word    = ifelse(is.na(   Var_Slope_Surp_Word ), log(1+exp(LogExpVar_Slope_Surp_Word     )),   Var_Slope_Surp_Word         ))
data = data %>% mutate(Var_Slope_UIDRate    = ifelse(is.na(  Var_Slope_UIDRate  ), log(1+exp( LogExpVar_Slope_UIDRate    )),    Var_Slope_UIDRate        ))
data = data %>% mutate(Distance_Sigma    = ifelse(is.na(Distance_Sigma    ), log(1+exp( Distance_LogExpSigma    )),    Distance_Sigma        ))




cat(" TODO do same with other variances and Sigmas")
cat("ALSO take into account depLogLength")
cat("Where is Norwegian surprisal data???")


dodge = position_dodge(.9)


