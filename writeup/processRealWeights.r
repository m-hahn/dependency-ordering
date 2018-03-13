freqs = data.frame()
for(lang in languages) {
   freqs = rbind(freqs, read.csv(paste("CS_SCR/deps/",lang,"-stats.tsv",sep=""), sep="\t") %>% mutate(Language=lang))
}
# TODO this is a hacky way to ensure Norwegian is in the data
freqs$Language[freqs$Language=="Norwegian-Nynorsk"] = "Norwegian"
data = merge(data, freqs %>% select(Head, Dependency, Dependent, Count, Language), by=c("Head", "Dependency", "Dependent", "Language")) #, all=TRUE)
######################################
realWeights = data %>% filter(ObjectiveName == "variational") 
# make sure only one set of real weights is used per languag
byLang = aggregate(realWeights["DH_Mean"], by=c(realWeights["Language"], realWeights["Counter"], realWeights["FileName"]), NROW)
bestCounter = aggregate(byLang["Counter"], by=c(byLang["Language"]), max)
realWeights = merge(realWeights, bestCounter, by=c("Language", "Counter"))
realWeights = realWeights %>% select(Head, Dependency, Dependent, Language, DH_Mean, DH_Sigma, Distance_Mean, Distance_Sigma)%>% rename(Real_DH_Mean = DH_Mean, Real_DH_Sigma = DH_Sigma, Real_Distance_Mean = Distance_Mean, Real_Distance_Sigma = Distance_Sigma)


data = merge(data, realWeights, by=c("Language", "Head", "Dependency", "Dependent"), all.x=TRUE)


countsPerLanguage = freqs %>% group_by(Language) %>% summarise(TotalCountPerLanguage = sum(Count))
percentagePerLanguage = merge(freqs, countsPerLanguage, by=c("Language"))
percentagePerLanguage = percentagePerLanguage %>% mutate(PercentageOfLanguage = Count/TotalCountPerLanguage)
totalCountPerTriple = percentagePerLanguage %>% group_by(Head, Dependency, Dependent) %>% summarise(TotalCount = mean(PercentageOfLanguage))
totalCountPerTriple = as.data.frame(totalCountPerTriple)
#totalCountPerTriple = totalCountPerTriple[order(-totalCountPerTriple$TotalCount),]
data = merge(data, totalCountPerTriple, by=c("Head", "Dependency", "Dependent"), all=TRUE)
percentagePerLanguage = percentagePerLanguage %>% select(Language, Head, Dependency, Dependent, PercentageOfLanguage)
data = merge(data, percentagePerLanguage, by=c("Head", "Dependency", "Dependent", "Language"), all=TRUE)

data$CoarseDependency = sub(":.*", "", data$Dependency)



totalCountPerCoarseDependency = totalCountPerTriple %>% mutate(CoarseDependency = sub(":.*", "", Dependency)) %>% group_by(CoarseDependency) %>% summarise(TotalCount_CoarseDependency = sum(TotalCount))
data = merge(data, totalCountPerCoarseDependency, by=c("CoarseDependency"))
totalCountPerDependent = totalCountPerTriple %>% mutate(Dependent = sub(":.*", "", Dependent)) %>% group_by(Dependent) %>% summarise(TotalCount_Dependent = sum(TotalCount))
data = merge(data, totalCountPerDependent, by=c("Dependent"))
totalCountPerHead = totalCountPerTriple %>% mutate(Head = sub(":.*", "", Head)) %>% group_by(Head) %>% summarise(TotalCount_Head = sum(TotalCount))
data = merge(data, totalCountPerHead, by=c("Head"))





freqs$CoarseDependency = sub(":.*", "", freqs$Dependency)
percentagePerLanguageCoarse = merge(freqs, countsPerLanguage, by=c("Language"))
percentagePerLanguageCoarse = percentagePerLanguageCoarse %>% mutate(PercentageOfLanguage = Count/TotalCountPerLanguage)
totalCountPerTripleCoarse = percentagePerLanguageCoarse %>% group_by(Head, CoarseDependency, Dependent) %>% summarise(TotalCountCoarse = mean(PercentageOfLanguage))
totalCountPerTripleCoarse = as.data.frame(totalCountPerTripleCoarse)
data = merge(data, totalCountPerTripleCoarse, by=c("Head", "CoarseDependency", "Dependent"), all=TRUE)

#freqsCoarseByDep = freqs %>% group_by(Language, CoarseDependency) %>% summarise(Count_CoarseDependency = sum(Count))
#freqsCoarseByDep = as.data.frame(freqsCoarseByDep)




# TODO here, need to make sure that only ONE variational model is taken per language



######################################
data = data %>% mutate(DH = ifelse(is.na(DH_Weight), Direction == "DH", DH_Weight>0))
data = data %>% mutate(Language = as.factor(Language), Head = as.factor(Head), Dependency = as.factor(Dependency), Dependent = as.factor(Dependent))

