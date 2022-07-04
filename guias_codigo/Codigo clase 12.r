
# Recuerden configurar el directorio de trabajo donde tiene los archivos

prepostExpWide <- read.csv("prepostExperimentWide.csv",
stringsAsFactors = TRUE)

prepostExpWide

prepostExpLong <- read.csv("prepostExperimentLong.csv",
stringsAsFactors = TRUE)

str(prepostExpLong)

contrasts(prepostExpLong$treatment)

summary(lm(post ~ treatment, data = prepostExpWide))

summary(lm(post ~ pre + treatment, data = prepostExpWide))

summary(lm(post ~ pre*treatment, data = prepostExpWide))

summary(lm(change ~ treatment, data = prepostExpWide))

t.test(prepostExpWide$change ~ as.numeric(prepostExpWide$treatment))

prepostExpLong

contrasts(prepostExpLong$treatment) <- contr.treatment(2, base = 2)
contrasts(prepostExpLong$time)      <- contr.treatment(2, base = 2)

contrasts(prepostExpLong$treatment)
contrasts(prepostExpLong$time)     

summary(aov(sociability ~ treatment, data = prepostExpLong))
summary(lm(sociability ~ treatment, data = prepostExpLong))

summary(aov(sociability ~ time, data = prepostExpLong))
summary(lm(sociability ~ time, data = prepostExpLong))

summary(aov(sociability ~ time + treatment, data = prepostExpLong))
summary(lm(sociability ~ time + treatment, data = prepostExpLong))

summary(aov(sociability ~ time*treatment, data = prepostExpLong))
summary(lm(sociability ~ time*treatment, data = prepostExpLong))


summary(aov(sociability ~ time*treatment + Error(id) , data = prepostExpLong))

library(lme4)
summary(lmer(sociability ~ time*treatment + (1|id) , data = prepostExpLong))
summary(lm(sociability ~ time*treatment  , data = prepostExpLong))



interaction.plot(
    x.factor = prepostExpLong$time, 
    trace.factor = prepostExpLong$treatment, 
    response = prepostExpLong$sociability, 
    fun = mean, 
    xlab = "time",
    ylab = "sociability",   
    trace.label = "treatment")


