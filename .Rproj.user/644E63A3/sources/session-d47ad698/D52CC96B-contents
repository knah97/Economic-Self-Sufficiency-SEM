#load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,       
  psych,      
  lavaan,     
  semPlot,    
  tidyverse   
)

#load Data 
load(here("data", "fsdata.Rdata"))

#standardize variables
zfs<-as.data.frame(scale(na.omit(fsdata[,2:20]),center=TRUE,scale=TRUE))

#estimate EFA with 2 common factors, use oblique rotation
fact5obl <- fa(zfs, n.obs=4829, nfactors=5, rotate="oblimin",fm="ml")
fact5obl

#residual correlation
resid<-cor(zfs)-
  (fact5obl$loadings%*%fact5obl$Phi%*%t(fact5obl$loadings)+
     diag(fact5obl$uniquenesses))

#compute number of residual correlations with absolute value larger than 0.05 below the diagonal
n<-sum(ifelse(abs(resid)>0.05,1,0))/2

#compute proportion of residual correlations with absolute value larger than 0.05 below the diagonal
print(n/(19*18/2))

#proportion of non-redundant residuals 
sum(ifelse(abs(fact5obl$residual-diag(diag(fact5obl$residual)))>0.05,1,0))/(19*18)


#Covariance matrix
covmat<-cov(fsdata[,2:20])
View(covmat)

#compute centered data
cfs <- fsdata
cfs[,2:20] <- scale(fsdata[,2:20], center = TRUE, scale = FALSE)

#measurement model latent variables
cfa1<-'financial_situation=~1*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money         
       financial_situation_family=~1*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
       lack_of_support_finding_job=~1*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
       importance_job_for_self_development=~1*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+SDJ_meet_people+SDJ_feeling_self_worth
       bad_health=~1*HEALTH_felt_down+HEALTH_limitation
       '
#fit model on covariance matrix
fitcfa1<-cfa(cfa1,cfs)

#print fitmeasures
fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#summary of results
summary(fitcfa1,fit.measures=TRUE)

#print standardized solution
standardizedSolution(fitcfa1)
d<-standardizedSolution(fitcfa1)

#reliability factor scores
#function composite reliability
compositerel<-function(x){
  A<-(sum(x))^2
  B<-sum(1-x^2)
  return(A/(A+B))
}

#composite reliability financial situation
compositerel(d[1:4,4])
#composite reliability financial situation family
compositerel(d[5:8,4])
#composite reliability lack of support finding job
compositerel(d[9:12,4])
#composite reliability importance job for self development
compositerel(d[13:17,4])
#composite reliability bad health
compositerel(d[18:19,4])

#average variance extracted
average_var_extracted<-round(c(mean(d[1:4,4]^2),
                               mean(d[5:8,4]^2),
                               mean(d[9:12,4]^2),
                               mean(d[13:17,4]^2),
                               mean(d[18:19,4]^2)), 3)

#maximum shared variance of latent variable with other latent variables
max_shared_var<-round(c(max(d[c(44:47),4]^2), #FS
                        max(d[c(44,48,49,50),4]^2), #FSF
                        max(d[c(45,48,51,52),4]^2), #SFJ
                        max(d[c(46,49,51,53),4]^2), #SDJ
                        max(d[c(47,50,52,53),4]^2)), 3) #HEALTH

#overview table composite reliability, average variance extracted and maximum shared variance
factorscore<-c("financial situation","financial situation family","lack of support finding job","importance job for self development", "bad health")
reliability<-round(c(compositerel(d[1:4,4]),compositerel(d[5:8,4]),compositerel(d[9:12,4]),compositerel(d[13:17,4]),compositerel(d[18:19,4])),3)
data.frame(factorscore,reliability, average_var_extracted, max_shared_var)



#modification indices
mod1<-modificationIndices(fitcfa1)
mod1[order(mod1[,4],decreasing=TRUE),][1:10,]

#adapted model: add FSF_afford_extras~~FSF_save_money
cfa2<-'financial_situation=~1*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money         
       financial_situation_family=~1*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
       lack_of_support_finding_job=~1*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
       importance_job_for_self_development=~1*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+SDJ_meet_people+SDJ_feeling_self_worth
       bad_health=~1*HEALTH_felt_down+HEALTH_limitation
       FSF_afford_extras ~~ FSF_save_money
       '

#fit model on covariance matrix
fitcfa2<-cfa(cfa2,cfs)

#summary of results
summary(fitcfa2,fit.measures=TRUE)

#print fitmeasures
fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#print standardized solution
standardizedSolution(fitcfa2)

#check modification indices of the adapted model
modificationindices(fitcfa2, sort. = TRUE)

#second adapted model: add SDJ_help_people~~SDJ_meet_people
cfa3<-'financial_situation=~1*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money         
       financial_situation_family=~1*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
       lack_of_support_finding_job=~1*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
       importance_job_for_self_development=~1*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+SDJ_meet_people+SDJ_feeling_self_worth
       bad_health=~1*HEALTH_felt_down+HEALTH_limitation
       FSF_afford_extras ~~ FSF_save_money
       SDJ_help_people ~~  SDJ_meet_people'

#fit model on covariance matrix
fitcfa3<-cfa(cfa3,cfs)

#summary of results
summary(fitcfa3,fit.measures=TRUE)

#print fitmeasures
fitmeasures(fitcfa3,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#print standardized solution
standardizedSolution(fitcfa3)



#analysis is conducted on improved model cfa3
#structural equation model to investigate how the latent variable “financial situation” can be explained by the other 4 latent variables

sem1 <- ' 
  # latent variable definitions
        financial_situation =~ 1*FS_pay_bills + FS_afford_extras + FS_afford_housing + FS_save_money       
        financial_situation_family =~ 1*FSF_pay_bills + FSF_afford_extras + FSF_afford_housing + FSF_save_money
        lack_of_support_finding_job =~ 1*SFJ_no_info + SFJ_no_chance_show + SFJ_no_training + SFJ_no_support_findjob
        importance_job_for_self_development =~ 1*SDJ_help_people + SDJ_learn_new_things + SDJ_develop_creativity + SDJ_meet_people + SDJ_feeling_self_worth
        bad_health =~ 1*HEALTH_felt_down + HEALTH_limitation
    
    
  # regressions
    FSF_afford_extras ~~ FSF_save_money
    SDJ_help_people ~~  SDJ_meet_people
    financial_situation ~ financial_situation_family + 
    lack_of_support_finding_job + 
    importance_job_for_self_development + 
    bad_health     
   '

#fit multi-group models to investigate measurement invariance
#1) a configural measurement invariance model with country-specific regression coefficients in the regression equation of the structural model
config_1<-sem(sem1, data=cfs, group = "country")
#2) a configural measurement invariance model with regression coefficients that are constrained to be equal across countries
config_2<-sem(sem1, data=cfs, group="country", group.equal="regressions")
#3) a metric measurement invariance model with country-specific regression coefficients in the regression equation of the structural model
metric_1<-sem(sem1, data=cfs, group="country", group.equal="loadings")
#4) a metric measurement invariance model with regression coefficients that are constrained to be equal across countries
metric_2<-sem(sem1, data=cfs, group="country", group.equal=c("loadings","regressions"))

#summarize fit measures
fitconfig_1<-fitmeasures(config_1, c("chisq","df","cfi","tli","rmsea","srmr"))
fitconfig_2<-fitmeasures(config_2, c("chisq","df","cfi","tli","rmsea","srmr"))
fitmetric_1<-fitmeasures(metric_1, c("chisq","df","cfi","tli","rmsea","srmr"))
fitmetric_2<-fitmeasures(metric_2, c("chisq","df","cfi","tli","rmsea","srmr"))

fit1<-rbind(fitconfig_1,fitconfig_2,fitmetric_1,fitmetric_2)
rownames(fit1)<-c("configural 1","configural 2","metric 1","metric 2")
round(fit1,3)

#compare models using LR test
anova(config_1, config_2) 
anova(metric_1, metric_2)  

#summary of results metric_2
summary(metric_2,fit.measures=TRUE,std=TRUE)

# ==============================================================================
# Visualization: Structural Equation Model (SEM) Path Diagram
# ==============================================================================

# 1. Define Node Labels
# Order: Manifest variables (Squares) first, followed by Latent variables (Circles)
node_labels <- c(
  # Manifest Variables (19 items)
  "FS1", "FS2", "FS3", "FS4",             
  "Fam1", "Fam2", "Fam3", "Fam4",         
  "Sup1", "Sup2", "Sup3", "Sup4",         
  "Dev1", "Dev2", "Dev3", "Dev4", "Dev5", 
  "Hlth1", "Hlth2",                       
  
  # Latent Variables (5 factors)
  "Fin\nSit", "Fam\nFin", "No\nSupp", "Self\nDev", "Bad\nHealth"
)

# 2. Define Color Palette for Latent Factors
group_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")

# 3. Export High-Resolution Plot
# Width set to 15 to ensure sufficient space for the legend and labels
png(filename = here("output", "final_sem_horizontal_v3.png"), 
    width = 15, height = 18, units = "in", res = 300) 

# Graphical Parameters:
# xpd = TRUE allows drawing outside the plot region to prevent label clipping
par(mar = c(6, 6, 6, 6), xpd = TRUE)

semPaths(fitcfa3, 
         whatLabels = "std",       # Display standardized estimates
         layout = "tree2",         # Tree layout structure
         rotation = 2,             # Rotate to horizontal layout (Latents on left)
         
         # --- Layout Adjustments ---
         # Manually expand x-axis limits (padding) to accommodate 
         # correlation coefficients on the left side
         xlim = c(-1.8, 1.2),
         
         # Reduce curvature of rotation lines for cleaner look
         curve = 1.5,              
         
         # --- Label Styling ---
         nodeLabels = node_labels, 
         label.cex = 1.2,           
         
         # --- Shape & Color Styling ---
         sizeMan = 4,              # Width of manifest squares 
         sizeMan2 = 2,             # Height of manifest squares
         sizeLat = 7,              # Size of latent circles
         groups = "latents",       # Group colors by latent factors
         color = group_colors,     
         borders = FALSE,          
         
         # --- Edge & Text Styling ---
         edge.label.cex = 0.9,     # Font size for path coefficients
         edge.color = "darkgray",  
         fade = FALSE,             
         
         # --- Simplification ---
         optimizeLatRes = TRUE,     
         residuals = FALSE,        # Hide residual variances to reduce clutter
         nCharNodes = 0)            

# Add Title
title("Drivers of Economic Self-Sufficiency: Structural Model", 
      adj = 0.5, line = 2, cex.main = 1.5)

# Add Legend
legend("bottomright", 
       legend = c("FinSit: Current Financial Situation", 
                  "FamFin: Family Background (Age 14)", 
                  "NoSupp: Lack of Job Support", 
                  "SelfDev: Job for Self-Development", 
                  "BadHealth: Health Limitations"),
       fill = group_colors, 
       bty = "n",       # No border
       cex = 1.0,       
       
       # Inset legend slightly to ensure it stays within the canvas
       inset = c(0.05, 0)) 

dev.off()













