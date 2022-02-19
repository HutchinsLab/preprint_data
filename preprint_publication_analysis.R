library(data.table)
library(exactRankTests)
library(lmerTest)
library(networkD3)
library(psych)
geomean = function(x) exp(mean(log(x)[!is.infinite(log(x))], na.rm=T))

data = read.csv("data.csv", stringsAsFactors = F)

# Number of matched preprints, data points, and stats
nmatches = length(unique(paste(data$id, data$cat)))
# nmatches 100
ndpoints = sum(!is.na(data$point.estimate) | !is.na(data$point.estimate.1))
# ndpoints 1921
avgpoints = ndpoints / nmatches
# avgpoints 19.21

# Number of points in preprints
pointspre = sum(!is.na(data$point.estimate))
# pointspre 1606
pointssurvived = sum(!is.na(data$point.estimate.1[!is.na(data$point.estimate)]))
# pointssurvived 1433
avgpointssurvived = pointssurvived / pointspre
# avgpointssurvived 0.89
1- avgpointssurvived
# 1- avgpointssurvived 0.108
pointspost = sum(!is.na(data$point.estimate.1))
# pointspost 1748
pointscarried = sum(!is.na(data$point.estimate[!is.na(data$point.estimate.1)]))
# pointscarried 1433
avgpointscarried = pointscarried / pointspost
1 - avgpointscarried
# 1 - avgpointscarried 0.1802059


# Ratio
ident = mean(data$ratio == 1, na.rm=T)
# ident 0.682
bigger = mean(data$ratio > 1, na.rm=T)
# bigger 0.177
smaller = mean(data$ratio < 1, na.rm=T)
# smaller 0.141
similar = mean(data$ratio <= 1.05  & data$ratio >= 0.95 , na.rm=T)
similar10 = mean(data$ratio <= 1.1  & data$ratio >= 0.9 , na.rm=T)
# similar 0.82
# similar10 0.86
changed_small_denom = mean(data$point.estimate[!(data$ratio <= 1.1  & data$ratio >= 0.9)] < 1, na.rm=T)
# changed_small_denom 0.653
magnitude = geomean(abs(1-data$ratio) + 1)
# magnitude 1.06
wilcox.exact(log(data$ratio), mu=0)
# V = 49851, p-value = 0.6752
wilcox.exact(log(data$ratio), mu=0, alternative = 'greater')
# V = 49851, p-value = 0.3376
wilcox.exact(log(data$ratio), mu=0, alternative = 'less')
# V = 49851, p-value = 0.6624
summary(lmer(log(ratio) ~ 1|id, data = na.omit(data[data$ratio != 0,c("ratio", "id")]) ))
#Fixed effects:
#Estimate Std. Error        df t value Pr(>|t|)
#(Intercept) 9.207e-03  1.424e-02 1.010e+02   0.647    0.519
wilcox.exact(log(data$ratio[data$cat == "CFR"]), mu=0)
# V = 11825, p-value = 0.8139
wilcox.exact(log(data$ratio[data$cat == "IFR"]), mu=0)
# V = 964, p-value = 0.1757
wilcox.exact(log(data$ratio[data$cat == "Inc"]), mu=0)
# V = 133, p-value = 0.1208
wilcox.exact(log(data$ratio[data$cat == "R0"]), mu=0)
# V = 5096, p-value = 0.233

# Dropped
dropped = tapply(is.na(data$point.estimate.1[!is.na(data$point.estimate)]),
                 paste(data$id, data$cat)[!is.na(data$point.estimate)], mean)
dropped_pct = mean(dropped > 0)
# dropped_pct 0.1616162
dropped_sum = tapply(is.na(data$point.estimate.1[!is.na(data$point.estimate)]),
                 paste(data$id, data$cat)[!is.na(data$point.estimate)], sum)
mean(dropped_sum)
# mean(dropped_sum) 1.747
dropped_n = tapply(is.na(data$point.estimate.1[!is.na(data$point.estimate)]),
                   paste(data$id, data$cat)[!is.na(data$point.estimate)], length)
sum(dropped_sum[dropped_sum > 0]) / sum(dropped_n[dropped_sum > 0])
# sum(dropped_sum[dropped_sum > 0]) / sum(dropped_n[dropped_sum > 0]) 0.4061

# Added
added = tapply(is.na(data$point.estimate[!is.na(data$point.estimate.1)]),
               paste(data$id, data$cat)[!is.na(data$point.estimate.1)], mean)
added_pct = mean(added > 0)
# added_pct 0.31313
added_sum = tapply(is.na(data$point.estimate[!is.na(data$point.estimate.1)]),
               paste(data$id, data$cat)[!is.na(data$point.estimate.1)], sum)
mean(added_sum[added_sum > 0])
# mean(added_sum[added_sum > 0]) 10.161

unique(paste(data$id, data$cat))

length(intersect(names(added), names(dropped)))
# 98

sum(dropped > 0)
# 16

sum(added > 0)
# 31

length(intersect(names(added)[added > 0], names(dropped)[dropped > 0]))
# 11

# Sankey diagram
df = data.frame(rbind(c(0,2,pointspre-pointssurvived),
           c(0,3,pointssurvived),
           c(1,3,pointspost - pointscarried)))
colnames(df) = c("source", "target", "value")
nms = data.frame("Names"=c(" ", "  ", "   ", "    "))
sankeyNetwork(df, nms, "source", "target", "value", "Names", fontSize = 24, height=513, width=360, nodeWidth = 40)

ci_range_pre = abs(data$confidence.upper - data$confidence.lower)
ci_range_pub = (data$confidence.upper.1 - data$confidence.lower.1)
ci_ratio = ci_range_pub / ci_range_pre
ci_ratio[is.infinite(ci_ratio)] = NA
wilcox.exact(log(ci_ratio), mu=0)
# p-value = 3.055e-06
avg_ci_ratio = geomean(ci_ratio)
# avg_ci_ratio 0.9260
summary(!is.na(ci_ratio[!is.na(data$point.estimate)]))
# has data in preprint and has usable confidence intervals 495

# Peer review
pr = read.csv("rapidreviews.csv",header=T)
pr$Published = pr$Published != ""
pr = pr[,c("Avg", "Field", "Elapsed", "Published")]

# Small model
summary(glm(Published ~ Avg , data=pr, family = binomial))
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)
#(Intercept)  -0.1641     1.0639  -0.154    0.877
#Avg          -0.0485     0.3003  -0.162    0.872

# Age plus peer review scores
summary(glm(Published ~ Avg + Elapsed, data=pr, family = binomial))
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)
#(Intercept) -2.204005   3.048977  -0.723    0.470
#Avg         -0.052036   0.300947  -0.173    0.863
#Elapsed      0.004919   0.006888   0.714    0.475

# Field plus peer review scores
model = glm(Published ~ Avg + Field, data=pr, family = binomial)
summary(model)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)     -0.8554     1.1704  -0.731    0.465  
#Avg             -0.0371     0.3166  -0.117    0.907  
#FieldMed         0.5517     0.6180   0.893    0.372  
#FieldPubHealth   1.5868     0.6977   2.274    0.023 *
newdata <- data.frame(Avg=rep(seq(min(pr$Avg), max(pr$Avg),len=100),3))
newdata$Field = c(rep("Biol", 100), rep("Med", 100), rep("PubHealth", 100))
newdata$Published = predict(model, newdata, type="response")
#write.csv(newdata, "logistic_fit.csv", row.names=F, na="")

# Loggreg
summary(glm(Published ~ Avg + Field + Elapsed, data=pr, family = binomial))
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)    -2.542768   3.263674  -0.779   0.4359  
#Avg            -0.044601   0.317917  -0.140   0.8884  
#FieldMed        0.598995   0.625883   0.957   0.3385  
#FieldPubHealth  1.573246   0.699600   2.249   0.0245 *
#Elapsed         0.004069   0.007338   0.555   0.5792 

# linear rel peer rev and pub prob
fract_published_score = tapply(pr$Published, pr$Avg, mean)
summary(lm(fract_published_score ~ I(as.numeric(names(fract_published_score)))))

# phi score
phi(matrix(c(11,20,5,64),nrow =2))
# phi 0.36
