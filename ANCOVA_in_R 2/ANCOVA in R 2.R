#Load Data

plotNormalHistogram(graduate_admissions$CGPA)

#Looks approximately normal, but could use a square transformation. 

graduate_admissions$CGPAsq <- graduate_admissions$CGPA * graduate_admissions$CGPA
plotNormalHistogram(graduate_admissions$CGPAsq)


plotNormalHistogram(graduate_admissions$TOEFL.Score)

#Square Transformation

graduate_admissions$TOEFL.ScoreSQ <- graduate_admissions$TOEFL.Score * graduate_admissions$TOEFL.Score
plotNormalHistogram(graduate_admissions$TOEFL.ScoreSQ)


#Homogeneity of Variance

leveneTest(CGPAsq~University.Rating, data=graduate_admissions)

#Results were not significant, and the assumption is met.

#Homogeneity of Regression Slopes

Homogeneity_RegrSlp = lm(CGPA~TOEFL.Score, data=graduate_admissions)
anova(Homogeneity_RegrSlp)

#Homogeneity isn't met

#Sample size met. I have 2, so need at least 40 and there are 400 cases.

#Run the Analysis

ANCOVA = lm(CGPA~TOEFL.Score + University.Rating*TOEFL.Score, data=graduate_admissions)
anova(ANCOVA)
#Significant interaction between TOEFL score and University Rating.

#Post Hocs
postHocs <- glht(ANCOVA,linfct=mcp(University.Rating = "Tukey"))
summary(postHocs)

# After examining the post hocs, it looks like a Type I error. No group significantly differs from any other group.

#Examine Adjusted Means

adjMeans <- effect("University.Rating", ANCOVA)
adjMeans

#By the look of the means all have a college GPA that is about the same.