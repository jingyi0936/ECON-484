library(ISLR)
dir = 'C:/Users/cuijy/Desktop' # the correction location for data
college <- read.csv(file.path(dir, "COllege.csv"))

fix(college)

rownames(college) = college [, 1]
fix(college)

college = college [, -1]
fix(college)


summary(college)
pairs(college[, 1:10])

boxplot(college$Outstate, college$Private, xlab = "Outstate", ylab = "Private",
        main = "Outstate vs Private")

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)

boxplot(college$Outstate, college$Elite, xlab = "Outstate", ylab = "Elite",
        main = "Outstate vs Elite")


par(mfrow = c(2, 3))
hist(college$Apps)
hist(college$Enroll)
hist(college$F.Undergrad)
hist(college$perc.alumni)
hist(college$PhD)
hist(college$Terminal)


plot(college$Top10perc, college$Accept)
plot(college$Top25perc, log(college$Accept))
cor(college$Top25perc, log(college$Accept))
hist(college$Accept)
hist(log(college$Accept))