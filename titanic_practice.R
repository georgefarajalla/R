#install.packages("randomForest")
#install.packages("reprtree")
library("randomForest")
library("data.table")
library("rpart")


dfTitanic <- data.frame(Titanic)
dtTitanic <- data.table(Titanic)

nTot <- dtTitanic[, sum(N)]
nTot

dtSurvived <- dtTitanic[, list(n = sum(N)), by = "Survived"]
dtSurvived

dtLong <- dtTitanic[, list(Freq = rep(1,N)), by = c("Survived", "Sex", "Age", "Class")]
dtLong

dtLong[, Freq := NULL]
dtLong[, Survived := ifelse(Survived == "Yes",1 ,0)] ## change yes/no to 1/0

formulaRpart <- formula("Survived ~ Sex + Age + Class")
treeRegr <- rpart(formula = formulaRpart, data = dtLong, method = "class")
prp(treeRegr)


dtDummy <- copy(dtLong)
dtDummy

dtDummy[, Male := Sex == "Male"]
dtDummy[, Sex := NULL]

dtDummy[, Child := Age == "Child"]
dtDummy[, Age := NULL]

dtDummy[, Class1 := Class == "1st"]
dtDummy[, Class2 := Class == "2nd"]
dtDummy[, Class3 := Class == "3rd"]
dtDummy[, Class := NULL]

formulaRf <- formula("Survived ~ Male + Child + Class1 + Class2 + Class3")
forest <- randomForest(formula = formulaRf, data = dtDummy)

forest$ntree
forest$mtry
forest$type

forest <- randomForest(formula = formulaRf, data = dtDummy, ntree = 1000, mtry = 3, sampsize = 1500)

rowRandom <- dtDummy[100]
rowRandom

predict(forest, rowRandom)

prediction = predict(forest, dtDummy)
sample(prediction, 6)

dtDummy[, SurvivalRatePred := predict(forest, dtDummy)] ## survival rate prediction

dtDummy[, SurvivedPred := ifelse(SurvivalRatePred > 0.5, 1, 0)] ## set threshold

dtDummy[, error := SurvivedPred != Survived]

percError <- dtDummy[, sum(error)/ .N]
percError


indexTrain <- sample(x = c(TRUE,FALSE), size = nrow(dtDummy), replace = TRUE, prob = c(0.8, 0.2))
indexTrain

dtTrain <- dtDummy[indexTrain]
dtTest <- dtDummy[!indexTrain]

forest <- randomForest(formula = formulaRf, data = dtTrain, ntree = 1000, mtry = 3, sampsize = 1200)

dtTest[, SurvivalRatePred := predict(forest, dtTest)]
dtTest[, SurvivedPred := ifelse(SurvivalRatePred > 0.5, 1, 0)]
dtTest[, error := SurvivedPred != Survived]
percError <- dtTest[, sum(error)/.N]
percError



