# Classification_modeling
A hospital readmission is an episode when a patient who had been discharged from a hospital is admitted
again within a specified time interval. Readmission rates have increasingly been used as an outcome
measure in health services research and as a quality benchmark for health systems. Hospital readmission
rates were formally included in reimbursement decisions for the Centers for Medicare and Medicaid
Services (CMS) as part of the Patient Protection and Affordable Care Act (ACA) of 2010, which penalizes
health systems with higher than expected readmission rates through the Hospital Readmission Reduction
Program.
The data in this problem is real-world data covering 10 years (19992008) of clinical care at 130 hospitals
and integrated delivery networks throughout the United States. The specific patient cases used in this
challenge also meet the following criteria:
• it is generated from a hospital admission with length of stay between 1 and 14 days
• diabetes was entered to the system as one of the diagnoses
• laboratory tests were performed during the encounter
• medications were administered during the encounter
In this machine learning problem, the exacting task is trying to predict whether or not a
patient will be readmitted to the hospital. The target here takes on binary values where 0 implies that
the patient was not readmitted, and 1 implies that the patient was readmitted. The latter is being predicted. 
The classification model will be evaluated using log loss. The predictions, will be probabilities
of readmission.

Approach 
Built at least 5 different classes of classifiers from the following list: linear discriminant
analysis, logistic regression, penalized logistic regression (e.g., using glmnet), MARS (for classifica-
tion), decision tree, random forest, bagged trees, boosted trees, SVM, neural nets. Hyper-parameters
was tuned using a re-sampling method. 

Solution
- Since the data has many features, a process has to be made to reduce the number of features in the model and pick only the important features. I utilized the Boruta method to select the best features using only the numerical features.
- From the features whcih are selected above, the model features are selected. The missingness of the features are checked. Race was the only feature with some missisng data. 
- Logitsic Regression with lasso regularization with optimization of lambda in the model. Alpha was set to ome to regularize the model and penelize for less important variables.
- Random forest model with optimization of the number of trees.
- Decision tree run on the data with the cross validation.
- The svm is run on the data and radial kernel is used and the value fo c was optimized in the predictive modelling.
- Classification Metrics was used analyze the model;
-   roc(model)
    auc(model)
    acc(model)
    precvsrec(model)
    aucpr(model)
    lift(model)
    prbe(model)
    #print(Concordance(model$y, model$fitted.values))
    matt(model)
    print(F1_Score(data$pred,data$true))
    print(ConfusionDF(data$pred, data$true))
    print(LogLoss(y_pred = model$fitted.Values,y_true=data$pred))
    print(LiftAUC(data$pred, data$true))
    print(GainAUC(data$pred, data$true))
    dstatistic (data,model)
    chart(data,model)
- The model that gave the lowest logloss was the logistic regression with a logloss of 0.65287. I am still working on this models to get a lower log loss.
