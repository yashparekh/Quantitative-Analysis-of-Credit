In the spreadsheet Credit_Dataset.csv, you will find data pertaining to 1000 personal loan accounts at a bank. The Excel spreadsheet Credit_Dictionary.xlsx contains a description of what the various variables mean.

When a new applicant applies for credit, as a part of the application, the company collects information which is available in the form of Variables 2 to 21. The company then decides an amount to be credited (the variable CREDIT_EXTENDED.) For these 1000 accounts, we also have information on how profitable each account turned out to be (variable PROFIT). A negative value indicates a net loss. This typically happens when the debtor defaults on his/her payments.

The goal in this case is to investigate how one can use this data to better manage the bank's credit extension program. Specifically, our goal is to develop logistic regression, trees, and knn classification models and find the best model to classify a new account as “profitable” or “not profitable”.

After developing the three models and comparing their performance we observe that, Logistic regression outperforms both kNN and Regression Trees with highest test accuracy

