# Wine Sale Regression
 a Zero-Inflated Poisson Model is built to predict wine sale

The purpose of this project was to predict cases of vine a company sell based on over 20 relevant features such as acidity, label, and pH values. Raw data was cleaned by removing features with over 10% missing values and rows with over two features missing. The rest missing values were imputed with medians.
Several Models were created and compared, including multiple regression model (Automated variable selection using StepAIC), a possion distribution model, and the final model is a Zero-Inflated Poisson Model containing 9 explanatory variables. The mean absolute error of the model is 1.3 cases when fitted to the validation dataset.
