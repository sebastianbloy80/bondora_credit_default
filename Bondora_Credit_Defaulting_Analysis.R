##########################################################################################
###################### Bondora credit defaulting risk analysis ########################### START
##########################################################################################

# Load Packages # 
pack <- c("pscl", "readr", "dplyr","DMwR", "zoo", "corrplot", "car", "caret", "MASS", "reshape")

if(length(setdiff(pack, rownames(installed.packages())))>0) {
  install.packages(setdiff(pack, rownames(installed.packages())))
}

lapply(pack, require, character.only = TRUE)

# temporarily change language to english

Sys.setlocale("LC_ALL","English")

# Load raw Data
path <- "path to the csv-file" # Enter the path + the filename + fileextension. Example "C:/Users/Name/raw_data/LoanData.csv"
LoanData <- read_csv(path)

# Set Seed
set.seed(15)

# Data overview- correct data format?
orig_vars <- data.frame("Data_Type" = unlist(sapply(LoanData, class)))

orig_vars$Colnames <- rownames(orig_vars) 

# Barplot of imported Data Types - Vartype
barplot(table(orig_vars$Data_Type), main = "Original Data Types of data set LoanData as imported", col = "#0080FF") 

# Analysis of Relevant Independent Variables - a priori selection
dep_vars <- c("DefaultDate")
dep_vars_calc  <- c("Defaulted")

indep_vars <- c("NewCreditCustomer", "LanguageCode", "Age", "Gender", "Country", "AppliedAmount", "FreeCash", "DebtToIncome",
                "Amount", "Interest", "LoanDuration", "MonthlyPayment", "Education", "EmploymentDurationCurrentEmployer", "HomeOwnershipType",
                "IncomeTotal", "ExistingLiabilities", "LiabilitiesTotal", "RefinanceLiabilities", "NoOfPreviousLoansBeforeLoan", "AmountOfPreviousLoansBeforeLoan",
                "PreviousRepaymentsBeforeLoan", "PreviousEarlyRepaymentsBefoleLoan", "PreviousEarlyRepaymentsCountBeforeLoan")

other_vars <- c("LoanId", "ProbabilityOfDefault")

# Feature calculation
# Feature Defaulted or Not Defaulted: A credit is defined as defaulted when higher than "31-60" 
LoanData$Defaulted <- ifelse(LoanData$WorseLateCategory %in% c("61-90", "91-120", "121-150","15-180" ,"180+"), 1, 0) # Defaulted = 1

# How much was applied for, how much was agrred by the lender
LoanData$NeedvsHave <- ifelse(LoanData$AppliedAmount - LoanData$Amount < 0 , 0, LoanData$AppliedAmount - LoanData$Amount)
indep_vars <- c(indep_vars, "NeedvsHave")

# What is the history of defaulted credits per Username - cumulative by date and if Defaulted is true!
# The first credit even when defaulted is always zero as it was not known from data at the time that it would default
# Calculate Days since last debt taken by individual UserName - if empty - first debt - after that the # of days after the consecutive credit date is counted (values from 0 to max difference are possible)
LoanData <- LoanData %>%
  group_by(UserName) %>%
  arrange(LoanDate) %>% # order by date so difference is taken date bay consecutive date
  mutate(NumCredits = 1:n(),
         NumDefaults = cumsum(Defaulted),
         QuotDefaults = (cumsum(Defaulted) / 1:n()),
         DaysLastCred = c(0, round(diff.difftime(LoanDate, units = 'days'))))

indep_vars <- c(indep_vars, "QuotDefaults", "DaysLastCred")

# Barplot of dependet and independent Vars- Vartype
barplot(table(orig_vars[c(indep_vars,dep_vars),1]), main = "Data Types of data set LoanData - Relevant Independent as imported", col = "#0080FF") 

# Only Analysis vars
analysis_var <- orig_vars[orig_vars$Colnames %in% c(indep_vars, dep_vars),]

# Filter Analysis Data Frame
# After Eyeballing Data to find erros
LoanData$MMYY_credit <- as.yearmon(LoanData$LoanDate, "%m-%Y")

sense_test <- LoanData %>%
  group_by(MMYY_credit) %>%
  summarise(Correlation_BidsManual_Amount = cor(BidsManual, Amount),
            Mean_BidsManual = mean(BidsManual),
            Mean_Amount = mean(Amount))

plot(sense_test$MMYY_credit, sense_test$Correlation_BidsManual_Amount, type = "l", 
     main = "Correlation Coefficient of BidsManual to Amount", 
     xlab = "MMYY_credit",
     ylab = "Mean Correlation coefficient [pearson] per MMYY_credit category",col = "blue") # Unusualy perfect correlation between Bids_Manual and Amount - seems to be a database extraction error until 2013

# Cutting of very early data from 2009 till August 2012 as a lot of data seems missing or wrong there

# Create Analysis data frame
LoanAna <- LoanData[which(LoanData$MaturityDate_Last < LoanData$ReportAsOfEOD &
                            LoanData$MMYY_credit > "Sep 2012" &
                            LoanData$Gender != "2"),] # Only take finished credits

cat("Original dimensions Loan Analysis:", dim(LoanAna), "\n")

# Descriptive Stats
descr_LoanAna <- do.call(cbind, lapply(LoanAna[ ,which(colnames(LoanAna) %in% c(dep_vars, indep_vars))], summary))

# Recode relevant Vars
# We have to force Homeownershiptype as on import it is not imported as Factor but as numeric
for (i in indep_vars){
  if (is.character(LoanAna[[i]]) | i %in% c("LanguageCode","Education", "Gender","HomeOwnershipType")){
    cat("Changing Variable:", i, "to factor", "\n")
    LoanAna[i] <- as.factor(LoanAna[[i]])
  } else {
    next
  }
} 

# Data overview- correct data format?
orig_vars_Ana <- data.frame("Data_Type" = unlist(sapply(LoanAna, class)))

orig_vars_Ana$Colnames <- rownames(orig_vars_Ana) 

# Re-check Only Analysis vars
analysis_var_Ana <- orig_vars_Ana[orig_vars_Ana$Colnames %in% c(indep_vars, dep_vars),]

# Descriptive Stats
descr_LoanAna <- do.call(cbind, lapply(LoanAna[ ,which(colnames(LoanAna) %in% c(dep_vars_calc, indep_vars))], summary))

# Outlier Detection
# Eyeballing outliers and missing values

par(mfrow=c(2,2))

for (i in indep_vars){
  if (is.numeric(LoanAna[[i]])){
    form <- as.formula(paste0(i, "~ MMYY_credit"))
    boxplot(LoanAna[i], main= paste(i , "[not cleaned yet]", sep = " "))
    boxplot(form, data=LoanAna, main= paste(i, "vs MM_credit [not cleaned yet]", sep = " "))
  } else {
    next
  }
}

# Discontinued Data DebtToIncome, FreeCash, Refinance Liabilities
indep_vars <- indep_vars[-which(indep_vars %in% c("DebtToIncome","FreeCash", "RefinanceLiabilities", "PreviousEarlyRepaymentsBefoleLoan"))]

# Find NAs and possibly replace them
sumNA <- sapply(LoanAna[indep_vars], function(x) sum(is.na(x)))
# There are a lot of missing values in PreviousRepaymentsBeforeLoan, PreviousEarlyRepaymentsBefoleLoan and MonthlyPayment

# Calculate the number of Factor-categories and delete low categories and drop emtpy levels
# This avoids the error of having factorial categories unknown in the testdata when splitting train and testdata
FactToLow <- apply(LoanAna[which(c(sapply(LoanAna, is.factor)))],2, function(x) {table(x) < 50})

for (i in indep_vars){
  if (is.logical(FactToLow)){
    print("No Factor categories to delete - breaking loop")
    break
  } else if (!any(FactToLow[[i]] %in% T)) {
    next
  } else {
    del <- names(FactToLow[[i]][(which(FactToLow[[i]] %in% T))])
    cat("Delete low population factor categories in:", i, "\n")
    LoanAna <- LoanAna[-which(LoanAna[[i]] %in% del),]
  }
  LoanAna[indep_vars] <- droplevels(LoanAna[indep_vars])
}

# DebtToIncome somethings fishy - somewhere in June 2017, the DebtToIncome is falling to zero without changing! REMOVE
# RefinanceLiabilities somethings fishy - stops around end of 2017 REMOVE
# FreeCash somethings fishy - stops around end of 2017 REMOVE
# Discontinued Data DebtToIncome, FreeCash

# MonthlyPayment fishy - approx middle 2014 only zero or NA
LoanAna <- subset(LoanAna, LoanAna$MonthlyPayment > 0)

# Correlation Matrix
cor_tbl <- cor(LoanAna[which(c(sapply(LoanAna, is.numeric)) & colnames(LoanAna) %in% indep_vars)], use = "complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

par(mfrow=c(1,1))

corrplot(cor_tbl, method="number",  col = col(200),
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         main = "Correlation Table with all independent Vars"
)

# Remove variables with high correlation
# Delete highly correlated vars
indep_vars <- indep_vars[-which(indep_vars %in% c("AppliedAmount", "NoOfPreviousLoansBeforeLoan", "PreviousRepaymentsBeforeLoan", "ExistingLiabilities", "MonthlyPayment"))]

cor_tbl <- cor(LoanAna[which(c(sapply(LoanAna, is.numeric)) & colnames(LoanAna) %in% indep_vars)], use = "complete.obs")
corrplot(cor_tbl, method="number",  col = col(200),
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         main = "Correlation Table with correlated independent Vars removed"
)

# Before i decided to ditch the variables completely or calculate values for NAi want to know if these vars are even relevant for a modell
# Doing a quick glm for testing
form <- as.formula(paste(dep_vars_calc, paste(indep_vars, collapse = " + "), sep = " ~ "))
glmTest <- glm(formula = form, family = binomial(link = "logit"), 
               data = LoanAna, na.action=na.exclude)
summary(glmTest)
test <- data.frame(summary(glmTest)$coefficients)

# clean indep_vars of non-significant vars
indep_vars <- indep_vars[-which(indep_vars %in% c("Country","Age","Gender","EmploymentDurationCurrentEmployer","NeedvsHave", "LanguageCode","Education", "PreviousEarlyRepaymentsBefoleLoan", "HomeOwnershipType", "LiabilitiesTotal", "AmountOfPreviousLoansBeforeLoan", "PreviousEarlyRepaymentsCountBeforeLoan"))]

# Remove NA
LoanAna <- na.omit(LoanAna[c(dep_vars_calc, indep_vars, other_vars, "MMYY_credit")])

# For good measure we are also doing AIC-Test
# Stepwise Modelcreation
#define intercept-only model
intercept_only_model <- glm(formula = Defaulted ~ 1, family = binomial(link = "logit"), 
                            data = LoanAna )

#define total model
formula(paste("Defaulted",paste0(indep_vars, collapse=" + "), sep = " ~ " ))
total_model <- glm(formula = formula(paste("Defaulted",paste0(indep_vars, collapse=" + "), sep = " ~ " )), family = binomial(link = "logit"), 
                   data = LoanAna )

#perform stepwise regression
step(intercept_only_model, direction = 'both', scope = formula(total_model))

# Graph cleaned data
par(mfrow=c(2,2))

for (i in indep_vars){
  if (is.numeric(LoanAna[[i]])){
    form <- as.formula(paste0(i, "~ MMYY_credit"))
    boxplot(LoanAna[i], main= paste(i , "[cleaned]", sep = " "))
    boxplot(form, data=LoanAna, main= paste(i, "vs MM_credit [cleaned]", sep = " "))
  } else {
    next
  }
}

# remove rows with empty cells
LoanAna <- LoanAna[complete.cases(LoanAna[indep_vars]), ]
cat("Final dimensions Loan Analysis:", dim(LoanAna), "\n")

# Define Training and Test
training.samples <- LoanAna$Defaulted %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- as.data.frame(LoanAna[training.samples, ])
test.data <-  as.data.frame(LoanAna[-training.samples, ])

## Creating the logistic Model ##

# Create growing formula for testing
glm_ls <- list()
rightform <- NULL
trainer <- "train.data"
tester <- "test.data"
R2Pseudo <- list()

# Adding each independent var by a loop to the formula
# In the loop a glm (binomial), a calculation of McFaddens Pseudo R and predictions of glm (train data-set) on test data-set are done
for (i in indep_vars){
  # Create growing formula for testing
  if(is.null(rightform)) {
    rightform <- i
  } else {
    rightform <- paste(rightform, i, sep = " + ")
  }
  form <- as.formula(paste(dep_vars_calc, rightform, sep = " ~ "))
  tmp <- glm(formula = form, family = binomial(link = "logit"), 
             data = get(trainer))
  glm_ls[[i]] <- tmp
  
  cat("\n", i, "\n")
  # print pseudo R-squared
  nullmod <- glm(as.formula(paste(dep_vars_calc, 1, sep = " ~ ")) , data = get(trainer))
  print('Pseudo R-square')
  # McFadens pseudo R for logit-modells
  print(1-logLik(tmp)/logLik(nullmod))
  R2Pseudo[[i]] <- c(1-logLik(tmp)/logLik(nullmod))
  try(print(vif(tmp)))
  
  # Make predictions
  tmp_pred <- predict(tmp,
                      newdata = get(tester),
                      type = "response")
  
  tmp_test <- as.vector(ifelse(tmp_pred > 0.5, 1, 0))
  
  table(tmp_test, get(tester)$Defaulted)
  print("Mean prediction")
  print(mean(tmp_test == get(tester)$Defaulted, na.rm = T))
  print("Sum of wrong predictions")
  print(dim(get(tester)))
  print(sum(abs(tmp_test - get(tester)$Defaulted), na.rm = T))
}

# Test - deleting Variables with high vif and no significance for the model

# Relative importance of independent Vars
relImp <- varImp(glm_ls$DaysLastCred, scale = F)
rowName <- rownames(relImp)
orderName <- order(relImp$Overall, decreasing = T)
relImp$Overall <- relImp$Overall[orderName]
rownames(relImp)<- rowName[orderName]

# Display relative importance as barplot
# Function found in https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot/21978596
# User: Cybernetic
rotate_x <- function(data, column_to_plot, labels_vec, rot_angle, text_main, ymax) {
  plt <- barplot(data[[column_to_plot]], col='blue', xaxt="n", main = text_main, ylim = c(0,ymax))
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.8) 
}

par(mfrow=c(1,1))
rotate_x(relImp, 'Overall', row.names(relImp), 20, text_main = "Relative Importance of indep. Vars in Default Prediction", ymax = 30)

# make predictions and add to test

# Model #5
model5 <- test.data
model5$PredictionProbabilityOfDefault <- round(predict(glm_ls$IncomeTotal,
                                                          newdata = model5,
                                                          type = "response"), digits = 8)

# Model #7
model7 <- test.data
model7$PredictionProbabilityOfDefault <- round(predict(glm_ls$DaysLastCred,
                                                          newdata = model7,
                                                          type = "response"), digits = 8)

# Test the modells against the default probability given by Bondora in LoanData 
test.model5 <- melt(model5, id = c( "ProbabilityOfDefault", "PredictionProbabilityOfDefault" ), measure.vars = c( "ProbabilityOfDefault", "PredictionProbabilityOfDefault" ))
test.model7 <- melt(model7, id = c( "ProbabilityOfDefault", "PredictionProbabilityOfDefault" ), measure.vars = c( "ProbabilityOfDefault", "PredictionProbabilityOfDefault" ))

# plot the probability as histogramms
for(i in c("test.model5", "test.model7")){
  print(
    ggplot(get(i), aes(x=value, fill = variable)) + 
    geom_histogram(bins = 100, alpha = 0.7, position = "identity") +
    scale_fill_manual(values=c("purple", "orange")) +
    labs(fill="") +
    theme(
      panel.background = element_rect(fill = "#F5F5F5"),
      legend.position="right",
      axis.text=element_text(size=10),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 14),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("Bondora ProbabilityOfDefault vs PredictionProbabilityOfDefault (+60 days or later)") +
    xlab("Probability of Default") +
    ylab("Count of Probability per bin")
  )
}

##########################################################################################
###################### Bondora credit defaulting risk analysis ########################### END
##########################################################################################
