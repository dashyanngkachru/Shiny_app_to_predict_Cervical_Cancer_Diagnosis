
library(shiny)
library(ggplot2)
library(dplyr)
library(caret)
library(caTools)

# Define UI for application
ui <- fluidPage(
  
  navbarPage("Cervical Cancer Diagnosis",
             tabPanel("Welcome Page",
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     img(src = "CervicalCancer.jpg", height = 300, width = 200)
                        ),
                        mainPanel(
                          div(h3("This is a Shiny app to predict Cervical Cancer Diagnosis", style = "font-family: 'times'; font-si16pt; color:red")),
                          p(h4(strong("Author: "), "Dashyanng Kachru", style = "font-family: 'times'; font-si16pt")),
                          p(h4(strong("Data Source: "), "Cervical cancer (Risk Factors) Data Set from UCI Machine Learning Repository. 
                                                               The dataset was collected at 'Hospital Universitario de Caracas' in
                                                               Caracas, Venezuela and comprises demographic information, habits, and historic medical records of 858 patients.",
                               style = "font-family: 'times'; font-si16pt")),
                          p(h4(strong("Background Information: "), "Even though cervical cancer can be prevented and cured by removing affected tissues in
                                                                          early stages, it remains a significant cause of mortality all around the world, especially in
                                                                          low income countries, as providing universal and efficient access to cervical screening
                                                                          programs is a challenge. It rarely develops in women younger than 20 years while it is most frequently diagnosed
                                                                          in women between the ages of 35 years and 44 years with 50 years being the average age
                                                                          at diagnosis. The American Cancer Society estimates that about 4,290 women will die
                                                                          from cervical cancer and about 13,800 new cases of invasive cervical cancer will be
                                                                          diagnosed in the United States for 2020.", style = "font-family: 'times'; font-si16pt")),
                        ),
                      ),
             ),
             tabPanel("Data",
                      sidebarLayout(
                        sidebarPanel(
                         fileInput("file1", "Choose CSV File", accept = (".csv")),
                         checkboxInput("cor", label = "Remove highly correlated variables", value = FALSE),
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Contents", tableOutput("contents")),
                                      tabPanel("Summary", verbatimTextOutput("view")),
                                      tabPanel("Correlation", verbatimTextOutput("view2"))
                          )
                        ),
                      ),
             ),
             navbarMenu("Data Visualization",
                        tabPanel("Univariate Distribution",
                                 titlePanel("Univariate distribution of original dataframe"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("var"),
                                     uiOutput("var2"),
                                     textInput(inputId = "title", label = "Title of the plot for numeric variable", value = "Distribution of numeric variable"),
                                     textInput(inputId = "title2", label = "Title of the plot for factor variable", value = "Distribution of factor variable"),
                                     radioButtons(inputId = "color", label = "Choose the color for numeric variable",
                                                  choices = c("Blue", "Green", "Red", "Purple"), selected = "Red"),
                                     radioButtons(inputId = "color2", label = "Choose the color for factor variable",
                                                  choices = c("Blue", "Green", "Red", "Purple"), selected = "Green")
                                   ),
                                   mainPanel(
                                     plotOutput("plot"),
                                     plotOutput("plot2")
                                   ),
                                 ),
                        ),
                        tabPanel("Bivariate Distribution",
                                 titlePanel("Bivariate distribution of original dataframe"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("var3"),
                                     uiOutput("var4"),
                                     selectInput("position", label = "Select position", choices = c("stack", "dodge")),
                                     textInput(inputId = "title3", label = "Title of the plot for numeric variable", value = "Distribution of numeric variable"),
                                     textInput(inputId = "title4", label = "Title of the plot for factor variable", value = "Distribution of factor variable"),
                                   ),
                                   mainPanel(
                                     plotOutput("plot3"),
                                     plotOutput("plot4")
                                   ),
                                 ),
                        )),
             tabPanel("Machine Learning",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(paste("Please do remove highly correlated (above 60%) variables before doing imputation:")),
                          selectInput("method", label = "Perform imputation", choices = c("", "Mode", "Median", "Mean")),
                          selectInput("mvar", "Choose training set split:", choices= c(.6, .7)),
                          selectInput("mvar2", "Choose method for resampling:", choices= c("none", "boot", "cv", "repeatedcv")),
                          selectInput("mvar3", "Choose number of folds/resampling iterations:", choices= c(3,5,10)),
                          selectInput("mvar4", "Choose number of repeats:", choices= c(3,5,10)),
                          helpText(paste("Please do remove variables highly correlated (above 60%) before choosing model:")),
                          selectInput("ml", label = "Select model:", choices = c("Logistic Regression", "Gradient Boosting", "Support Vector Machine")),
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Training Data", tableOutput("contents3")),
                                      tabPanel("Training model", verbatimTextOutput("contents2")),
                                      tabPanel("Prediction", verbatimTextOutput("pred")),
                                      tabPanel("Confusion Matrix", verbatimTextOutput("conf"))
                          ),
                        ),
                      )
             )
  )
)


# Define server logic 
server <- function(input, output){
  
  # Upload csv
  cer_cancer <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    validate(
      need(inFile == "risk_factors_cervical_cancer.csv", "Upload File: risk_factors_cervical_cancer.csv")
    )
    
    cancer <- read.csv(inFile$datapath, na.strings = "?")
    
    # Renaming columns
    names(cancer)[names(cancer) == "Number.of.sexual.partners"] <- "Number_of_sexual_partners"
    names(cancer)[names(cancer) == "First.sexual.intercourse"] <- "First_sexual_intercourse"
    names(cancer)[names(cancer) == "Num.of.pregnancies"] <- "Num_of_pregnancies"
    names(cancer)[names(cancer) == "Smokes..years."] <- "Smokes_years"
    names(cancer)[names(cancer) == "Smokes..packs.year."] <- "Smokes_packs_year"    
    names(cancer)[names(cancer) == "Hormonal.Contraceptives"] <- "Hormonal_Contraceptives"    
    names(cancer)[names(cancer) == "Hormonal.Contraceptives..years."] <- "Hormonal_Contraceptives_years"    
    names(cancer)[names(cancer) == "IUD..years."] <- "IUD_years"    
    names(cancer)[names(cancer) == "STDs..number."] <- "STDs_number"    
    names(cancer)[names(cancer) == "STDs.condylomatosis"] <- "STDs_condylomatosis"    
    names(cancer)[names(cancer) == "STDs.cervical.condylomatosis"] <- "STDs_cervical_condylomatosis"    
    names(cancer)[names(cancer) == "STDs.vaginal.condylomatosis"] <- "STDs_vaginal_condylomatosis"    
    names(cancer)[names(cancer) == "STDs.vulvo.perineal.condylomatosis"] <- "STDs_vulvo_perineal_condylomatosis"    
    names(cancer)[names(cancer) == "STDs.syphilis"] <- "STDs_syphilis"    
    names(cancer)[names(cancer) == "STDs.syphilis"] <- "STDs_syphilis"    
    names(cancer)[names(cancer) == "STDs.pelvic.inflammatory.disease"] <- "STDs_pelvic_inflammatory_disease"    
    names(cancer)[names(cancer) == "STDs.genital.herpes"] <- "STDs_genital_herpes"    
    names(cancer)[names(cancer) == "STDs.molluscum.contagiosum"] <- "STDs_molluscum_contagiosum"    
    names(cancer)[names(cancer) == "STDs.AIDS"] <- "STDs_AIDS"    
    names(cancer)[names(cancer) == "STDs.HIV"] <- "STDs_HIV"    
    names(cancer)[names(cancer) == "STDs.Hepatitis.B"] <- "STDs_Hepatitis_B"    
    names(cancer)[names(cancer) == "STDs.HPV"] <- "STDs_HPV"    
    names(cancer)[names(cancer) == "STDs..Number.of.diagnosis"] <- "STDs_Number_of_diagnosis"    
    names(cancer)[names(cancer) == "STDs..Time.since.first.diagnosis"] <- "STDs_Time_since_first_diagnosis"    
    names(cancer)[names(cancer) == "STDs..Time.since.last.diagnosis"] <- "STDs_Time_since_last_diagnosis"    
    names(cancer)[names(cancer) == "Dx.Cancer"] <- "Dx_Cancer"    
    names(cancer)[names(cancer) == "Dx.CIN"] <- "Dx_CIN"    
    names(cancer)[names(cancer) == "Dx.HPV"] <- "Dx_HPV"   
    
    # Converting numeric to factor variables
    cancer$Dx_Cancer <- as.factor(cancer$Dx_Cancer)
    cancer$Dx_CIN <- as.factor(cancer$Dx_CIN)
    cancer$Dx_HPV <- as.factor(cancer$Dx_HPV)
    cancer$Dx <- as.factor(cancer$Dx)
    cancer$Hinselmann <- as.factor(cancer$Hinselmann)
    cancer$Schiller <- as.factor(cancer$Schiller)
    cancer$Citology <- as.factor(cancer$Citology)
    cancer$Biopsy <- as.factor(cancer$Biopsy)
    levels(cancer$Schiller) <- c("Healthy","Cancer")
    
    # Dropping columns that have more than 60% NAs and columns that have only one level 
    cancer <- subset(cancer, select = -c(STDs_cervical_condylomatosis, STDs_AIDS, STDs_Time_since_first_diagnosis,
                                         STDs_Time_since_last_diagnosis))
    cancer
  })

  # Display dataframe
  output$contents <- renderTable({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    cancer_data
    X <- select(cancer_data, Age:STDs_Number_of_diagnosis)
    X
  })
  
  # Display summary of dataframe
  output$view <- renderPrint({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    X <- select(cancer_data, Age:STDs_Number_of_diagnosis)
    summary(X)
  })
  
  # Display correlation
  output$view2 <- renderPrint({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer <- cer_cancer()
    X <- select(cancer, Age:STDs_Number_of_diagnosis)
    cor_X <- cor(X, use= "pairwise.complete.obs", method = "spearman")
    cor_X
  })

  # Display training data
  output$contents3 <- renderPrint({
    validate(
      need(input$method != "", "Please do imputation after removing highly correlated variables first")
    )
  })
    
  # Display training model
  output$contents2 <- renderPrint({
    validate(
      need(input$method != "", "Please do imputation after removing highly correlated variables first")
    )
  })
  
  # Display confusion matrix for testing
  output$conf <- renderPrint({
    validate(
      need(input$method != "", "Please do imputation after removing highly correlated variables first")
    )
  })
  
  # Display prediction of testing data
  output$pred <- renderPrint({
    validate(
      need(input$method != "", "Please do imputation after removing highly correlated variables first")
    )
  })
  
  # Observe imputation, correlation, machine learning method, training set split, resampling, fold, repeats
  check_list <- reactive({
    list(input$cor, input$method, input$ml, input$mvar, input$mvar2, input$mvar3, input$mvar4)
  })
  
  observeEvent(check_list(), {
    
    if (is.null(cer_cancer())){
      return(NULL)
    }
    
    cancer <- cer_cancer()
    
    if(input$cor == TRUE){
      # Variables left after correlated variables are removed
      X <- select(cancer, c(Age,Number_of_sexual_partners,First_sexual_intercourse,Num_of_pregnancies,
                            Smokes,Hormonal_Contraceptives,IUD,STDs))
      
      # Create mode function
      mode_func <- function(x) {                                     
        unique_x <- unique(x)
        mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
        mode
      }
      
      response <- cancer$Schiller
      
      # Making the result reproducible
      set.seed(800)
      
      # Train-test split
      df <- cbind(X, response)
      index <- createDataPartition(y = df$response, p = as.numeric(input$mvar), list = FALSE)
      training <- df[index,]
      testing  <- df[-index,]
      
      if(input$method == ""){
        output$contents <- renderTable({
          X  
        })
        
        output$view <- renderPrint({
          summary(X)
        })
        
        output$view2 <- renderPrint({
          cor_X <- cor(X, use= "pairwise.complete.obs", method = "spearman")
          cor_X
        })
        
        output$contents2 <- renderPrint({
          validate(
            need(input$method != "", "Please do imputation first")
          )
        })
        
        output$contents3 <- renderPrint({
          validate(
            need(input$method != "", "Please do imputation first")
          )
        })
        
        output$conf <- renderPrint({
          validate(
            need(input$method != "", "Please do imputation first")
          )
        })
        
        output$pred <- renderPrint({
          validate(
            need(input$method != "", "Please do imputation first")
          )
        })
      }
      
      # Performing mode imputation
      if (input$method == "Mode"){ 
        
        # Imputing variables in the training data using mode
        training$Number_of_sexual_partners[is.na(training$Number_of_sexual_partners)] <- mode_func(training$Number_of_sexual_partners)
        training$First_sexual_intercourse[is.na(training$First_sexual_intercourse)] <- mode_func(training$First_sexual_intercourse)
        training$Num_of_pregnancies[is.na(training$Num_of_pregnancies)] <- mode_func(training$Num_of_pregnancies)
        training$Smokes[is.na(training$Smokes)] <- mode_func(training$Smokes)
        training$Hormonal_Contraceptives[is.na(training$Hormonal_Contraceptives)] <- mode_func(training$Hormonal_Contraceptives)
        training$IUD[is.na(training$IUD)] <- mode_func(training$IUD)
        training$STDs[is.na(training$STDs)] <- mode_func(training$STDs)
        
        # Imputing variables in the testing data using mode
        testing$Number_of_sexual_partners[is.na(testing$Number_of_sexual_partners)] <- mode_func(testing$Number_of_sexual_partners)
        testing$First_sexual_intercourse[is.na(testing$First_sexual_intercourse)] <- mode_func(testing$First_sexual_intercourse)
        testing$Num_of_pregnancies[is.na(testing$Num_of_pregnancies)] <- mode_func(testing$Num_of_pregnancies)
        testing$Smokes[is.na(testing$Smokes)] <- mode_func(testing$Smokes)
        testing$Hormonal_Contraceptives[is.na(testing$Hormonal_Contraceptives)] <- mode_func(testing$Hormonal_Contraceptives)
        testing$IUD[is.na(testing$IUD)] <- mode_func(testing$IUD)
        testing$STDs[is.na(testing$STDs)] <- mode_func(testing$STDs)
        
        output$contents3 <- renderTable({
          training
        })
        
        if(input$ml == "Logistic Regression"){
          
          if(input$mvar2 == "repeatedcv"){
            
            mode_glm <- train(response ~ ., method = "glm", 
                              trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       repeats = as.numeric(input$mvar4), sampling = "up"),
                              metric = "Accuracy", family = "binomial", data = training)
            
            output$contents2 <- renderPrint({
              mode_glm
            })
            
            output$pred <- renderPrint({
              predict(mode_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mode_glm, testing[ ,1:8]), testing$response)
            })
            
            output$contents3 <- renderTable({
              training
            })
          }
          
          else{
          
            mode_glm <- train(response ~ ., method = "glm", 
                              trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       sampling = "up"),
                             metric ="Accuracy", family = "binomial", data = training)
            
            output$contents2 <- renderPrint({
              mode_glm
            })
            
            output$pred <- renderPrint({
              predict(mode_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mode_glm, testing[ ,1:8]), testing$response)
            })
          }   
        }
        
        if(input$ml == "Gradient Boosting"){
             
             if(input$mvar2 == "repeatedcv"){
               
               gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                       n.trees = c(100,250,500,1000,5000), 
                                       shrinkage = c(0.01, 0.1),
                                       n.minobsinnode = c(10,20))
               
               mode_gbm <- train(response ~ ., method = "gbm", tuneGrid = gbmGrid, metric ="ROC", data = training, 
                                trControl = trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                         method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         repeats = as.numeric(input$mvar4), preProc = "range", sampling = "up"))
               
               output$contents2 <- renderPrint({
                 mode_gbm
               })
               
               output$pred <- renderPrint({
                 predict(mode_gbm, newdata = testing[ ,1:8])
               })
               
               output$conf <- renderPrint({
                 confusionMatrix(predict(mode_gbm, newdata = testing[ ,1:8]), testing$response)
               })
               output$contents3 <- renderTable({
                 training
               })
             }     
          
             else if (input$mvar2 == "cv"){
            
               gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                       n.trees = c(100,250,500,1000,5000), 
                                       shrinkage = c(0.01, 0.1),
                                       n.minobsinnode = c(10,20))
               
               mode_gbm <- train(response ~ ., data = training, method = "gbm",
                                 trControl= trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                         method = input$mvar2, number = as.numeric(input$mvar3), sampling = "up"),
                                 preProcess = "range", metric = "ROC", tuneGrid = gbmGrid)
            
               output$contents2 <- renderPrint({
                 mode_gbm
               })
            
               output$pred <- renderPrint({
                 predict(mode_gbm, newdata = testing[ ,1:8])
               })
            
               output$conf <- renderPrint({
                 confusionMatrix(predict(mode_gbm, newdata = testing[ ,1:8]), testing$response)
               })
             } 
          
             else{ 
                   
               mode_gbm <- train(response ~ ., method = "gbm", metric ="Accuracy", data = training, 
                                trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         preProc = "range", sampling = "up"))
                    
               output$contents2 <- renderPrint({
                 mode_gbm
               })
               
               output$pred <- renderPrint({
                 predict(mode_gbm, newdata = testing[ ,1:8])
               })   
                   
               output$conf <- renderPrint({
                 confusionMatrix(predict(mode_gbm, newdata = testing[ ,1:8]), testing$response)
               })
             }
        }
        
        if(input$ml == "Support Vector Machine"){
          
          if(input$mvar2 == "repeatedcv"){
            
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            mode_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                     trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                             repeats = as.numeric(input$mvar4), sampling = "up"),
                                     preProcess = "range", metric = "Accuracy", tuneGrid = grid, tuneLength = 10)
            
            output$contents2 <- renderPrint({
              mode_svm_Linear
            })
            
            output$pred <- renderPrint({
              predict(mode_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mode_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            output$contents3 <- renderTable({
              training
            })
          }
          
          else if (input$mvar2 == "cv"){
            
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            mode_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                     trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), sampling = "up"),
                                     preProcess = "range", metric = "Accuracy", tuneGrid = grid, tuneLength = 10)
            
            output$contents2 <- renderPrint({
              mode_svm_Linear
            })
            
            output$pred <- renderPrint({
              predict(mode_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mode_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
          } 
            
          else{
            
            mode_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                     trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                             sampling = "up"), preProcess = "range", metric = "Accuracy")
            
            output$contents2 <- renderPrint({
              mode_svm_Linear
            })
            
            output$pred <- renderPrint({
              predict(mode_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mode_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
          }
        }
      }
      
      # Performing median imputation
      
      if(input$method == "Median"){
        
        # Imputing variables in the training data using median
        training$Number_of_sexual_partners[is.na(training$Number_of_sexual_partners)] <- median(training$Number_of_sexual_partners, na.rm = TRUE)
        training$First_sexual_intercourse[is.na(training$First_sexual_intercourse)] <- median(training$First_sexual_intercourse, na.rm = TRUE)
        training$Num_of_pregnancies[is.na(training$Num_of_pregnancies)] <- median(training$Num_of_pregnancies, na.rm = TRUE)
        training$Smokes[is.na(training$Smokes)] <- median(training$Smokes, na.rm = TRUE)
        training$Hormonal_Contraceptives[is.na(training$Hormonal_Contraceptives)] <- median(training$Hormonal_Contraceptives, na.rm = TRUE)
        training$IUD[is.na(training$IUD)] <- median(training$IUD, na.rm = TRUE)
        training$STDs[is.na(training$STDs)] <- median(training$STDs, na.rm = TRUE)
        
        # Imputing variables in the testing data using median
        testing$Number_of_sexual_partners[is.na(testing$Number_of_sexual_partners)] <- median(testing$Number_of_sexual_partners, na.rm = TRUE)
        testing$First_sexual_intercourse[is.na(testing$First_sexual_intercourse)] <- median(testing$First_sexual_intercourse, na.rm = TRUE)
        testing$Num_of_pregnancies[is.na(testing$Num_of_pregnancies)] <- median(testing$Num_of_pregnancies, na.rm = TRUE)
        testing$Smokes[is.na(testing$Smokes)] <- median(testing$Smokes, na.rm = TRUE)
        testing$Hormonal_Contraceptives[is.na(testing$Hormonal_Contraceptives)] <- median(testing$Hormonal_Contraceptives, na.rm = TRUE)
        testing$IUD[is.na(testing$IUD)] <- median(testing$IUD, na.rm = TRUE)
        testing$STDs[is.na(testing$STDs)] <- median(testing$STDs, na.rm = TRUE)
        
        output$contents3 <- renderTable({
          training
        })
        
        if(input$ml == "Logistic Regression"){
          
          if(input$mvar2 == "repeatedcv"){
            median_glm <- train(response ~ ., method = "glm", family = "binomial", data = training,
                                trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         repeats = as.numeric(input$mvar4), sampling = "up"))
            
            output$pred <- renderPrint({
              predict(median_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_glm, testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              median_glm
            })
            output$contents3 <- renderTable({
              training
            })
          }
          
          else{
            median_glm <- train(response ~ ., method = "glm", family = "binomial", data = training,
                                trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         sampling = "up"))
            
            output$pred <- renderPrint({
              predict(median_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_glm, testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              median_glm
            })
          }
        }
        
        if(input$ml == "Gradient Boosting"){
          
          if(input$mvar2 == "repeatedcv"){
            
            gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                    n.trees = c(100,250,500,1000,5000), 
                                    shrinkage = c(0.01, 0.1),
                                    n.minobsinnode = c(10,20))
            
            median_gbm <- train(response ~ ., method = "gbm", tuneGrid = gbmGrid, metric ="ROC", data = training, 
                              trControl = trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                       method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       repeats = as.numeric(input$mvar4), preProc = "range", sampling = "up"))
            
            output$contents2 <- renderPrint({
              median_gbm
            })
            
            output$pred <- renderPrint({
              predict(median_gbm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_gbm, newdata = testing[ ,1:8]), testing$response)
            })
            output$contents3 <- renderTable({
              training
            })
          }     
          
          else if (input$mvar2 == "cv"){
            
            gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                    n.trees = c(100,250,500,1000,5000), 
                                    shrinkage = c(0.01, 0.1),
                                    n.minobsinnode = c(10,20))
            
            median_gbm <- train(response ~ ., data = training, method = "gbm",
                              trControl= trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                      method = input$mvar2, number = as.numeric(input$mvar3), sampling = "up"),
                              preProcess = "range", metric = "ROC", tuneGrid = gbmGrid)
            
            output$contents2 <- renderPrint({
              median_gbm
            })
            
            output$pred <- renderPrint({
              predict(median_gbm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_gbm, newdata = testing[ ,1:8]), testing$response)
            })
          } 
          
          else{ 
            
            median_gbm <- train(response ~ ., method = "gbm", metric ="Accuracy", data = training, 
                              trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       preProc = "range", sampling = "up"))
            
            output$contents2 <- renderPrint({
              median_gbm
            })
            
            output$pred <- renderPrint({
              predict(median_gbm, newdata = testing[ ,1:8])
            })   
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_gbm, newdata = testing[ ,1:8]), testing$response)
            })
          }
        }
        
        if(input$ml == "Support Vector Machine"){
          
          if(input$mvar2 == "repeatedcv"){      
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            median_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               repeats = as.numeric(input$mvar4), sampling = "up"), tuneGrid = grid,
                                       preProcess = "range", metric = "Accuracy", tuneLength = 10)
            
            output$pred <- renderPrint({
              predict(median_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              median_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
          
          else if(input$mvar2 == "cv"){      
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            median_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               sampling = "up"), tuneGrid = grid,
                                       preProcess = "range", metric = "Accuracy", tuneLength = 10)
            
            output$pred <- renderPrint({
              predict(median_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              median_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
          
          else{
            median_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               sampling = "up"), preProcess = "range", metric = "Accuracy")
            
            output$pred <- renderPrint({
              predict(median_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(median_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              median_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
        }  
      }
      
      # Performing mean imputation
      if(input$method == "Mean"){
        # Imputing variables that will be used for modeling using mean
        X$Number_of_sexual_partners[is.na(X$Number_of_sexual_partners)] <- mean(X$Number_of_sexual_partners, na.rm = TRUE)
        X$First_sexual_intercourse[is.na(X$First_sexual_intercourse)] <- mean(X$First_sexual_intercourse, na.rm = TRUE)
        X$Num_of_pregnancies[is.na(X$Num_of_pregnancies)] <- mean(X$Num_of_pregnancies, na.rm = TRUE)
        X$Smokes[is.na(X$Smokes)] <- mean(X$Smokes, na.rm = TRUE)
        X$Hormonal_Contraceptives[is.na(X$Hormonal_Contraceptives)] <- mean(X$Hormonal_Contraceptives, na.rm = TRUE)
        X$IUD[is.na(X$IUD)] <- mean(X$IUD, na.rm = TRUE)
        X$STDs[is.na(X$STDs)] <- mean(X$STDs, na.rm = TRUE)
        
        response <- cancer$Schiller
        
        # Making the result reproducible
        set.seed(800)
        
        # Train test split
        df <- cbind(X, response)
        index <- createDataPartition(y = df$response, p = as.numeric(input$mvar), list = FALSE)
        training <- df[index,]
        testing  <- df[-index,]
        
        output$contents <- renderTable({
          X
        })
        
        output$view <- renderPrint({
          summary(X)
        })
        
        output$view2 <- renderPrint({
          cor_X <- cor(X, use= "pairwise.complete.obs", method = "spearman")
          cor_X
        })
        
        if(input$ml == "Logistic Regression"){
          
          if(input$mvar2 == "repeatedcv"){
            mean_glm <- train(response ~ ., method = "glm", family = "binomial", data = training,
                              trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       repeats = as.numeric(input$mvar4), sampling = "up"))
            
            output$pred <- renderPrint({
              predict(mean_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_glm, testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              mean_glm
            })
          }
          else{
            mean_glm <- train(response ~ ., method = "glm", family = "binomial", data = training,
                              trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                       sampling = "up"))
            
            output$pred <- renderPrint({
              predict(mean_glm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_glm, testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              mean_glm
            })
          }
        }
        
        if(input$ml == "Gradient Boosting"){
          
          if(input$mvar2 == "repeatedcv"){
            
            gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                    n.trees = c(100,250,500,1000,5000), 
                                    shrinkage = c(0.01, 0.1),
                                    n.minobsinnode = c(10,20))
            
            mean_gbm <- train(response ~ ., method = "gbm", tuneGrid = gbmGrid, metric ="ROC", data = training, 
                                trControl = trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                         method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         repeats = as.numeric(input$mvar4), preProc = "range", sampling = "up"))
            
            output$contents2 <- renderPrint({
              mean_gbm
            })
            
            output$pred <- renderPrint({
              predict(mean_gbm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_gbm, newdata = testing[ ,1:8]), testing$response)
            })
            output$contents3 <- renderTable({
              training
            })
          }     
          
          else if (input$mvar2 == "cv"){
            
            gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                    n.trees = c(100,250,500,1000,5000), 
                                    shrinkage = c(0.01, 0.1),
                                    n.minobsinnode = c(10,20))
            
            mean_gbm <- train(response ~ ., data = training, method = "gbm",
                                trControl= trainControl(summaryFunction=twoClassSummary,classProbs = TRUE,
                                                        method = input$mvar2, number = as.numeric(input$mvar3), sampling = "up"),
                                preProcess = "range", metric = "ROC", tuneGrid = gbmGrid)
            
            output$contents2 <- renderPrint({
              mean_gbm
            })
            
            output$pred <- renderPrint({
              predict(mean_gbm, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_gbm, newdata = testing[ ,1:8]), testing$response)
            })
          } 
          
          else{ 
            
            mean_gbm <- train(response ~ ., method = "gbm", metric ="Accuracy", data = training, 
                                trControl = trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                         preProc = "range", sampling = "up"))
            
            output$contents2 <- renderPrint({
              mean_gbm
            })
            
            output$pred <- renderPrint({
              predict(mean_gbm, newdata = testing[ ,1:8])
            })   
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_gbm, newdata = testing[ ,1:8]), testing$response)
            })
          }
        }
        if(input$ml == "Support Vector Machine"){
          
          if(input$mvar2 == "repeatedcv"){      
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            mean_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               repeats = as.numeric(input$mvar4), sampling = "up"), tuneGrid = grid,
                                       preProcess = "range", metric = "Accuracy", tuneLength = 10)
            
            output$pred <- renderPrint({
              predict(mean_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              mean_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
          
          else if(input$mvar2 == "cv"){      
            grid <- expand.grid(C = c(0.005,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
            mean_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               sampling = "up"), tuneGrid = grid,
                                       preProcess = "range", metric = "Accuracy", tuneLength = 10)
            
            output$pred <- renderPrint({
              predict(mean_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              mean_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
          
          else{
            mean_svm_Linear <- train(response ~ ., data = training, method = "svmLinear",
                                       trControl= trainControl(method = input$mvar2, number = as.numeric(input$mvar3), 
                                                               sampling = "up"), preProcess = "range", metric = "Accuracy")
            
            output$pred <- renderPrint({
              predict(mean_svm_Linear, newdata = testing[ ,1:8])
            })
            
            output$conf <- renderPrint({
              confusionMatrix(predict(mean_svm_Linear, newdata = testing[ ,1:8]), testing$response)
            })
            
            output$contents2 <- renderPrint({
              mean_svm_Linear
            })
            output$contents3 <- renderPrint({
              training
            })
          }
        }
      }
    }
    
    if(input$cor != TRUE){
      # Variables selected when not removing correlated variables
      X <- select(cancer, Age:STDs_Number_of_diagnosis)
      output$contents <- renderTable({
        X  
      })
      
      output$view <- renderPrint({
        summary(X)
      })
      
      output$view2 <- renderPrint({
        cor_X <- cor(X, use= "pairwise.complete.obs", method = "spearman")
        cor_X
      })
      
      output$contents2 <- renderPrint({
        validate(
          need(input$cor == TRUE, "Please remove highly correlated variables first and then impute")
        )
      })
      
      output$pred <- renderPrint({
        validate(
          need(input$cor == TRUE, "Please remove highly correlated variables first and then impute")
        )
      })
      
      output$conf <- renderPrint({
        validate(
          need(input$cor == TRUE, "Please remove highly correlated variables first and then impute")
        )
      })
    }
  })
  
  # Selecting numeric variable to plot for univariate distribution
  output$var <- renderUI({
    if (is.null(cer_cancer())){
      return(NULL)
    }
    cancer_data <- cer_cancer()
    selectInput("var", "Choose variable:", choices= names(cancer_data %>% select(Age, Number_of_sexual_partners, First_sexual_intercourse, Num_of_pregnancies,
                                                                                 Smokes_years, Smokes_packs_year, Hormonal_Contraceptives_years, IUD_years)))
  })
  
  # Selecting factor variable to plot for univariate distribution
  output$var2 <- renderUI({
    if (is.null(cer_cancer())){
      return(NULL)
    }
    cancer_data <- cer_cancer()
    selectInput("var2", "Choose factor variable:", choices= names(cancer_data %>% select(-Age, -Number_of_sexual_partners, -First_sexual_intercourse, -Num_of_pregnancies,
                                                                                         -Smokes_years, -Smokes_packs_year, -Hormonal_Contraceptives_years, -IUD_years,
                                                                                         -Dx_Cancer, -Dx_CIN, -Dx_HPV, -Dx, -Hinselmann, -Schiller, -Citology, -Biopsy)))
  })
  
  # Univariate distribution of numeric variable
  output$plot <- renderPlot({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    g <- ggplot(cancer_data, aes(x = get(input$var))) + geom_density(color=input$color) +
      ggtitle(input$title) + xlab(input$var)
    return(g)
  })
  
  # Univariate distribution of factor variable
  output$plot2 <- renderPlot({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    g1 <- ggplot(cancer_data, aes(x = factor(get(input$var2)))) + geom_bar(color=input$color2) +
      ggtitle(input$title2) + xlab(input$var2)
    return(g1)
  })
  
  # Selecting numeric variable to plot for bivariate distribution
  output$var3 <- renderUI({
    if (is.null(cer_cancer())){
      return(NULL)
    }
    cancer_data <- cer_cancer()
    selectInput("var3", "Choose variable:", choices= names(cancer_data %>% select(Age, Number_of_sexual_partners, First_sexual_intercourse, Num_of_pregnancies,
                                                                                  Smokes_years, Smokes_packs_year, Hormonal_Contraceptives_years, IUD_years)))
  })
  
  # Selecting factor variable to plot for bivariate distribution
  output$var4 <- renderUI({
    if (is.null(cer_cancer())){
      return(NULL)
    }
    cancer_data <- cer_cancer()
    selectInput("var4", "Choose factor variable:", choices= names(cancer_data %>% select(-Age, -Number_of_sexual_partners, -First_sexual_intercourse, -Num_of_pregnancies,
                                                                                         -Smokes_years, -Smokes_packs_year, -Hormonal_Contraceptives_years, -IUD_years,
                                                                                         -Dx_Cancer, -Dx_CIN, -Dx_HPV, -Dx, -Hinselmann, -Schiller, -Citology, -Biopsy)))
  })
  
  # Bivariate distribution of numeric variable colored by Schiller (response variable)
  output$plot3 <- renderPlot({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    g2 <- ggplot(cancer_data) + geom_density(aes(x = get(input$var3), color = Schiller)) +
      ggtitle(input$title3) + xlab(input$var3)
    return(g2)
  })
  
  # Bivariate distribution of factor variable colored by Schiller (response variable)
  output$plot4 <- renderPlot({
    validate(
      need(is.null(cer_cancer()) == FALSE, "Please select a dataset first")
    )
    cancer_data <- cer_cancer()
    g3 <- ggplot(cancer_data) + geom_bar(aes(x = factor(get(input$var4)), fill = Schiller), position = input$position) +
      ggtitle(input$title4) + xlab(input$var4)
    return(g3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
