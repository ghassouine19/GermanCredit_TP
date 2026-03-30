
# Mini-Projet Classification – Version améliorée interactive
# Comparaison Random Forest vs Naive Bayes
# Base de données : GermanCredit.csv

# ==========================
# 1️ Chargement des librairies
# ==========================

library(shiny)          # Création application web interactive
library(randomForest)   # Modèle Random Forest
library(e1071)          # Naive Bayes
library(caret)          # Partition + matrice de confusion
library(dplyr)          # Manipulation données
library(ggplot2)        # Visualisations

# ==========================
# 2️ Interface utilisateur (UI)
# ==========================

ui <- fluidPage(
  
  titlePanel("Classification Interactive – Master BD2C - GHASSOUINE Abderrahmane - ELAYADI Younes - ZAITER Walid - JAMMARI Zineb"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Paramètres du modèle"),
      
      selectInput("model",
                  "Choisir le modèle :",
                  choices = c("Random Forest", "Naive Bayes")),
      
      selectInput("metric",
                  "Métrique de performance :",
                  choices = c("Accuracy", "Precision", "Recall")),
      
      sliderInput("trees",
                  "Nombre d’arbres (Random Forest)",
                  min = 50, max = 500, value = 200),
      
      actionButton("run", "Lancer la classification")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Résultats",
                 verbatimTextOutput("result"),
                 tableOutput("confmat")),
        
        tabPanel("Distribution Classe",
                 plotOutput("classPlot")),
        
        tabPanel("Importance Variables",
                 plotOutput("varImpPlot"))
      )
    )
  )
)

# ==========================
# 3️ Serveur
# ==========================

server <- function(input, output) {
  
  ###########################################################
  # 3.1 Chargement et préparation des données
  ###########################################################
  
  data_raw <- reactive({
    
    # Chargement GermanCredit
    df <- read.csv("GermanCredit.csv")
    
    # Conversion de la variable cible en facteur
    df$Class <- as.factor(df$Class)
    
    df
  })
  
  ###########################################################
  # 3.2 Séparation Train / Test (80% / 20%)
  ###########################################################
  
  split_data <- reactive({
    
    df <- data_raw()
    
    set.seed(123)
    
    idx <- createDataPartition(df$Class, p = 0.8, list = FALSE)
    
    list(
      train = df[idx, ],
      test  = df[-idx, ]
    )
  })
  
  ###########################################################
  # 3.3 Entraînement des modèles
  ###########################################################
  
  train_model <- reactive({
    
    req(input$run)
    
    sp <- split_data()
    
    if (input$model == "Random Forest") {
      
      model <- randomForest(
        Class ~ .,
        data = sp$train,
        ntree = input$trees
      )
      
    } else {
      
      model <- naiveBayes(
        Class ~ .,
        data = sp$train
      )
    }
    
    model
  })
  
  ###########################################################
  # 3.4 Prédiction
  ###########################################################
  
  predictions <- reactive({
    
    sp <- split_data()
    
    predict(train_model(), sp$test)
  })
  
  ###########################################################
  # 3.5 Matrice de confusion
  ###########################################################
  
  conf <- reactive({
    
    sp <- split_data()
    
    confusionMatrix(predictions(), sp$test$Class)
  })
  
  ###########################################################
  # 3.6 Affichage métriques
  ###########################################################
  
  output$result <- renderPrint({
    
    if (input$metric == "Accuracy") {
      conf()$overall["Accuracy"]
    } else if (input$metric == "Precision") {
      conf()$byClass["Precision"]
    } else {
      conf()$byClass["Recall"]
    }
  })
  
  output$confmat <- renderTable({
    conf()$table
  })
  
  ###########################################################
  # 3.7 Visualisation distribution classe
  ###########################################################
  
  output$classPlot <- renderPlot({
    
    df <- data_raw()
    
    ggplot(df, aes(x = Class)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Distribution des classes",
           x = "Classe",
           y = "Nombre d'observations")
  })
  
  ###########################################################
  # 3.8 Importance des variables (RF uniquement)
  ###########################################################
  
  output$varImpPlot <- renderPlot({
    
    if (input$model == "Random Forest") {
      varImpPlot(train_model())
    }
  })
}

# ==========================
# 4️ Lancement application
# ==========================

shinyApp(ui = ui, server = server)
