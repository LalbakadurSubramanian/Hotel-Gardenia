library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(car)
library(readxl)
library(openxlsx)
library(caret)
library(FactoMineR)
library(lubridate)
library(DT)

ui <- fluidPage(
  titlePanel("Hotel Clustering & Demand Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/Excel File", accept = c(".csv", ".xlsx")),
      checkboxInput("select_all", "Select All Features", value = TRUE),
      uiOutput("select_features"),
      numericInput("clusters", "Number of Clusters", value = 3, min = 2),
      actionButton("run", "Run Analysis"),
      downloadButton("download", "Download Clusters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Clusters",
                 plotOutput("cluster_plot", height = "600px"),
                 hr(),  # Horizontal line separator
                 plotOutput("pca_cluster_plot", height = "600px"),
                 hr(),  # Another separator for clarity
                 plotOutput("feature_importance_plot", height = "600px")
        ),
        
        tabPanel(
          "Price-Demand Curve",
          sidebarLayout(
            sidebarPanel(
              numericInput("clusters", "Number of Clusters:", value = 3, min = 1),
              uiOutput("priceInputs"),
              numericInput("lambda", "Lambda (Max Demand):", value = 500),
              numericInput("alpha", "Alpha (Price Sensitivity):", value = 0.0015),
              numericInput("capacity", "Capacity:", value = 200),
              actionButton("plot_curve", "Plot Demand Curve"),
              plotOutput("demandCurvePlot"),
              verbatimTextOutput("exp_formula")
            ),
            mainPanel(
              plotOutput("demandCurvePlot"),
              verbatimTextOutput("exp_formula")
            )
          )
        ),
        
        
        tabPanel("VIF Analysis", 
                 tableOutput("vif_table"),
                 plotOutput("vif_plot", height = "600px")  # 📈 Added VIF Plot
        ),
        
        tabPanel("Feature Distribution", plotOutput("feature_hist", height = "800px")),
        tabPanel("Data Check", tableOutput("data_summary")),
        tabPanel("PCA Variables", plotOutput("pca_plot", height = "700px")),
        tabPanel("Cluster Wise Distributions", plotOutput("box_plot", height = "700px")),
        tabPanel("Top 10 Rows", tableOutput("top_10_rows")),
        tabPanel("Cluster Distribution",
                 selectInput("dist_feature", "Select a Feature for Distribution", choices = NULL),
                 plotOutput("cluster_boxplot", height = "300px"),
                 plotOutput("cluster_histogram", height = "300px"),
                 tableOutput("cluster_summary")
        ),
        tabPanel("Features vs Clusters", 
                 tableOutput("features_vs_clusters_table")) ,
        tabPanel("Reward-Sacrifice Matrix", 
                 tableOutput("reward_sacrifice_table")
        ),
        tabPanel("Encoding Reference Table",
         tableOutput("encoding_reference_table")
),
# New tab added here
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else {
      read_excel(input$file$datapath)
    }
  })
  processed_data <- reactive({
    req(data())
    df <- data()
    
    # 📅 --- Date Handling ---
    date_cols <- c("Booking date", "Check in Date")
    for (col in date_cols) {
      if (col %in% names(df)) {
        if (is.numeric(df[[col]])) {
          # If numeric and large (epoch seconds), convert to POSIXct
          df[[col]] <- as.POSIXct(df[[col]], origin = "1970-01-01", tz = "UTC")
        } else if (is.character(df[[col]])) {
          # If character like "18/03/24", parse using dmy()
          df[[col]] <- lubridate::dmy(df[[col]])
        } else if (inherits(df[[col]], "Date") || inherits(df[[col]], "POSIXct")) {
          # Already Date/POSIXct - no changes needed
        }
      }
    }
    
    # 🔥 Encode character, factor, logical columns into numeric
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x))]
    df[cat_cols] <- lapply(df[cat_cols], function(x) as.numeric(as.factor(x)))
    
    # ✅ Convert Date/POSIXct to numeric for modeling
    date_cols_present <- names(df)[sapply(df, function(x) inherits(x, c("POSIXct", "Date")))]
    df[date_cols_present] <- lapply(df[date_cols_present], function(x) as.numeric(as.POSIXct(x)))
    
    # ✅ Handle infinite values
    df <- df %>% mutate(across(everything(), ~ifelse(is.infinite(.), NA, .)))
    
    df
  })
  
  
  output$select_features <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())
    checkboxGroupInput("features", "Select Features for Clustering:",
                       choices = numeric_cols,
                       selected = if (input$select_all) numeric_cols else character(0))
  })
  
  observeEvent(input$select_all, {
    numeric_cols <- names(processed_data())
    updateCheckboxGroupInput(session, "features", choices = numeric_cols,
                             selected = if (input$select_all) numeric_cols else character(0))
  })
  
  kmeans_result <- reactive({
    req(input$features, input$clusters)
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    df <- scale(df)
    kmeans(df, centers = input$clusters, nstart = 25)
  })
  
  output$cluster_plot <- renderPlot({
    req(kmeans_result())
    fviz_cluster(kmeans_result(), data = scale(processed_data()[, input$features, drop = FALSE]))
  })
  
  output$pca_cluster_plot <- renderPlot({
    req(input$features, kmeans_result())
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    df_scaled <- scale(df)
    
    # Perform PCA
    pca_res <- PCA(df_scaled, graph = FALSE)
    pca_df <- as.data.frame(pca_res$ind$coord)
    pca_df$Cluster <- as.factor(kmeans_result()$cluster)
    
    # Plot PCA colored by clusters
    ggplot(pca_df, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = "PCA Clustering Plot", x = "PC1", y = "PC2") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
  })
  
  output$feature_importance_plot <- renderPlot({
    req(input$features, kmeans_result())
    centers <- kmeans_result()$centers
    importance <- apply(centers, 2, function(x) sd(x))
    importance_df <- data.frame(Feature = names(importance), Importance = importance)
    
    ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = round(Importance, 2)), hjust = -0.2) +
      coord_flip() +
      labs(title = "Feature Importance based on Cluster Centroid Variation",
           x = "Feature", y = "Importance (SD across centroids)") +
      theme_minimal()
  })
  
  # cluster inputs
  output$priceInputs <- renderUI({
    req(input$clusters)
    lapply(1:input$clusters, function(i) {
      numericInput(
        inputId = paste0("price_", i),
        label = paste("Price for Cluster", i, ":"),
        value = 100 * i, min = 0
      )
    })
  })
  
  observeEvent(input$plot_curve, {
    req(input$clusters, input$lambda, input$alpha, input$capacity)
    
    # Collect prices
    prices <- sapply(1:input$clusters, function(i) {
      as.numeric(input[[paste0("price_", i)]])
    })
    
    if (any(is.na(prices))) {
      showNotification("Please enter all price values!", type = "error")
      return()
    }
    
    lambda <- as.numeric(input$lambda)
    alpha <- as.numeric(input$alpha)
    capacity <- as.numeric(input$capacity)
    
    # Exponential Demand Calculation
    demand <- lambda * exp(-alpha * prices)
    
    # Calculate Revenue per cluster (capped at capacity)
    actual_demand <- pmin(demand, capacity)
    revenue <- prices * actual_demand
    total_revenue <- sum(revenue)
    
    # Plot demand curve with capacity line
    output$demandCurvePlot <- renderPlot({
      plot(prices, demand, type = "b", pch = 19, col = "darkred",
           xlab = "Price (AED)", ylab = "Estimated Demand",
           main = "Exponential Price-Demand Curve")
      
      # Add horizontal capacity line
      abline(h = capacity, col = "blue", lwd = 2, lty = 2)
      legend("topright", legend = paste("Capacity =", capacity),
             col = "blue", lwd = 2, lty = 2, bty = "n")
      
      grid()
    })
    
    # Print formula, demand, revenue, total revenue
    output$exp_formula <- renderPrint({
      cat("Exponential Demand Formula:\n")
      cat("D = λ * exp(-α * P)\n")
      cat(sprintf("λ (Max Demand) = %s\n", lambda))
      cat(sprintf("α (Price Sensitivity) = %s\n", alpha))
      cat(sprintf("Capacity = %s\n\n", capacity))
      cat("Prices (AED):", paste(round(prices, 2), collapse = ", "), "\n")
      cat("Estimated Demands:", paste(round(demand, 2), collapse = ", "), "\n")
      cat("Actual Demands (Capped by Capacity):", paste(round(actual_demand, 2), collapse = ", "), "\n")
      cat("Revenue per Cluster (AED):", paste(round(revenue, 2), collapse = ", "), "\n")
      cat("\nTotal Revenue (AED):", round(total_revenue, 2), "\n")
    })
  })
  
  
  
  
  
  #VIF ANALYSIS
  output$vif_table <- renderTable({
    req(input$features)
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    
    # Create formula with backticked features to handle spaces
    features_backtick <- paste0("`", names(df)[-1], "`", collapse = " + ")
    
    # Select a random dependent variable (first column)
    formula_str <- paste0("`", names(df)[1], "` ~ ", features_backtick)
    
    model <- lm(as.formula(formula_str), data = df)
    vif_vals <- vif(model)
    
    data.frame(Feature = names(vif_vals), VIF = vif_vals)
  })
  
  output$vif_plot <- renderPlot({
    req(input$features)
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    
    # Create formula with backticked features to handle spaces
    features_backtick <- paste0("`", names(df)[-1], "`", collapse = " + ")
    
    # Select a random dependent variable (first column)
    formula_str <- paste0("`", names(df)[1], "` ~ ", features_backtick)
    
    model <- lm(as.formula(formula_str), data = df)
    vif_vals <- vif(model)
    vif_df <- data.frame(Feature = names(vif_vals), VIF = vif_vals)
    
    ggplot(vif_df, aes(x = reorder(Feature, VIF), y = VIF, fill = VIF)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(VIF, 2)), hjust = -0.2) +  # Add labels to bars
      coord_flip() +
      scale_fill_gradient(low = "skyblue", high = "red") +
      labs(title = "VIF Plot",
           x = "Features",
           y = "VIF Value") +
      theme_minimal(base_size = 14)
  })
  
  output$feature_hist <- renderPlot({
    req(input$features)
    df <- processed_data()[, input$features, drop = FALSE]
    df_long <- pivot_longer(df, cols = everything())
    ggplot(df_long, aes(x = value)) +
      geom_histogram(bins = 30) +
      facet_wrap(~ name, scales = "free")
  })
  
  output$data_summary <- renderTable({
    df <- processed_data()
    summary_data <- data.frame(
      Feature = names(df),
      Missing_Values = sapply(df, function(x) sum(is.na(x))),
      Infinite_Values = sapply(df, function(x) sum(is.infinite(x)))
    )
    summary_data
  })
  
  output$pca_plot <- renderPlot({
    req(input$features)
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    df <- scale(df)
    pca_res <- PCA(df, graph = FALSE)
    fviz_pca_var(pca_res, col.var = "contrib", gradient.cols = c("blue", "yellow", "red"))
  })
  
  output$box_plot <- renderPlot({
    req(input$features, kmeans_result())
    df <- processed_data()[, input$features, drop = FALSE]
    df$Cluster <- as.factor(kmeans_result()$cluster)
    df_long <- pivot_longer(df, cols = -Cluster)
    ggplot(df_long, aes(x = Cluster, y = value, fill = Cluster)) +
      geom_boxplot() +
      facet_wrap(~ name, scales = "free") +
      theme_minimal()
  })
  
  output$top_10_rows <- renderTable({
    head(processed_data(), 10)
  })
  
  # Populate Feature Dropdown based on uploaded data
  observe({
    req(processed_data())
    updateSelectInput(session, "dist_feature", choices = names(processed_data()))
  })
  
  # Cluster-wise Boxplot
  output$cluster_boxplot <- renderPlot({
    req(kmeans_result(), input$dist_feature)
    plot_data <- processed_data() %>%
      mutate(Cluster = factor(kmeans_result()$cluster))  # Properly extract cluster labels
    
    ggplot(plot_data, aes(x = Cluster, y = .data[[input$dist_feature]], fill = Cluster)) +
      geom_boxplot(alpha = 0.6) +
      labs(title = paste("Distribution of", input$dist_feature, "Across Clusters"),
           y = "Value") +
      theme_minimal(base_size = 14)
  })
  
  # Cluster-wise Histogram
  output$cluster_histogram <- renderPlot({
    req(kmeans_result(), input$dist_feature)
    plot_data <- processed_data() %>%
      mutate(Cluster = factor(kmeans_result()$cluster))
    
    ggplot(plot_data, aes(x = .data[[input$dist_feature]], fill = Cluster)) +
      geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
      labs(title = paste("Histogram of", input$dist_feature, "Across Clusters"),
           y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  # Cluster Summary Stats
  output$cluster_summary <- renderTable({
    req(kmeans_result(), input$dist_feature)
    plot_data <- processed_data() %>%
      mutate(Cluster = factor(kmeans_result()$cluster))
    
    plot_data %>%
      group_by(Cluster) %>%
      summarise(
        Min = min(.data[[input$dist_feature]], na.rm = TRUE),
        Max = max(.data[[input$dist_feature]], na.rm = TRUE),
        Mean = round(mean(.data[[input$dist_feature]], na.rm = TRUE), 2),
        Median = round(median(.data[[input$dist_feature]], na.rm = TRUE), 2),
        SD = round(sd(.data[[input$dist_feature]], na.rm = TRUE), 2)
      )
  })
  
  # Features vs Clusters Table
  output$features_vs_clusters_table <- renderTable({
    req(kmeans_result())
    df <- processed_data()
    
    # Add cluster information to the data
    df$Cluster <- as.factor(kmeans_result()$cluster)
    
    # Gather numeric columns only for statistical summary
    numeric_features <- df %>% select(where(is.numeric), -Cluster) %>% colnames()
    
    # Generate the summary: Count, Min, Max, Mean, SD for each feature per cluster
    features_vs_clusters <- df %>%
      group_by(Cluster) %>%
      summarise(across(all_of(numeric_features), list(
        Count = ~n(),
        Min = ~min(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE),
        Mean = ~mean(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE)
      ), .names = "{.col}_{.fn}")) %>%
      pivot_longer(cols = -Cluster, 
                   names_to = c("Feature", ".value"), 
                   names_pattern = "(.*)_(.*)") %>%
      arrange(as.numeric(Cluster), Feature)
    
    return(features_vs_clusters)
  })
  
# R& S
  output$reward_sacrifice_table <- renderUI({
    req(kmeans_result())
    df <- processed_data()
    df <- na.omit(df)
    df$Cluster <- as.factor(kmeans_result()$cluster)
    
    features <- input$features
    clusters <- sort(unique(df$Cluster))
    
    # Cluster counts
    cluster_counts <- df %>% count(Cluster)
    
    # Cluster-level summary
    cluster_summary <- df %>%
      group_by(Cluster) %>%
      summarise(across(all_of(features), ~ if (is.numeric(.)) mean(., na.rm = TRUE) else {
        uniq_vals <- unique(.)
        uniq_vals[which.max(tabulate(match(., uniq_vals)))]
      }))
    
    # Overall summary
    overall_summary <- df %>%
      summarise(across(all_of(features), ~ if (is.numeric(.)) mean(., na.rm = TRUE) else {
        uniq_vals <- unique(.)
        uniq_vals[which.max(tabulate(match(., uniq_vals)))]
      }))
    
    # Initialize lists
    reward_features <- c()
    sacrifice_features <- c()
    
    # Decide globally if feature is Reward or Sacrifice
    for (feat in features) {
      numeric_flag <- is.numeric(df[[feat]])
      cluster_vals <- cluster_summary[[feat]]
      overall_val <- overall_summary[[feat]]
      
      if (numeric_flag) {
        reward_score <- sum(pmax(cluster_vals - overall_val, 0))
        sacrifice_score <- sum(pmax(overall_val - cluster_vals, 0))
        if (reward_score > sacrifice_score) {
          reward_features <- c(reward_features, feat)
        } else if (sacrifice_score > 0) {
          sacrifice_features <- c(sacrifice_features, feat)
        }
      } else {
        diff_count <- sum(cluster_vals != overall_val)
        if (diff_count >= length(clusters) / 2) {
          reward_features <- c(reward_features, feat)
        } else {
          sacrifice_features <- c(sacrifice_features, feat)
        }
      }
    }
    
    # Prepare Reward Matrix
    reward_df <- data.frame(Feature = reward_features)
    for (clus in clusters) {
      reward_vals <- sapply(reward_features, function(feat) {
        cluster_summary %>% filter(Cluster == clus) %>% pull(feat)
      })
      reward_df[[paste0("Cluster_", clus)]] <- reward_vals
    }
    
    # Prepare Sacrifice Matrix
    sacrifice_df <- data.frame(Feature = sacrifice_features)
    for (clus in clusters) {
      sacrifice_vals <- sapply(sacrifice_features, function(feat) {
        cluster_summary %>% filter(Cluster == clus) %>% pull(feat)
      })
      sacrifice_df[[paste0("Cluster_", clus)]] <- sacrifice_vals
    }
    
    # Store data frames into reactiveValues for table rendering
    output$cluster_counts <- renderTable({ cluster_counts }, na = "")
    output$reward_matrix <- renderTable({ reward_df }, na = "")
    output$sacrifice_matrix <- renderTable({ sacrifice_df }, na = "")
    
    # Return the table outputs
    tagList(
      h4("Cluster Counts"),
      tableOutput("cluster_counts"),
      
      h4("Reward Matrix (Feature globally treated as REWARD)"),
      tableOutput("reward_matrix"),
      
      h4("Sacrifice Matrix (Feature globally treated as SACRIFICE)"),
      tableOutput("sacrifice_matrix")
    )
  })
  
  output$encoding_reference_table <- renderTable({
    data.frame(
      Column_Name = c("Family (Y/N)", "Family (Y/N)", "Age Group", "Age Group", "Age Group", "Age Group", "Age Group",
                      "Booking Channel", "Booking Channel", "Booking Channel", "Booking Channel", "Booking Channel",
                      "Spending Level", "Spending Level", "Spending Level", "Spending Level",
                      "Purpose of Travel", "Purpose of Travel", "Purpose of Travel", "Purpose of Travel", "Purpose of Travel", "Purpose of Travel", "Purpose of Travel",
                      "Membership Level", "Membership Level", "Membership Level", "Membership Level",
                      "Special Request", "Special Request", "Special Request", "Special Request", "Special Request", "Special Request",
                      "Asked for Deals/Discount", "Asked for Deals/Discount",
                      "Asked for change of room", "Asked for change of room",
                      "Accepted Tour Pack", "Accepted Tour Pack",
                      "Loyalty Points Used to pay", "Loyalty Points Used to pay"),
      
      Encoded_Value = c(1,2, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4, 1,2,3,4,5,6,7, 1,2,3,4, 1,2,3,4,5,6, 1,2, 1,2, 1,2, 1,2),
      
      Original_Category = c("N","Y", "26-35","18-25","36-45","46-60","60+",
                            "Consolidator Website","Online Travel Agency","Direct Website","Travel Agent","Corporate Booking",
                            "Luxury","Low","High","Medium",
                            "Leisure","Event","Religious","Business","Transit","Medical","Education",
                            "Gold","None","Silver","Platinum",
                            "Early Check-in","None","Airport Pickup","Late Check-out","Room Service","Extra Bed",
                            "N","Y",
                            "No","Yes",
                            "No","Yes",
                            "Y","N")
    )
  }, sanitize.text.function = identity)
  

  
  
output$download <- downloadHandler(
  filename = function() {
    paste0("Clustered_Data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(kmeans_result())
    df <- processed_data()[, input$features, drop = FALSE]
    df <- na.omit(df)
    df$Cluster <- kmeans_result()$cluster  # Add cluster labels to data
    write.csv(df, file, row.names = FALSE)
  }
)

  
  
}

shinyApp(ui, server)