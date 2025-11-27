# app.R
# Application Shiny — Dashboard SAE (ACP / CAH / Cartes / Descriptives / Séries)
# Pré-requis : monde, monde_data, mCL (ou on calcule mCL dans l'app), emissions_co2, surface_fores, serie_CO2_long, serie_FOR_long
# Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(leaflet)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)
library(GGally)    # for ggpairs
library(tidyr)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "SAE — Climat & Développement"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("Accueil", tabName="home", icon=icon("home")),
                menuItem("Descriptives", tabName="desc", icon=icon("table")),
                menuItem("Bivariées", tabName="bivar", icon=icon("project-diagram")),
                menuItem("Cartes", tabName="maps", icon=icon("globe")),
                menuItem("ACP", tabName="acp", icon=icon("chart-line")),
                menuItem("CAH", tabName="cah", icon=icon("layer-group")),
                menuItem("Séries temporelles", tabName="time", icon=icon("chart-area")),
                menuItem("Données", tabName="data", icon=icon("database"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      /* petits styles pour rendre plus propre */
      .box-title { font-weight: 700; }
      .small-note { font-size:12px; color: #666; }
    "))),
    tabItems(
      # HOME
      tabItem(tabName="home",
              fluidRow(
                box(width=8, status="primary", title="Projet — Contexte & objectif",
                    p("Objectif : caractériser le développement des pays et leur exposition / vulnérabilité face aux risques climatiques."),
                    tags$ul(
                      tags$li("Indicateurs : démographie, économie, santé, énergie, environnement (CO₂, précipitations, surface forestière), catastrophes."),
                      tags$li("Méthodes : statistiques descriptives, ACP, classification (CAH), analyses temporelles, cartographie interactive.")
                    ),
                    p(class="small-note", "Remarque : ggpairs et certaines cartes peuvent être un peu longues à calculer pour tous les pays")
                ),
                box(width=4, status="info", title="Raccourcis utiles",
                    actionButton("go_desc","Aller aux descriptives"),
                    br(), br(),
                    actionButton("go_acp","Aller à l'ACP"),
                    br(), br(),
                    actionButton("go_maps","Aller aux cartes")
                )
              )
      ),
      
      # DESCRIPTIVES univariées
      tabItem(tabName="desc",
              fluidRow(
                box(width=6, title="Tableau résumé (compact)", status="primary",
                    DTOutput("stats_table")
                ),
                box(width=6, title="Histogramme simple", status="warning",
                    selectInput("hist_var", "Choisir un indicateur", choices = NULL),
                    plotOutput("hist_plot", height=300)
                )
              ),
              fluidRow(
                box(width=6, title="Boxplots standardisés (tous indicateurs)", status="info",
                    checkboxInput("show_points_box", "Afficher labels min/max", value = TRUE),
                    plotOutput("boxplots", height=420)
                ),
                box(width=6, title="Comparaison pays (barplot)", status="info",
                    selectizeInput("bar_countries", "Pays (plusieurs)", choices = NULL, multiple = TRUE),
                    selectInput("bar_var","Indicateur", choices = NULL),
                    plotOutput("bar_plot", height=420)
                )
              )
      ),
      
      # BIVARIEES (ggpairs + scatter sélection)
      tabItem(tabName="bivar",
              fluidRow(
                box(width=12, title="Matrice bivariée (ggpairs)", status="primary",
                    plotOutput("ggpairs_plot", height=800))
              ),
              fluidRow(
                box(width=6, title="Nuage 2 variables (label pays)", status="info",
                    selectInput("x_var","X", choices = NULL),
                    selectInput("y_var","Y", choices = NULL),
                    plotOutput("scatter_xy", height=400)),
                box(width=6, title="Exemples prêts à l'emploi", status="warning",
                    p("Espérance vie vs Accès à l'électricité"),
                    plotOutput("scatter_example", height=400))
              )
      ),
      
      # MAPS
      tabItem(tabName="maps",
              fluidRow(
                box(width=4, title="Options carte", status="primary",
                    selectInput("map_indicator", "Indicateur", choices = NULL),
                    sliderInput("map_radius", "Taille des points", min=3, max=12, value=6),
                    checkboxInput("map_log", "Échelle log (si pertinent)", value = FALSE)
                ),
                box(width=8, title="Carte points (circle markers)", status="primary",
                    leafletOutput("map_leaf", height=600))
              )
      ),
      
      # ACP
      tabItem(tabName="acp",
              fluidRow(
                box(width=6, title="Cercle des corrélations (axes 1 & 2)", status="primary",
                    plotOutput("pca_var_plot", height=420)),
                box(width=6, title="Nuage des individus (Axes 1 & 2)", status="primary",
                    sliderInput("pca_topk", "Nombre de pays étiquetés (top K)", min=5, max=50, value=20),
                    plotOutput("pca_ind_plot", height=420))
              ),
              fluidRow(
                box(width=12, title="Valeurs propres (rapide)", status="info",
                    verbatimTextOutput("pca_eig")
                )
              )
      ),
      
      # CAH
      tabItem(tabName="cah",
              fluidRow(
                box(width=6, title="Dendrogramme coloré", status="primary",
                    numericInput("k_classes", "Nombre de classes k", value=5, min=2, max=8),
                    actionButton("recalc_cah","Recalcule CAH"),
                    plotOutput("dendro_plot", height=420)
                ),
                box(width=6, title="Carte des classes (points colorés)", status="primary",
                    leafletOutput("cah_map", height=420)
                )
              ),
              fluidRow(
                box(width=12, title="Moyennes par classe", status="info",
                    DTOutput("class_means")
                )
              )
      ),
      
      # TIME SERIES
      tabItem(tabName="time",
              fluidRow(
                box(width=4, title="Sélection séries", status="primary",
                    selectizeInput("time_countries", "Pays (séries)", choices = NULL, multiple = TRUE, selected = c("France","United States","China")),
                    checkboxInput("show_CO2", "Tracer CO2", TRUE),
                    checkboxInput("show_forest", "Tracer surface forestière", TRUE)
                ),
                box(width=8, title="Séries temporelles", status="primary",
                    plotOutput("time_series_plot", height=520))
              )
      ),
      
      # DATA
      tabItem(tabName="data",
              fluidRow(
                box(width=12, title="Table complète des données", status="primary",
                    DTOutput("full_table"))
              )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # ---- Setup: vérifier que objets existent ----
  # On suppose que 'monde_data' (sf) et 'data_t' (data.frame propre) et 'mCL' ou 'cah5' peuvent être calculés ici.
  # Si ces objets ne sont pas présents, l'app va planter : tu dois exécuter la préparation des données avant d'ouvrir l'app.
  if (!exists("monde_data") || !exists("data_t")) {
    stop("Les objets 'monde_data' et 'data_t' doivent exister dans l'environnement avant de lancer l'app.\n
         Ex : charger data_t (table finale) et monde_data (fusion monde + data_t).")
  }
  
  # --- Indicateurs numériques (pour selectors) ---
  data_num <- data_t %>% select(where(is.numeric))
  numeric_vars <- colnames(data_num)
  
  # Mettre à jour menus dynamiquement
  updateSelectInput(session, "hist_var", choices = numeric_vars, selected = numeric_vars[1])
  updateSelectInput(session, "map_indicator", choices = numeric_vars, selected = "electricite")
  updateSelectInput(session, "bar_var", choices = numeric_vars, selected = numeric_vars[which(colnames(data_t)=="emissions_co2")])
  updateSelectInput(session, "x_var", choices = numeric_vars, selected = "electricite")
  updateSelectInput(session, "y_var", choices = numeric_vars, selected = "esperance_vie")
  updateSelectizeInput(session, "bar_countries", choices = data_t$pays)
  updateSelectizeInput(session, "time_countries", choices = data_t$pays, selected = c("France","United States","China"))
  updateSelectizeInput(session, "go_desc", choices = NULL) # placeholder
  
  # ---- Stats table (compact) ----
  stats_desc <- reactive({
    df <- data_num
    res <- data.frame(
      Variable = colnames(df),
      Moyenne = sapply(df, mean, na.rm=TRUE),
      Mediane = sapply(df, median, na.rm=TRUE),
      SD = sapply(df, sd, na.rm=TRUE),
      Min = sapply(df, min, na.rm=TRUE),
      Max = sapply(df, max, na.rm=TRUE)
    )
    res
  })
  output$stats_table <- renderDT({
    datatable(round(stats_desc(),3), options=list(pageLength=10, dom='t'), rownames=FALSE)
  })
  
  # ---- Histogramme ----
  output$hist_plot <- renderPlot({
    v <- req(input$hist_var)
    ggplot(data_t, aes_string(x=v)) +
      geom_histogram(bins=30, fill="steelblue", color="white") +
      theme_minimal() + labs(x=v, y="Effectif")
  })
  
  # ---- Boxplots standardisés ----
  output$boxplots <- renderPlot({
    dfs <- as.data.frame(scale(data_num))
    dfm <- tidyr::pivot_longer(cbind(df=1:nrow(dfs), dfs), cols = -df, names_to = "variable", values_to = "value")
    p <- ggplot(dfm, aes(x = variable, y = value)) +
      geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
      geom_jitter(width = 0.15, alpha = 0.5, size=1) +
      coord_flip() + theme_minimal() + labs(y="Valeur standardisée", x="")
    if (input$show_points_box) {
      # add labels for min/max per variable (approx)
      mins <- aggregate(value ~ variable, data = dfm, FUN = min)
      maxs <- aggregate(value ~ variable, data = dfm, FUN = max)
      p
    } else p
  })
  
  # ---- Bar plot comparatif pays ----
  output$bar_plot <- renderPlot({
    sel <- input$bar_countries
    var <- input$bar_var
    req(var)
    if (is.null(sel) || length(sel)==0) {
      # show top 10 by var
      df <- data_t %>% arrange(desc(.data[[var]])) %>% slice_head(n=10)
    } else {
      df <- data_t %>% filter(pays %in% sel) %>% arrange(desc(.data[[var]]))
    }
    ggplot(df, aes(x = reorder(pays, .data[[var]]), y = .data[[var]])) +
      geom_col(fill="steelblue") + coord_flip() + theme_minimal() +
      labs(x="", y=var, title=paste("Comparaison —", var))
  })
  
  # ---- ggpairs (bivariée) ----
  output$ggpairs_plot <- renderPlot({
    # ATTENTION : ggpairs peut être lourd : on prend un sous-ensemble si > 10 variables
    df <- data_num
    if (ncol(df) > 9) df_for_plot <- df[, 1:9] else df_for_plot <- df
    GGally::ggpairs(df_for_plot,
                    diag=list(continuous = "densityDiag"),
                    upper = list(continuous = wrap("cor", method="spearman")),
                    lower = list(continuous = wrap("points", alpha=0.4, size=0.8)))
  }, height = function(){ if(ncol(data_num)>9) 900 else 700 })
  
  # ---- scatter XY ----
  output$scatter_xy <- renderPlot({
    x <- req(input$x_var); y <- req(input$y_var)
    df <- data_t %>% select(pays, all_of(c(x,y))) %>% na.omit()
    ggplot(df, aes_string(x=x, y=y, label="pays")) +
      geom_point() + geom_smooth(method="lm", se=FALSE, color="darkred") +
      geom_text_repel(size=2.5, max.overlaps = 20) + theme_minimal()
  })
  
  output$scatter_example <- renderPlot({
    ggplot(data_t, aes(x=electricite, y=esperance_vie, label=pays)) +
      geom_point() + geom_smooth(method="lm", se=FALSE, color="darkgreen") +
      geom_text_repel(size=2.8) + theme_minimal() +
      labs(title="Espérance de vie vs Accès à l'électricité")
  })
  
  # ---- Leaflet cartes (points, palette, légende) ----
  output$map_leaf <- renderLeaflet({
    req(input$map_indicator)
    ind <- input$map_indicator
    df <- monde_data %>% filter(!is.na(.data[[ind]]))
    vals <- df[[ind]]
    pal <- colorNumeric("Blues", domain = if (input$map_log) log1p(vals) else vals)
    leaflet(df) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~LON, ~LAT,
                       radius = input$map_radius,
                       color = ~pal(if (input$map_log) log1p(get(ind)) else get(ind)),
                       fillOpacity = 0.9,
                       stroke = FALSE,
                       label = ~paste0(NAME, " : ", round(get(ind),2)),
                       popup = ~paste0("<b>", NAME, "</b><br>", ind, " : ", round(get(ind),2))
      ) %>%
      addLegend("bottomright", pal = pal, values = if (input$map_log) log1p(vals) else vals,
                title = ind)
  })
  
  # ---- PCA / ACP (préparation et affichage) ----
  pca_objs <- reactive({
    # construire dACP comme dans ton Rmd : choisir colonnes numériques dans monde_data (les colonnes indicateurs)
    tmp <- as.data.frame(monde_data)[, 13:23]   # ATTENTION : indices selon ta table ; adapte si nécessaire
    # ne garder que numériques (protection)
    is_num <- sapply(tmp, is.numeric)
    tmp <- tmp[, is_num, drop=FALSE]
    keep <- complete.cases(tmp)
    dACP <- tmp[keep, , drop=FALSE]
    # rownames = ISO3 des lignes gardées
    iso_full <- as.character(monde_data$ISO3)
    iso_codes <- iso_full[keep]
    rownames(dACP) <- iso_codes
    acp <- PCA(dACP, graph = FALSE)
    list(acp=acp, dACP=dACP)
  })
  
  output$pca_eig <- renderPrint({
    acp <- pca_objs()$acp
    print(acp$eig)
  })
  
  output$pca_var_plot <- renderPlot({
    acp <- pca_objs()$acp
    # Cercle des corrélations pour axes 1 & 2
    fviz_pca_var(acp, axes = c(1,2), repel=TRUE)
  })
  
  output$pca_ind_plot <- renderPlot({
    acp <- pca_objs()$acp
    coords <- as.data.frame(acp$ind$coord[,1:2])
    coords$iso <- rownames(coords)
    # compute contribution sum to pick topk
    contrib_sum <- rowSums(acp$ind$contrib[,1:2, drop=FALSE])
    coords$contrib <- contrib_sum
    k <- input$pca_topk
    idx <- order(coords$contrib, decreasing = TRUE)[1:min(k, nrow(coords))]
    coords$label <- ""
    coords$label[idx] <- coords$iso[idx]
    ggplot(coords, aes(x = Dim.1, y = Dim.2)) +
      geom_point(alpha=0.6) +
      geom_text_repel(aes(label=label), size=3, max.overlaps = 30) +
      theme_minimal() + labs(x="Dim 1", y="Dim 2", title="Nuage des individus (ACP)")
  })
  
  # ---- CAH (clustering) ----
  cah_objs <- reactiveVal(NULL)
  observe({
    acp <- pca_objs()$acp
    cp <- acp$ind$coord[,1:4, drop=FALSE]
    cah <- hclust(dist(cp), method="ward.D2")
    k <- 5
    cls <- cutree(cah, k)
    cah_objs(list(cah=cah, cls=cls))
  })
  
  observeEvent(input$recalc_cah, {
    acp <- pca_objs()$acp
    cp <- acp$ind$coord[,1:4, drop=FALSE]
    cah <- hclust(dist(cp), method="ward.D2")
    k <- input$k_classes
    cls <- cutree(cah, k)
    cah_objs(list(cah=cah, cls=cls))
  })
  
  output$dendro_plot <- renderPlot({
    req(cah_objs())
    fviz_dend(cah_objs()$cah, k = input$k_classes, show_labels = FALSE,
              k_colors = "jco", rect = TRUE, rect_border = "jco", rect_fill = TRUE,
              main = paste("Dendrogramme (k =", input$k_classes, ")"))
  })
  
  output$cah_map <- renderLeaflet({
    req(cah_objs())
    cls <- cah_objs()$cls
    df_cls <- data.frame(ISO = names(cls), class = as.factor(cls), stringsAsFactors = FALSE)
    # merge with monde_data by ISO3
    map_df <- merge(as.data.frame(monde_data), df_cls, by.x="ISO3", by.y="ISO", all.x=FALSE, sort=FALSE)
    pal <- colorFactor("Set1", domain = map_df$class)
    leaflet(map_df) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~LON, ~LAT, color = ~pal(class), radius = 6, fillOpacity = 0.9,
                       popup = ~paste0(NAME, "<br>Classe: ", class)) %>%
      addLegend("bottomright", pal = pal, values = ~class, title = "Classe CAH")
  })
  
  output$class_means <- renderDT({
    req(cah_objs())
    cls <- cah_objs()$cls
    df_cls <- data.frame(ISO = names(cls), class = cls, stringsAsFactors = FALSE)
    merged <- merge(data.frame(ISO = monde_data$ISO3, data_t), df_cls, by="ISO")
    mpar <- merged %>% group_by(class) %>% summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE)), n=n())
    datatable(round(as.data.frame(mpar),3))
  })
  
  # ---- TIME SERIES (CO2 & Forest) ----
  # We expect serie_CO2_long and serie_FOR_long exist in the environment
  output$time_series_plot <- renderPlot({
    sel <- input$time_countries
    p <- ggplot()
    if (input$show_CO2 && exists("serie_CO2_long")) {
      dfc <- serie_CO2_long %>% filter(Pays %in% sel)
      p <- p + geom_line(data=dfc, aes(x=annee, y=Emissions_CO2, color=Pays), size=1)
    }
    if (input$show_forest && exists("serie_FOR_long")) {
      dff <- serie_FOR_long %>% filter(Pays %in% sel)
      p <- p + geom_line(data=dff, aes(x=annee, y=Surface_Forestiere, color=Pays), linetype="dashed", size=1)
    }
    p + theme_minimal() + labs(x="Année", y="Valeur", title="Séries temporelles sélectionnées")
  })
  
  # ---- Tables ----
  output$full_table <- renderDT({
    datatable(monde_data, options = list(pageLength=20, scrollX=TRUE))
  })
  
  output$full_table2 <- renderDT({
    datatable(data_t, options = list(pageLength=20))
  })
  
  # ---- navigation shortcuts ----
  observeEvent(input$go_desc, { updateTabItems(session, "tabs", "desc") })
  observeEvent(input$go_acp,  { updateTabItems(session, "tabs", "acp") })
  observeEvent(input$go_maps, { updateTabItems(session, "tabs", "maps") })
}

# --- run app ---
shinyApp(ui, server)
