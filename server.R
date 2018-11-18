### ---------------------------------------------------------------------------
### --- WDCM Wikipedia Semantics Dashboard
### --- Script: server.R, v. Beta 0.1
### --- Wikidata, WMDE
### ---------------------------------------------------------------------------

### --- Setup
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(ggplot2)
library(ggrepel)
library(scales)
library(visNetwork)
library(networkD3)
library(ape)
library(igraph)

# - to dataDir
setwd('/srv/shiny-server/WDCM_WikipediaSemanticsDashboard/data')

### --- Update string:
updateString <- readLines('wikipediaSemanticsUpdateString.txt')
updateString <- paste0(updateString, ' UTC')

### --- Constants
# - WDCM categories
lF <- list.files()
lF <- lF[grepl("_themeDescription", lF)]
wdcmCategories <- gsub("wdcmdo_", "", lF)
wdcmCategories <- gsub("_themeDescription.csv", "", wdcmCategories)
# - WDCM wikies
wdcmWikies <- read.csv("Wikipedia_wdcmSUsage_CategoryOverview.csv",
                       header = T,
                       check.names = F,
                       row.names = 1,
                       stringsAsFactors = F)
wdcmSelectedWikies <- rownames(wdcmWikies)
# - wiki x topics matrices
lF <- list.files()
lF <- lF[grepl("_wikitopic", lF)]
wikiTopic <- vector(mode = "list", length = length(lF))
for (i in 1:length(lF)) {
  wikiTopic[[i]] <- fread(lF[i], data.table = F)
  rownames(wikiTopic[[i]]) <- wikiTopic[[i]]$V1
  wikiTopic[[i]]$V1 <- NULL
}
names(wikiTopic) <- sapply(lF, function(x) {
  strsplit(x, split = "_")[[1]][1]
})


### --- Server (Session) Scope
### --------------------------------

### --- shinyServer
shinyServer(function(input, output, session) {
  
  
  ### --------------------------------------------------------------------
  ### --- TAB: Category View
  ### --------------------------------------------------------------------
  
  ### --- GENERAL: Update String
  output$updateString <- renderText({
    paste0('<p style="font-size:80%;"align="right"><b>', updateString, '</b></p>')
  })
  
  ### --- SELECT: update select 'selectCategory'
  updateSelectizeInput(session,
                       'selectCategory',
                       choices = wdcmCategories,
                       selected = wdcmCategories[1],
                       server = TRUE)
  
  ### ---  when user selects a category
  selectedCategory <- reactive({input$selectCategory})
  
  ### ------ 
  ### --- SubTAB: Theme Description
  ### ------ 
  
  ### --- reactive themeDescription
  themeDescription <- eventReactive(input$selectCategory,
                                    {
                                      lF <- list.files()
                                      lF <- lF[grepl("_themeDescription", lF)]
                                      tD <- fread(lF[which(grepl(input$selectCategory, lF))], 
                                                  header = T)
                                      tD$V1 <- NULL
                                      colnames(tD)[2] <- 'Distinctive Classes'
                                      colnames(tD)[3] <- 'Typical Items'
                                      tD$`Distinctive Classes` <- gsub('"+', '"', tD$`Distinctive Classes`)
                                      tD$`Typical Items` <- gsub('"+', '"', tD$`Typical Items`)
                                      return(tD)
                                    })
  ### --- output$themeDescription
  output$themeDescription_DT <- DT::renderDataTable({
    datatable(themeDescription(),
              escape = F,
              selection = 'none',
              options = list(
                pageLength = 20,
                dom = 't',
                ordering = F,
                width = '100%',
                columnDefs = list(list(className = 'dt-left', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### ------ 
  ### --- SubTAB: Themes/Items
  ### ------ 

  ### --- SELECT: update select 'selectTheme_Items'
  output$selectTheme_Items <-
    renderUI({
      if ((is.null(themeDescription())) | (length(themeDescription()) == 0)) {
        selectizeInput(inputId = 'selectTheme_Items',
                       label = "Select theme:",
                       choices = NULL,
                       selected = NULL)
      } else {
        cH <- themeDescription()$Theme
        selectizeInput(inputId = 'selectTheme_Items',
                       label = "Select theme:",
                       choices = cH,
                       selected = cH[1])
        }
    })
  
  ### --- OUTPUT output$themeDistributionItems_SelectedCategory
  output$themeItems_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionItems
  output$themeDistributionItems <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- list.files()
      lF <- lF[grepl('itemtopic.csv', lF)]
      plotFrame <- fread(lF[which(grepl(selectedCategory(), lF))])
      incProgress(0.33, detail = "Computing")
      
      plotFrame$V1 <- NULL
      plotFrame$eu_entity_id <- NULL
      colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))] <- 
        gsub("topic", "Theme ", colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))])
      plotFrame <- select(plotFrame, 
                          as.character(input$selectTheme_Items), 'eu_label')
      colnames(plotFrame)[1] <- 'Score'
      plotFrame <- arrange(plotFrame, desc(Score)) %>% 
        head(50)
      plotFrame$eu_label <- factor(plotFrame$eu_label, 
                                   levels = plotFrame$eu_label[order(plotFrame$Score)])
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot(plotFrame, aes(x = eu_label, y = Score, label = eu_label)) +
        geom_line(size = .25, color = "#4c8cff", group = 1) +
        geom_point(size = 1.5, color = "#4c8cff") +
        geom_point(size = 1, color = "white") +
        geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
        xlab("\nItem") + ylab("Item Weight (0-1)\n") +
        ggtitle("Top Items in Semantic Theme") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 12, vjust = -1)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_text(size = 12)) +
        theme(axis.title.y = element_text(size = 12)) +
        theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) + 
        theme(strip.text = element_text(hjust = 0.5, size = 13))
      return(g)
      incProgress(1, detail = "Done")
    })
    
  })
  
  ### ------ 
  ### --- SubTAB: Distribution: Items
  ### ------
  
  ### --- OUTPUT output$themeDistributionItems_SelectedCategory
  output$themeDistributionItems_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  
  ### --- OUTPUT output$themeDistributionItems_Full
  output$themeDistributionItems_Full <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
    
    lF <- list.files()
    lF <- lF[grepl('itemtopic.csv', lF)]
    plotFrame <- fread(lF[which(grepl(selectedCategory(), lF))])
    incProgress(0.33, detail = "Computing")
    
    plotFrame$V1 <- NULL
    plotFrame$eu_entity_id <- NULL
    plotFrame$eu_label <- NULL
    plotFrame <- plotFrame %>% 
      gather(key = "Theme", 
             value = "Score", 
             starts_with('topic'))
    plotFrame$Theme <- gsub("[[:alpha:]]+", "Theme ", plotFrame$Theme)
    plotFrame$Theme <- factor(plotFrame$Theme, 
                              levels = unique(plotFrame$Theme)[order(
                                as.numeric(
                                  str_extract(unique(plotFrame$Theme), "[[:digit:]]+")
                                )
                              )]
    )
    incProgress(0.66, detail = "Generating Chart")
    
    g <- ggplot(plotFrame, aes(x = Score)) + 
      geom_histogram(bins = 50, fill = "deepskyblue") +
      facet_wrap(~ Theme, scales = "free") +  
      ylab("Number of items\n") + xlab("\nItem Weight (0-1)") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      facet_wrap(~Theme, ncol = 2, scales = 'free') + 
      ggtitle("The Distribution of Item Weight") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, hjust = 1)) +
      theme(axis.text.y = element_text(size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) + 
      theme(strip.text = element_text(hjust = 0.5, size = 13))
    incProgress(1, detail = "Done")
    return(g)
    
    })
  })
  
  
  ### ------ 
  ### --- SubTAB: Theme/Projects
  ### ------ 
  
  ### --- SELECT: update select 'selectTheme_Project'
  output$selectTheme_Project <-
    renderUI({
      if ((is.null(themeDescription())) | (length(themeDescription()) == 0)) {
        selectizeInput(inputId = 'selectTheme_Project',
                       label = "Select theme:",
                       choices = NULL,
                       selected = NULL)
      } else {
        cH <- themeDescription()$Theme
        selectizeInput(inputId = 'selectTheme_Project',
                       label = "Select theme:",
                       choices = cH,
                       selected = cH[1])
      }
    })
  
  ### --- OUTPUT output$themeProjects_SelectedCategory
  output$themeProjects_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionProjects
  output$themeDistributionProjects <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
    
    lF <- list.files()
    lF <- lF[grepl('themeWiki.csv', lF)]
    plotFrame <- fread(lF[which(grepl(selectedCategory(), lF))])
    incProgress(0.33, detail = "Computing")
    
    plotFrame$V1 <- NULL
    plotFrame <- filter(plotFrame, 
                        Theme %in% as.character(input$selectTheme_Project)) %>% 
      arrange(desc(Score))
    plotFrame$Wikipedia <- factor(plotFrame$Wikipedia, 
                              levels = plotFrame$Wikipedia[order(-plotFrame$Score)])
    
    incProgress(0.66, detail = "Generating Chart")
    
    g <- ggplot(plotFrame, aes(x = Wikipedia, y = Score, label = Wikipedia)) + 
      geom_line(size = .25, color = "#4c8cff", group = 1) +
      geom_point(size = 1.5, color = "#4c8cff") +
      geom_point(size = 1, color = "white") +
      geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
      ylab("Project Weight (0-1)\n") + xlab("\nWikipedia") +
      scale_y_continuous(labels = comma) +
      ggtitle("Top Wikipedias in Semantic Theme") +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      theme(axis.text.y = element_text(size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) + 
      theme(strip.text = element_text(hjust = 0.5, size = 13))
    incProgress(1, detail = "Done")
    return(g)
    })
  })
  
  ### ------ 
  ### --- SubTAB: Distribution:Projects
  ### ------ 
  
  ### --- OUTPUT output$themeDistributionProjects_SelectedCategory
  output$themeDistributionProjects_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionProjects_Full
  output$themeDistributionProjects_Full <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
    lF <- list.files()
    lF <- lF[grepl('wikitopic.csv', lF)]
    plotFrame <- fread(lF[which(grepl(selectedCategory(), lF))])
    incProgress(0.33, detail = "Computing")
    
    plotFrame$V1 <- NULL
    plotFrame <- plotFrame %>% 
      gather(key = "Theme", 
             value = "Score", 
             starts_with('topic'))
    plotFrame$Theme <- gsub("[[:alpha:]]+", "Theme ", plotFrame$Theme)
    plotFrame$Theme <- factor(plotFrame$Theme, 
                              levels = unique(plotFrame$Theme)[order(
                                as.numeric(
                                  str_extract(unique(plotFrame$Theme), "[[:digit:]]+")
                                )
                              )]
    )
    incProgress(0.66, detail = "Generating Chart")
    
    g <- ggplot(plotFrame, aes(x = Score)) + 
      geom_histogram(bins = 50, fill = "deepskyblue") +
      facet_wrap(~ Theme, scales = "free") +  
      ylab("Number of Wikipedias\n") + xlab("\nProject Weight (0-1)") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      facet_wrap(~Theme, ncol = 2, scales = 'free') +
      ggtitle("The Distribution of Project Weight in Semantic Themes") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, hjust = 1)) +
      theme(axis.text.y = element_text(size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) + 
      theme(strip.text = element_text(hjust = 0.5, size = 13))
    incProgress(1, detail = "Done")
    return(g)
    })
  })
  
  ### ------ 
  ### --- SubTAB: Items:Graph
  ### ------ 
  
  ### --- OUTPUT output$itemsGraph_SelectedCategory
  output$itemsGraph_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$itemsGraph
  output$itemsGraph <- renderVisNetwork({
    nodes <- fread(paste0(input$selectCategory, 
                          "_itemNodes.csv"))
    nodes$color <- NULL
    nodes$shape <- 'square'
    edges <- fread(paste0(input$selectCategory, 
                          "_itemEdges.csv"))
    edges$width <- (edges$width - 2)/2
    visNetwork(nodes = nodes,
               edges = edges,
               width = "100%",
               height = "100%") %>%
      visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>% 
      visOptions(highlightNearest = TRUE, selectedBy = "label") %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })

  
  ### ------ 
  ### --- SubTAB: Projects:Graph
  ### ------ 
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$projectsGraph_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$projectsGraph
  output$projectsGraph <- renderVisNetwork({
    nodes <- fread(paste0(input$selectCategory, 
                          "_projectNodes.csv"))
    nodes$shape <- 'square'
    nodes$color <- NULL
    edges <- fread(paste0(input$selectCategory, 
                          "_projectEdges.csv"))
    edges$width <- (edges$width - 2)/2
    nodes$color <- NULL
    visNetwork(nodes = nodes,
               edges = edges,
               width = "100%",
               height = "100%") %>%
      visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>% 
      visOptions(highlightNearest = TRUE, selectedBy = "label") %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })

  ### ------ 
  ### --- SubTAB: Items:Hierarchy
  ### ------ 
  
  ### --- OUTPUT output$projectsHierarchy_SelectedCategory
  output$itemsHierarchy_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$output$itemsHierarchy
  output$itemsHierarchy <- renderPlot({
    cluster <- readRDS(paste0(input$selectCategory, 
                              "_itemClusters.Rds"))
    cluster$labels <- gsub("\\(Q[[:digit:]]+\\)", "", cluster$labels)
    plot(as.phylo(cluster), type = "radial", tip.color = 'black',
         direction = "leftwards",
         edge.color = "deepskyblue4", edge.width = 1.6,
         cex = .75,
         lab4ut = 'axial',
         label.offset = .05, 
         no.margin = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ------ 
  ### --- SubTAB: Projects:Hierarchy
  ### ------ 
  
  ### --- OUTPUT output$projectsHierarchy_SelectedCategory
  output$projectsHierarchy_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$projectsHierarchy
  output$projectsHierarchy <- renderPlot({
    cluster <- readRDS(paste0(input$selectCategory, 
                          "_projectClusters.Rds"))
    plot(as.phylo(cluster), type = "radial", tip.color = 'black',
         direction = "leftwards",
         edge.color = "deepskyblue4", edge.width = 1.6,
         cex = .95,
         lab4ut = 'axial',
         label.offset = .05, 
         no.margin = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  
  ### --------------------------------------------------------------------
  ### --- TAB: Wiki View
  ### --------------------------------------------------------------------
  
  ### ------ 
  ### --- SubTAB: Wiki
  ### ------
  
  ### --- SELECT: update select 'selectWiki'
  updateSelectizeInput(session,
                       'selectWiki',
                       choices = wdcmSelectedWikies,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### ---  when user selects a category
  selectedWiki <- reactive({input$selectWiki})
  
  ### --- OUTPUT output$wiki_CategoryDistribution
  output$wiki_CategoryDistribution <- renderPlot({
    pFrame <- t(wdcmWikies[rownames(wdcmWikies) %in% selectedWiki(), ])
    pFrame <- data.frame(category = rownames(pFrame), 
                         count = pFrame[, 1],
                         stringsAsFactors = F)
    pFrame$percent <- round(pFrame$count/sum(pFrame$count)*100, 2)
    pFrame$label <- paste0(pFrame$percent, "%")
    pFrame$category <- paste0(pFrame$category, " (", pFrame$label, ")")
    ggplot(pFrame, aes(x = "", 
                       y = percent, 
                       label = label, 
                       group = category, 
                       fill = category)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      ylab("") + xlab("") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() + 
      theme(legend.title = element_blank()) + 
      theme(legend.text =  element_text(size = 12)) + 
      theme(axis.text.x = element_blank()) + 
      theme(panel.grid = element_blank())
  })
  
  ### --- OUTPUT output$wiki_Neighbourhood
  output$wiki_Neighbourhood <- renderPlot({
    simData <- read.csv(paste0(selectedWiki(), "_localSimilarity.csv"), 
                        header = T, 
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    similarWikies <- names(sort(
      simData[rownames(simData) %in% selectedWiki(), ], 
      decreasing = F)[1:11])
    simData <- simData[rownames(simData) %in% similarWikies, similarWikies]
    simData$wiki <- rownames(simData)
    simData <- simData %>% 
      gather(key = neighbour, 
             value = similarity, 
             -wiki) %>% 
      filter(wiki != neighbour) %>% 
      group_by(wiki) %>% 
      arrange(wiki, similarity) %>% 
      top_n(-3)
    # - igraph
    simData <- data.frame(ougoing = simData$wiki,
                          incoming = simData$neighbour,
                          stringsAsFactors = F)
    simData <- graph.data.frame(simData, directed = T)
    # - plot w. {igraph}
    V(simData)$color <- ifelse(names(V(simData)) == selectedWiki(), "red", "white")
    plot(simData,
         edge.width = .75,
         edge.color = "grey40",
         edge.arrow.size = 0.35,
         vertex.size = 5,
         vertex.label.color = "black",
         vertex.label.font = 1,
         vertex.label.family = "sans",
         vertex.label.cex = 1,
         vertex.label.dist = .45,
         edge.curved = 0.5,
         margin = c(rep(0,4)))
  })
  
  ### --- OUTPUT output$wiki_CategoryProfiles
  output$wiki_CategoryProfiles <- renderPlot({
    simData <- read.csv(paste0(selectedWiki(), "_localSimilarity.csv"), 
                        header = T, 
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    similarWikies <- names(sort(
      simData[rownames(simData) %in% selectedWiki(), ], 
      decreasing = F)[1:11])
    pFrame <- wdcmWikies[rownames(wdcmWikies) %in% similarWikies, ]
    pFrame$wiki <- rownames(pFrame)
    pFrame <- pFrame %>% 
      gather(key = category,
             value = items,
             -wiki)
    pFrame$color <- ifelse(pFrame$wiki %in% selectedWiki(), "red", "grey40")
    pFrame$label <- character(length(pFrame$wiki))
    pFrame$label[pFrame$wiki %in% selectedWiki()] <- 
      pFrame$items[pFrame$wiki %in% selectedWiki()]
    ggplot(pFrame, aes(x = category, 
                       y = log10(items), 
                       color = wiki, 
                       group = wiki, 
                       label = label)) + 
      geom_path() + 
      geom_text_repel(size = 3, 
                      show.legend = F) + 
      scale_color_brewer(palette = "Spectral") +
      xlab("\nWDCM Semantic Category") + ylab("log(Item Usage)") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.text =  element_text(size = 12))
      
  })
  
  ### --- OUTPUT output$wiki_SimilarityProfile
  output$wiki_SimilarityProfile <- renderPlot({
    simData <- read.csv(paste0(selectedWiki(), "_localSimilarity.csv"), 
                        header = T, 
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    simData <- simData[rownames(simData) %in% selectedWiki(), ]
    simData <- simData[, !(colnames(simData) %in% selectedWiki())]
    simData <- data.frame(wiki = colnames(simData), 
                          similarity = as.numeric(simData[1, ]), 
                          stringsAsFactors = F)
    ggplot(simData, aes(x = similarity)) +
      geom_histogram(binwidth = .1,
                     colour = "white", 
                     fill = "deepskyblue", 
                     alpha = .5) + 
      xlab("\nSimilarity (0 - 1)") + ylab("Num. of Wikies") + 
      scale_x_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
      theme_minimal() + 
      theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.text =  element_text(size = 12))
    
  })
  
  ### ------ 
  ### --- SubTAB: Wiki:Similarity
  ### ------
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$wikiGraph_SelectedWiki <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected Wikipedia: </b>', input$selectWiki, '</p>')
  })
  
  # - output$wikiGraph
  output$wikiGraph <- renderVisNetwork({
    nodes <- fread(paste0(input$selectWiki, 
                          "_wikiSimNodes.csv"))
    nodes$V1 <- NULL
    nodes$color[nodes$color != "red"] <- "white" 
    edges <- fread(paste0(input$selectWiki, 
                          "_wikiSimEdges.csv"))
    edges$V1 <- NULL
    edges$width <- (edges$width - 2)/2
    edges$color <- "grey40"
    visNetwork(nodes = nodes,
               edges = edges,
               width = "100%",
               height = "100%") %>%
      visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>% 
      visOptions(highlightNearest = TRUE, selectedBy = "label") %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  
  ### ------ 
  ### --- SubTAB: Wiki:Topics
  ### ------
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$wiki_TopicProfile_SelectedWiki <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected Wikipedia: </b>', input$selectWiki, '</p>')
  })
  
  ### --- OUTPUT output$wiki_TopicProfile
  output$wiki_TopicProfile <- renderPlot({
    
    collectTopics <- lapply(wikiTopic, function(x) {
      x[which(rownames(x) %in% selectedWiki()), ]
    })
    collectTopics <- rbindlist(collectTopics, use.names = T, fill = T)
    collectTopics[is.na(collectTopics)] <- 0
    collectTopics$category <- names(wikiTopic)
    collectTopics <- collectTopics %>% 
      gather(key = Topic, 
             value = Probability, 
             -category)
    collectTopics$label <- sapply(collectTopics$Probability, function(x) {
      if (x == 0) {""
      } else {
          round(x, 3)
        }
    })
    collectTopics$Topic <- gsub("topic([[:digit:]]+)", "Theme \\1", collectTopics$Topic)
    collectTopics$Topic <- factor(collectTopics$Topic, 
                                  levels = paste0("Theme ", 
                                                  order(as.numeric(
                                                    unlist(
                                                      str_extract_all(
                                                        unique(collectTopics$Topic), 
                                                        "[[:digit:]]+")
                                                      )
                                                    )
                                                    )
                                                  )
                                  )
  
    # - visualize w. ggplot2
    ggplot(collectTopics,
           aes(x = Topic, 
               y = Probability, 
               label = label)
           ) +
      geom_line(color = "black", group = 1, size = .35) + 
      geom_point(size = 1.5, color = "black") + 
      geom_point(size = 1, color = "white") +
      geom_text_repel(size = 3) +
      facet_wrap(~ category, 
                 ncol = 3, 
                 scales = "free_y") +
      xlab('Topic') + ylab('Probability') +
      scale_y_continuous(labels = comma) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(strip.text = element_text(size = 13)) + 
      theme(panel.background = element_rect(color = "white", fill = "aliceblue"))
            
  })
  
  
  
  }) ### --- END shinyServer




 