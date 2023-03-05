library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)
library(survival)
library(survminer)
library(tidyverse)

my.env <- new.env()
autofill <- sort(readRDS("../ebvgcr-data/autofill-miRNA.rds"))
optionsCompare <- c(2, 3, 4)
gc.survival <- readRDS("../ebvgcr-data/EBVaGC_surv-imm.rds")
colnames(gc.survival) <- gsub("\\.", "-", colnames(gc.survival))
gc.survival <- gc.survival[c(1,3,60,61,62,63)]

ui <- fluidPage(title="EBV-GCR - miRNA vs. OS", theme = shinytheme("cosmo"), useShinyjs(),
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  titlePanel("EBV-GCR - Correlation of Cellular miRNA Expression and Overal Survival by EBV Status"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between expression of a candidate miRNA and patient overall survival (OS) between Epstein-Barr virus (EBV) positive, microsatellite-unstable/instability (MSI), chromosomal instability (CIN), genomically stable (GS), and mutant polymerase epsilon (POLE) tumors in gastric carcinoma (GC). Select the candidate miRNA by typing the name in the input box on the top right. Patients can be stratified into 2, 3 or 4 comparison groups based on expression levels of the target miRNA using the adjacent input box. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Kaplan-Meier plots can be downloaded as PNG or PDF files. Survival data was extracted from Liu et al, Cell 2018",a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066282/", "DOI: 10.1016/j.cell.2018.02.052")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v4", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout( sidebarPanel(
  fluidRow(column(8, align="left", offset = 0, selectizeInput(
    inputId = 'search', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
    choices = NULL, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
    column(4, align="left", offset = 0, selectizeInput(
       inputId = 'compare', label = HTML('<br/>', 'Select # of Comparison Groups'),
       choices = optionsCompare, options = list(openOnFocus = FALSE), multiple = FALSE))), width=6, dataTableOutput(outputId = 'table'),
  dataTableOutput(outputId = 'tablePQ'), tags$div(HTML('<br/>')),
  fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadPQ', 'Download Data'),
                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>'))
  ), 
  mainPanel(
    jqui_resizable(plotOutput(outputId = "survCINgc", height=500)),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload1P', 'Download GC CIN Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload1V', 'Download GC CIN Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
    jqui_resizable(plotOutput(outputId = "survEBVgc", height=500)), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload2P', 'Download GC EBV Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload2V', 'Download GC EBV Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), 
    jqui_resizable(plotOutput(outputId = "survMSIgc", height=500)), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload3P', 'Download GC MSI Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload3V', 'Download GC MSI Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
    jqui_resizable(plotOutput(outputId = "survGSgc", height=500)), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload4P', 'Download GC GS Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload4V', 'Download GC GS Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
    jqui_resizable(plotOutput(outputId = "survPOLEgc", height=500)), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload5P', 'Download GC POLE Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownload5V', 'Download GC POLE Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'search', choices = autofill, server = TRUE, selected = character(0))
  
  output$downloadPQ <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-pq.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePQ, filename, row.names=FALSE)
  })
  
  output$gcdownload1P <- downloadHandler(
    filename = function() {paste("GC-CIN-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot1[[1]], width = 3*input$survCINgc_size$width, height = 3*input$survCINgc_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownload2P <- downloadHandler(
    filename = function() {paste("GC-EBV-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot2[[1]], width = 3*input$survEBVgc_size$width, height = 3*input$survEBVgc_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownload3P <- downloadHandler(
    filename = function() {paste("GC-MSI-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot3[[1]], width = 3*input$survMSIgc_size$width, height = 3*input$survMSIgc_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownload4P <- downloadHandler(
    filename = function() {paste("GC-GS-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot4[[1]], width = 3*input$survGSgc_size$width, height = 3*input$survGSgc_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownload5P <- downloadHandler(
    filename = function() {paste("GC-POLE-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot5[[1]], width = 3*input$survPOLEgc_size$width, height = 3*input$survPOLEgc_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownload1V <- downloadHandler(
    filename = function() {paste("GC-CIN-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot1[[1]], width = 3*input$survCINgc_size$width, height = 3*input$survCINgc_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$gcdownload2V <- downloadHandler(
    filename = function() {paste("GC-EBV-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot2[[1]], width = 3*input$survEBVgc_size$width, height = 3*input$survEBVgc_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$gcdownload3V <- downloadHandler(
    filename = function() {paste("GC-MSI-Neg-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot3[[1]], width = 3*input$survMSIgc_size$width, height = 3*input$survMSIgc_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$gcdownload4V <- downloadHandler(
    filename = function() {paste("GC-GS-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot4[[1]], width = 3*input$survGSgc_size$width, height = 3*input$survGSgc_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$gcdownload5V <- downloadHandler(
    filename = function() {paste("GC-POLE-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot5[[1]], width = 3*input$survPOLEgc_size$width, height = 3*input$survPOLEgc_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$tablePQ <- renderDT({
    hide("downloadPQ")
    req(input$search, input$compare)
    show("downloadPQ")
    gene <- input$search
    compare <- as.numeric(input$compare)
    assign("g.gene", gene, envir = my.env)
    if (compare == 2){
      temp <- readRDS("pq-2group-miRNA.rds")
    } else if (compare == 3){
      temp <- readRDS("pq-3group-miRNA.rds")
    } else if (compare == 4){
      temp <- readRDS("pq-4group-miRNA.rds")
    }
    data=temp[paste("hsa-",gene,sep=""),]
    gc.plot.data <- makePlotData(gene, "GC")
    percent1 <- 100*colSums(na.omit(gc.plot.data)[1] == 0)/nrow(na.omit(gc.plot.data))
    truthy1=TRUE
    if (percent1 > 50){truthy1=FALSE}
    pval=c()
    for (i in seq(2,ncol(data),2)){
      q <- as.numeric(data[i])
      if (is.na(q) | q > 0.1 | truthy1==FALSE){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      pval <- c(pval, p.text)
    }
    if (compare == 2){
      names <- c("GC CIN High vs. Low", "GC EBV High vs. Low", "GC MSI High vs. Low","GC GS High vs. Low", "GC POLE High vs. Low")
      pvals <- c(data[1], data[3], data[5], data[7], data[9])
      qvals <- c(data[2], data[4], data[6], data[8], data[10])
    } else if (compare == 3){
      names <- c("GC CIN Mid vs. Low", "GC CIN High vs. Low", "GC CIN High vs. Mid", "GC EBV Mid vs. Low", "GC EBV High vs. Low", "GC EBV High vs. Mid", "GC MSI Mid vs. Low", "GC MSI High vs. Low", "GC MSI High vs. Mid", "GC GS Mid vs. Low", "GC GS High vs. Low", "GC GS High vs. Mid", "GC POLE Mid vs. Low", "GC POLE High vs. Low", "GC POLE High vs. Mid")
      pvals <- c(data[1], data[3], data[5], data[7], data[9], data[11], data[13], data[15], data[17], data[19], data[21], data[23], data[25], data[27], data[29])
      qvals <- c(data[2], data[4], data[6], data[8], data[10], data[12], data[14], data[16], data[18], data[20], data[22], data[24], data[26], data[28], data[30])
    } else if (compare == 4){
      names <- c("GC CIN Mid-Low vs. Low", "GC CIN High-Mid vs. Low", "GC CIN High vs. Low", "GC CIN High-Mid vs. Mid-Low", "GC CIN High vs. Mid-Low", "GC CIN High vs. Mid-High", "GC EBV Mid-Low vs. Low", "GC EBV High-Mid vs. Low", "GC EBV High vs. Low", "GC EBV High-Mid vs. Mid-Low", "GC EBV High vs. Mid-Low", "GC EBV High vs. Mid-High", "GC MSI Mid-Low vs. Low", "GC MSI High-Mid vs. Low", "GC MSI High vs. Low", "GC MSI High-Mid vs. Mid-Low", "GC MSI High vs. Mid-Low", "GC MSI High vs. Mid-High", "GC GS Mid-Low vs. Low", "GC GS High-Mid vs. Low", "GC GS High vs. Low", "GC GS High-Mid vs. Mid-Low", "GC GS High vs. Mid-Low", "GC GS High vs. Mid-High", "GC POLE Mid-Low vs. Low", "GC POLE High-Mid vs. Low", "GC POLE High vs. Low", "GC POLE High-Mid vs. Mid-Low", "GC POLE High vs. Mid-Low", "GC POLE High vs. Mid-High")
      pvals <- c(data[1], data[3], data[5], data[7], data[9], data[11], data[13], data[15], data[17], data[19], data[21], data[23], data[25], data[27], data[29], data[31], data[33], data[35], data[37], data[39], data[41], data[43], data[45], data[47], data[49], data[51], data[53], data[55], data[57], data[59])
      qvals <- c(data[2], data[4], data[6], data[8], data[10], data[12], data[14], data[16], data[18], data[20], data[22], data[24], data[26], data[28], data[30], data[32], data[34], data[36], data[38], data[40], data[42], data[44], data[46], data[48], data[50], data[52], data[54], data[56], data[58], data[60])
    }
    tablePQ <- data.frame(cbind(names, pvals, qvals, pval))
    tablePQ=data.frame(lapply(tablePQ, as.character), stringsAsFactors=FALSE)
    colnames(tablePQ) <- c("Comparison Group", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.tablePQ", tablePQ, envir = my.env)
    datatable(tablePQ, options=list(scrollX=TRUE, bFilter=0, paginate=FALSE, bLengthChange=0, bInfo=0, autoWidth=FALSE, columnDefs=list(list(className = 'dt-center', targets = 0:3))), rownames=FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(2, 3), digits=3)
  })
  
  drawPlot <- function(plot.data, gene, titles, mode, compare.G) {
    if (mode == "CIN"){
      CIN.data <- plot.data[which(plot.data$Status == "CIN"), ]
      plot.data.mut <- CIN.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    } else if (mode == "EBV"){
      EBV.data <- plot.data[which(plot.data$Status == "EBV"), ]
      plot.data.mut <- EBV.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    } else if (mode == "MSI"){
      MSI.data <- plot.data[which(plot.data$Status == "MSI"), ]
      plot.data.mut <- MSI.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    } else if (mode == "GS"){
      GS.data <- plot.data[which(plot.data$Status == "GS"), ]
      plot.data.mut <- GS.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    } else if (mode == "POLE"){
      POLE.data <- plot.data[which(plot.data$Status == "POLE"), ]
      plot.data.mut <- POLE.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    }
    surv.plot.data <- survival::survfit(formula = survival::Surv(time = OS.Time/30, event = OS) ~ quantile, data = plot.data.mut, conf.type = "log-log")
    if (as.numeric(compare.G) == 2){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("hsa-","",gene), sep=" "), paste("HIGH", gsub("hsa-","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"))
    } else if (as.numeric(compare.G) == 3){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("hsa-","",gene), sep=" "), paste("MID", gsub("hsa-","",gene), sep=" "), paste("HIGH", gsub("hsa-","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"),)
    } else if (as.numeric(compare.G) == 4){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("hsa-","",gene), sep=" "), paste("MID-LOW", gsub("hsa-","",gene), sep=" "), paste("HIGH-MID", gsub("hsa-","",gene), sep=" "), paste("HIGH", gsub("hsa-","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"))
    }
    return(my.plot)
  }
  
  output$survCINgc <- renderPlot({
    hide("gcdownload1P")
    hide("gcdownload2P")
    hide("gcdownload3P")
    hide("gcdownload4P")
    hide("gcdownload5P")
    hide("gcdownload1V")
    hide("gcdownload2V")
    hide("gcdownload3V")
    hide("gcdownload4V")
    hide("gcdownload5V")
    req(input$search, input$compare)
    show("gcdownload1P")
    show("gcdownload2P")
    show("gcdownload3P")
    show("gcdownload4P")
    show("gcdownload5P")
    show("gcdownload1V")
    show("gcdownload2V")
    show("gcdownload3V")
    show("gcdownload4V")
    show("gcdownload5V")
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, "GC Dataset - CIN", "CIN", input$compare)
    assign("g.gc.plot1", gc.plot, envir = my.env)
    gc.plot
  })
  
  output$survEBVgc <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, "GC Dataset - EBV", "EBV", input$compare)
    assign("g.gc.plot2", gc.plot, envir = my.env)
    gc.plot
  })
  
  output$survMSIgc <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, "GC Dataset - MSI", "MSI", input$compare)
    assign("g.gc.plot3", gc.plot, envir = my.env)
    gc.plot
  })
  
  output$survGSgc <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, "GC Dataset - GS", "GS", input$compare)
    assign("g.gc.plot4", gc.plot, envir = my.env)
    gc.plot
  })
  
  output$survPOLEgc <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, "GC Dataset - POLE", "POLE", input$compare)
    assign("g.gc.plot5", gc.plot, envir = my.env)
    gc.plot
  })
  
  makePlotData <- function(gene, mode) {
    if (mode == "GC"){
      gc.mRNA <- readRDS(paste("../ebvgcr-data/mirna-genes/GC-", gene, ".rds", sep=""))
      gc.combined <- merge(gc.mRNA, gc.survival, by="Sample")
      assign("g.gc.combined", gc.combined, envir = my.env)
      CIN <- data.frame(gc.combined[gc.combined$Status == "STAD_CIN" , c(gene,"OS", "OS-Time")])
      EBV <- data.frame(gc.combined[gc.combined$Status == "STAD_EBV" , c(gene,"OS", "OS-Time")])
      MSI <- data.frame(gc.combined[gc.combined$Status == "STAD_MSI" , c(gene,"OS", "OS-Time")])
      GS <- data.frame(gc.combined[gc.combined$Status == "STAD_GS" , c(gene,"OS", "OS-Time")])
      POLE <- data.frame(gc.combined[gc.combined$Status == "STAD_POLE" , c(gene,"OS", "OS-Time")])
    }
    CIN['Status'] = "CIN"
    EBV['Status'] = "EBV"
    MSI['Status'] = "MSI"
    GS['Status'] = "GS"
    POLE['Status'] = "POLE"
    names(CIN)[1] = "Value"
    names(EBV)[1] = "Value"
    names(MSI)[1] = "Value"
    names(GS)[1] = "Value"
    names(POLE)[1] = "Value"
    plot.data <- rbind(CIN, EBV, MSI, GS, POLE)
    return(plot.data)
  } 
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui = ui, server = server)
