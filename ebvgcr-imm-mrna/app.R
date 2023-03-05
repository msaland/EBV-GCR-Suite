library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

my.env <- new.env()
autofill1 <- sort(readRDS("../ebvgcr-data/autofill-mRNA.rds"))
gc.imm <- readRDS("../ebvgcr-data/EBVaGC_surv-imm.rds")
autofill2 <- sort(gsub("\\.", " ", colnames(gc.imm[c(4:6,8:54,57:59)])))
colnames(gc.imm) <- gsub("\\.", "-", colnames(gc.imm))
descript <- readRDS("../ebvgcr-data/description-imm.rds")

ui <- fluidPage(title = "EBV-GCR - mRNA vs. Immune Function", theme = shinytheme("cosmo"), useShinyjs(),
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  titlePanel("EBV-GCR - Correlation of Cellular mRNA Expression and Immune Function by EBV Status"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between candidate cellular mRNA expression and various calculated characteristics related to the tumor immune landscape in Epstein-Barr virus (EBV) positive, microsatellite-unstable/instability (MSI), chromosomal instability (CIN), genomically stable (GS), and mutant polymerase epsilon (POLE) tumors in the TCGA gastric carcinoma (GC) cohort. Select the candidate cellular mRNAs by typing the name in the input box on the top right. Select the immune function/factor to analyze by selecting the desired option in the adjacent dropdown. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Correlation plots can be downloaded as PNG or PDF files and the information necessary to reproduce the box and scatter plots can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA GC cohort was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"),". Immune landscape data was extracted from Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023")),
  p(style="text-align: justify;", strong("Immune Landscape Features: "), "A master list of all 53 immune landscape features, put together according to Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023"), "can be found", a(href="description-imm.txt", "here"), "."),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v2", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout( sidebarPanel(width=6, 
      fluidRow(column(7, selectizeInput(inputId = 'searchGene', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
                                        choices = NULL, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
               column(5, selectizeInput(inputId = 'searchFactor', label = HTML('<br/>', 'Factor Selection'),
                                        choices = autofill2, selected = character(0), options = list(placeholder = 'Please type in the factor or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE))),
        dataTableOutput(outputId = 'tableFactor'),
        fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadFactorCSV', 'Download Factor Data'),
                        tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>')),
        tags$div(id="p1", tags$strong("P-values for GC Boxplot Data:")), dataTableOutput(outputId = "tableBox"), tags$div(HTML('<br/>')),
        dataTableOutput(outputId = 'table'), tags$div(HTML('<br/>')),
        fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadCSV', 'Download Data'),
           tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
    mainPanel(
      tags$div(id="d", tags$strong("Description:")), htmlOutput(outputId = "description"), tags$div(HTML('<br/>')),
      jqui_resizable(plotOutput(outputId = "scatterPlotGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadScatterP', 'Download GC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadScatterV', 'Download GC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "boxPlotGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadBoxP', 'Download GC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadBoxV', 'Download GC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'searchGene', choices = autofill1, server = TRUE, selected = character(0))
  
  output$downloadCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-", my.env$g.factor, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.table, filename, row.names=FALSE)
  })
  
  output$downloadFactorCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-all_factors.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tableFactor, filename, row.names=FALSE)
  })
  
  output$gcdownloadScatterP <- downloadHandler(
    filename = function() {paste("GC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plotScatter, width = 5*input$scatterPlotGC_size$width, height = 5*input$scatterPlotGC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownloadScatterV <- downloadHandler(
    filename = function() {paste("GC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plotScatter, width = 5*input$scatterPlotGC_size$width, height = 5*input$scatterPlotGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$gcdownloadBoxP <- downloadHandler(
    filename = function() {paste("GC-box-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plotBox, width = 5*input$boxPlotGC_size$width, height = 5*input$boxPlotGC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownloadBoxV <- downloadHandler(
    filename = function() {paste("GC-box-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plotBox, width = 5*input$boxPlotGC_size$width, height = 5*input$boxPlotGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$table <- renderDT({
    hide("gcdownloadScatterP")
    hide("gcdownloadScatterV")
    hide("gcdownloadBoxP")
    hide("gcdownloadBoxV")
    hide("downloadCSV")
    hide("downloadFactorCSV")
    hide("p1")
    hide("d")
    req(input$searchGene, input$searchFactor)
    show("gcdownloadScatterP")
    show("gcdownloadScatterV")
    show("gcdownloadBoxP")
    show("gcdownloadBoxV")
    show("downloadCSV")
    show("downloadFactorCSV")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    assign("g.factor", factor, envir = my.env)
    gc.mRNA <- readRDS(paste("../ebvgcr-data/mrna-genes/GC-", gene, ".rds", sep=""))
    gc.combined <- merge(gc.mRNA, gc.imm, by="Sample")
    assign("g.gc.combined", gc.combined, envir = my.env)
    
    gc.CIN.gene <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_CIN", gene]))
    names(gc.CIN.gene)[1] = "Value"
    gc.CIN.gene.summary <- c(paste("GC CIN -", gsub('(.*)-\\w+','\\1',gene), sep=" "), as.numeric(count(gc.CIN.gene)$n), round(as.numeric(min(gc.CIN.gene)), digits = 3), round(as.numeric(max(gc.CIN.gene)), digits = 3), round(mean(unlist(gc.CIN.gene)), digits = 3), round(as.numeric(summarize_all(gc.CIN.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.EBV.gene <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_EBV", gene]))
    names(gc.EBV.gene)[1] = "Value"
    gc.EBV.gene.summary <- c(paste("GC EBV -", gsub('(.*)-\\w+','\\1',gene), sep=" "), as.numeric(count(gc.EBV.gene)$n), round(as.numeric(min(gc.EBV.gene)), digits = 3), round(as.numeric(max(gc.EBV.gene)), digits = 3), round(mean(unlist(gc.EBV.gene)), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.MSI.gene <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_MSI", gene]))
    names(gc.MSI.gene)[1] = "Value"
    gc.MSI.gene.summary <- c(paste("GC MSI -", gsub('(.*)-\\w+','\\1',gene), sep=" "), as.numeric(count(gc.MSI.gene)$n), round(as.numeric(min(gc.MSI.gene)), digits = 3), round(as.numeric(max(gc.MSI.gene)), digits = 3), round(mean(unlist(gc.MSI.gene)), digits = 3), round(as.numeric(summarize_all(gc.MSI.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.GS.gene <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_GS", gene]))
    names(gc.GS.gene)[1] = "Value"
    gc.GS.gene.summary <- c(paste("GC GS -", gsub('(.*)-\\w+','\\1',gene), sep=" "), as.numeric(count(gc.GS.gene)$n), round(as.numeric(min(gc.GS.gene)), digits = 3), round(as.numeric(max(gc.GS.gene)), digits = 3), round(mean(unlist(gc.GS.gene)), digits = 3), round(as.numeric(summarize_all(gc.GS.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.POLE.gene <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_POLE", gene]))
    names(gc.POLE.gene)[1] = "Value"
    gc.POLE.gene.summary <- c(paste("GC POLE -", gsub('(.*)-\\w+','\\1',gene), sep=" "), as.numeric(count(gc.POLE.gene)$n), round(as.numeric(min(gc.POLE.gene)), digits = 3), round(as.numeric(max(gc.POLE.gene)), digits = 3), round(mean(unlist(gc.POLE.gene)), digits = 3), round(as.numeric(summarize_all(gc.POLE.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.CIN.factor <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_CIN", factor]))
    names(gc.CIN.factor)[1] = "Value"
    gc.CIN.factor.summary <- c(paste("GC CIN -", gsub("-"," ",factor), sep=" "), as.numeric(count(gc.CIN.factor)$n), round(as.numeric(min(gc.CIN.factor)), digits = 3), round(as.numeric(max(gc.CIN.factor)), digits = 3), round(mean(unlist(gc.CIN.factor)), digits = 3), round(as.numeric(summarize_all(gc.CIN.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.EBV.factor <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_EBV", factor]))
    names(gc.EBV.factor)[1] = "Value"
    gc.EBV.factor.summary <- c(paste("GC EBV -", gsub("-"," ",factor), sep=" "), as.numeric(count(gc.EBV.factor)$n), round(as.numeric(min(gc.EBV.factor)), digits = 3), round(as.numeric(max(gc.EBV.factor)), digits = 3), round(mean(unlist(gc.EBV.factor)), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.MSI.factor <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_MSI", factor]))
    names(gc.MSI.factor)[1] = "Value"
    gc.MSI.factor.summary <- c(paste("GC MSI -", gsub("-"," ",factor), sep=" "), as.numeric(count(gc.MSI.factor)$n), round(as.numeric(min(gc.MSI.factor)), digits = 3), round(as.numeric(max(gc.MSI.factor)), digits = 3), round(mean(unlist(gc.MSI.factor)), digits = 3), round(as.numeric(summarize_all(gc.MSI.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.GS.factor <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_GS", factor]))
    names(gc.GS.factor)[1] = "Value"
    gc.GS.factor.summary <- c(paste("GC GS -", gsub("-"," ",factor), sep=" "), as.numeric(count(gc.GS.factor)$n), round(as.numeric(min(gc.GS.factor)), digits = 3), round(as.numeric(max(gc.GS.factor)), digits = 3), round(mean(unlist(gc.GS.factor)), digits = 3), round(as.numeric(summarize_all(gc.GS.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.POLE.factor <- data.frame(na.omit(gc.combined[gc.combined$Status == "STAD_POLE", factor]))
    names(gc.POLE.factor)[1] = "Value"
    gc.POLE.factor.summary <- c(paste("GC POLE -", gsub("-"," ",factor), sep=" "), as.numeric(count(gc.POLE.factor)$n), round(as.numeric(min(gc.POLE.factor)), digits = 3), round(as.numeric(max(gc.POLE.factor)), digits = 3), round(mean(unlist(gc.POLE.factor)), digits = 3), round(as.numeric(summarize_all(gc.POLE.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    
    table <- rbind(gc.CIN.gene.summary, gc.EBV.gene.summary, gc.MSI.gene.summary, gc.GS.gene.summary, gc.POLE.gene.summary, gc.CIN.factor.summary, gc.EBV.factor.summary, gc.MSI.factor.summary, gc.GS.factor.summary, gc.POLE.factor.summary)
    colnames(table) <- c("Dataset", "# of Patients", "Min", "Max", "Mean", "1st Quartile", "Median", "3rd Quartile")
    row.names(table) <- c(paste("GC CIN -", gsub('(.*)-\\w+','\\1',gene), sep=" "), paste("GC EBV -", gsub('(.*)-\\w+','\\1',gene), sep=" "), paste("GC MSI -", gsub('(.*)-\\w+','\\1',gene), sep=" "), paste("GC GS -", gsub('(.*)-\\w+','\\1',gene), sep=" "), paste("GC POLE -", gsub('(.*)-\\w+','\\1',gene), sep=" "), paste("GC CIN -", gsub("-"," ",factor), sep=" "), paste("GC EBV -", gsub("-"," ",factor), sep=" "), paste("GC MSI -", gsub("-"," ",factor), sep=" "), paste("GC GS -", gsub("-"," ",factor), sep=" "), paste("GC POLE -", gsub("-"," ",factor), sep=" "))
    assign("g.table", table, envir = my.env)
    table
  }, options=list(scrollX=TRUE, bFilter=0, bPaginate=0, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:7))), rownames= FALSE)
  
  output$tableFactor <- renderDT({
    hide("gcdownloadScatterP")
    hide("gcdownloadScatterV")
    hide("gcdownloadBoxP")
    hide("gcdownloadBoxV")
    hide("downloadCSV")
    hide("downloadFactorCSV")
    hide("p1")
    hide("d")
    req(input$searchGene)
    show("downloadFactorCSV")
    gene <- input$searchGene
    gc.mRNA <- readRDS(paste("../ebvgcr-data/mrna-genes/GC-", gene, ".rds", sep=""))
    gc.combined <- merge(gc.mRNA, gc.imm, by="Sample")
    assign("g.gc.combined", gc.combined, envir = my.env)
    show("downloadFactorCSV")
    listFactor <- sort(gsub("\\.", " ", colnames(gc.imm[c(4:6,8:54,57:59)])))
    assign("g.gene", gene, envir = my.env)
    tablePR <- readRDS(paste("../ebvgcr-data/imm-stats-mrna/", gene, ".rds", sep=""))
    pval.CIN = c()
    pval.EBV = c()
    pval.MSI = c()
    pval.GS = c()
    pval.POLE = c()
    for (i in 1:length(listFactor)){
      gc.plot.data <- makePlotDataScatter(gene, listFactor[i], "GC")
      percent1 <- 100*colSums(na.omit(gc.plot.data[1]) == 0)/nrow(gc.plot.data)
      truthy1=TRUE
      truthy2=TRUE
      if (percent1 > 50){truthy1=FALSE}
      q.CIN <- as.numeric(tablePR[i,4])
      q.EBV <- as.numeric(tablePR[i,7])
      q.MSI <- as.numeric(tablePR[i,10])
      q.GS <- as.numeric(tablePR[i,13])
      q.POLE <- as.numeric(tablePR[i,16])
      if (is.na(q.CIN) | q.CIN > 0.1 | truthy1==FALSE){
        p.CIN.text <- "NO"
      } else {
        p.CIN.text <- "YES"
      } 
      if (is.na(q.EBV) | q.EBV > 0.1 | truthy1==FALSE){
        p.EBV.text <- "NO"
      } else {
        p.EBV.text <- "YES"
      } 
      if (is.na(q.MSI) | q.MSI > 0.1 | truthy1==FALSE){
        p.MSI.text <- "NO"
      } else {
        p.MSI.text <- "YES"
      } 
      if (is.na(q.GS) | q.GS > 0.1 | truthy1==FALSE){
        p.GS.text <- "NO"
      } else {
        p.GS.text <- "YES"
      }
      if (is.na(q.POLE) | q.POLE > 0.1 | truthy1==FALSE){
        p.POLE.text <- "NO"
      } else {
        p.POLE.text <- "YES"
      }
      pval.CIN = c(pval.CIN, p.CIN.text)
      pval.EBV = c(pval.EBV, p.EBV.text)
      pval.MSI = c(pval.MSI, p.MSI.text)
      pval.GS = c(pval.GS, p.GS.text)
      pval.POLE = c(pval.POLE, p.POLE.text)
    }
    tablePRN = data.frame(tablePR[1], tablePR[2], tablePR[3], tablePR[4], pval.CIN, tablePR[5], tablePR[6], tablePR[7], pval.EBV, tablePR[8], tablePR[9], tablePR[10], pval.MSI, tablePR[11], tablePR[12], tablePR[13], pval.GS, tablePR[14], tablePR[15], tablePR[16], pval.POLE)
    colnames(tablePRN) <- c("Comparison Group", "GC CIN Spearman ρ", "GC CIN p-Val.", "GC CIN q-Val. (FDR=0.1)", "GC CIN Signif.", "GC EBV Spearman ρ", "GC EBV p-Val.", "GC EBV q-Val. (FDR=0.1)", "GC EBV Signif.", "GC MSI Spearman ρ", "GC MSI p-Val.", "GC MSI q-Val. (FDR=0.1)", "GC MSI Signif.", "GC GS Spearman ρ", "GC GS p-Val.", "GC GS q-Val. (FDR=0.1)", "GC GS Signif.", "GC POLE Spearman ρ", "GC POLE p-Val.", "GC POLE q-Val. (FDR=0.1)", "GC POLE Signif.")
    assign("g.tableFactor", tablePRN, envir = my.env)
    datatable(tablePRN, options=list(scrollX=TRUE, bFilter=0, pageLength = 10, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:20))), rownames= FALSE) %>% formatSignif(columns=c(2,3,4,6,7,8,10,11,12,14,15,16,18,19,20), digits=3) %>%  formatStyle(columns = c('GC CIN Signif.', 'GC EBV Signif.', 'GC MSI Signif.', 'GC GS Signif.','GC POLE Signif.'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212')))
  })
  
  drawPlotScatter <- function(plot.data, gene, factor, titles) {
    my.plot <- ggplot(data = plot.data, aes(x=ValueFactor, y=ValueGene, color=Status)) + 
      geom_point() + 
      xlab(paste(gsub("-"," ",factor), sep="")) +
      ylab(paste("mRNA expression levels of", gsub('(.*)-\\w+','\\1',gene), sep=" ")) +
      ggtitle(titles) +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_color_manual(labels = c("CIN", "EBV", "GS", "MSI", "POLE"), values = c("red", "blue", "darkgreen", "purple", "black")) +
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
      stat_cor(method = "spearman") +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=10, color='black'), axis.title.x = element_text(size=10, color='black'))
    return(my.plot)
  }
  
  drawPlotBox <- function(transform.data, factor, titles) {
    my.plot <- ggplot(transform.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("CIN", "EBV", "MSI", "GS", "POLE"), name="EBV Status") +
      ylab(paste("Measured levels of", gsub("-"," ",factor), sep=" ")) +
      ggtitle(titles) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    return(my.plot)
  }
  
  output$scatterPlotGC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    show("gcdownloadBoxP")
    show("gcdownloadBoxV")
    show("gcdownloadScatterP")
    show("gcdownloadScatterV")
    show("downloadCSV")
    show("p1")
    show("p2")
    show("d")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataScatter(gene, factor, "GC")
    gc.plotScatter <- drawPlotScatter(plot.data, gene, factor, "GC Dataset - Immune Function Correlations")
    assign("g.gc.plotScatter", gc.plotScatter, envir = my.env)
    gc.plotScatter
  })
  
  output$boxPlotGC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "GC")
    CIN=unlist(unname(plot.data[plot.data[2]=="CIN",][1]))
    EBV=unlist(unname(plot.data[plot.data[2]=="EBV",][1]))
    MSI=unlist(unname(plot.data[plot.data[2]=="MSI",][1]))
    GS=unlist(unname(plot.data[plot.data[2]=="GS",][1]))
    POLE=unlist(unname(plot.data[plot.data[2]=="POLE",][1]))
    transform.CIN=data.frame(signif(max(CIN, na.rm=TRUE),3), signif(min(CIN, na.rm=TRUE),3), signif(median(CIN, na.rm=TRUE),3), quantile(CIN, c(0.25,0.75), na.rm=TRUE)[1], quantile(CIN, c(0.25,0.75), na.rm=TRUE)[2], "CIN")
    colnames(transform.CIN)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.EBV=data.frame(signif(max(EBV, na.rm=TRUE),3), signif(min(EBV, na.rm=TRUE),3), signif(median(EBV, na.rm=TRUE),3), quantile(EBV, c(0.25,0.75), na.rm=TRUE)[1], quantile(EBV, c(0.25,0.75), na.rm=TRUE)[2], "EBV")
    colnames(transform.EBV)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.MSI=data.frame(signif(max(MSI, na.rm=TRUE),3), signif(min(MSI, na.rm=TRUE),3), signif(median(MSI, na.rm=TRUE),3), quantile(MSI, c(0.25,0.75), na.rm=TRUE)[1], quantile(MSI, c(0.25,0.75), na.rm=TRUE)[2], "MSI")
    colnames(transform.MSI)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.GS=data.frame(signif(max(GS, na.rm=TRUE),3), signif(min(GS, na.rm=TRUE),3), signif(median(GS, na.rm=TRUE),3), quantile(GS, c(0.25,0.75), na.rm=TRUE)[1], quantile(GS, c(0.25,0.75), na.rm=TRUE)[2], "GS")
    colnames(transform.GS)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.POLE=data.frame(signif(max(POLE, na.rm=TRUE),3), signif(min(POLE, na.rm=TRUE),3), signif(median(POLE, na.rm=TRUE),3), quantile(POLE, c(0.25,0.75), na.rm=TRUE)[1], quantile(POLE, c(0.25,0.75), na.rm=TRUE)[2], "POLE")
    colnames(transform.POLE)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.data=rbind(transform.CIN, transform.EBV, transform.MSI, transform.GS, transform.POLE)
    transform.data$Status <- factor(transform.data$Status , levels=c("CIN", "EBV", "MSI", "GS", "POLE"))
    gc.plotBox <- drawPlotBox(transform.data, factor, "GC Dataset - Immune Function")
    assign("g.gc.plotBox", gc.plotBox, envir = my.env)
    gc.plotBox
  })
  
  makePlotDataScatter <- function(gene, factor, mode) {
    if (mode == "GC"){
      gc.combined <- my.env$g.gc.combined
      CIN <- data.frame(gc.combined[gc.combined$Status == "STAD_CIN", c(gene,factor)])
      EBV <- data.frame(gc.combined[gc.combined$Status == "STAD_EBV", c(gene,factor)])
      MSI <- data.frame(gc.combined[gc.combined$Status == "STAD_MSI", c(gene,factor)])
      GS <- data.frame(gc.combined[gc.combined$Status == "STAD_GS", c(gene,factor)])
      POLE <- data.frame(gc.combined[gc.combined$Status == "STAD_POLE", c(gene,factor)])
    } 
    CIN['Status'] = "CIN"
    names(CIN)[1] = "ValueGene"
    names(CIN)[2] = "ValueFactor"
    EBV['Status'] = "EBV"
    names(EBV)[1] = "ValueGene"
    names(EBV)[2] = "ValueFactor"
    MSI['Status'] = "MSI"
    names(MSI)[1] = "ValueGene"
    names(MSI)[2] = "ValueFactor"
    GS['Status'] = "GS"
    names(GS)[1] = "ValueGene"
    names(GS)[2] = "ValueFactor"
    POLE['Status'] = "POLE"
    names(POLE)[1] = "ValueGene"
    names(POLE)[2] = "ValueFactor"
    plot.data <- rbind(CIN, EBV, MSI, GS, POLE)
    return(plot.data)
  } 
  
  makePlotDataBox <- function(gene, factor, mode) {
    if (mode == "GC"){
      gc.combined <- my.env$g.gc.combined
      CIN <- data.frame(gc.combined[gc.combined$Status == "STAD_CIN", factor])
      EBV <- data.frame(gc.combined[gc.combined$Status == "STAD_EBV", factor])
      MSI <- data.frame(gc.combined[gc.combined$Status == "STAD_MSI", factor])
      GS <- data.frame(gc.combined[gc.combined$Status == "STAD_GS", factor])
      POLE <- data.frame(gc.combined[gc.combined$Status == "STAD_POLE", factor])
    }
    CIN['Status'] = "CIN"
    names(CIN)[1] = "ValueFactor"
    EBV['Status'] = "EBV"
    names(EBV)[1] = "ValueFactor"
    MSI['Status'] = "MSI"
    names(MSI)[1] = "ValueFactor"
    GS['Status'] = "GS"
    names(GS)[1] = "ValueFactor"
    POLE['Status'] = "POLE"
    names(POLE)[1] = "ValueFactor"
    plot.data <- rbind(CIN, EBV, MSI, GS, POLE)
    return(plot.data)
  } 

  output$tableBox <- renderDT({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "GC")
    sub1 <- data.frame(na.omit(plot.data[plot.data$Status== "CIN", "ValueFactor"]))
    sub2 <- data.frame(na.omit(plot.data[plot.data$Status== "EBV", "ValueFactor"]))
    sub3 <- data.frame(na.omit(plot.data[plot.data$Status== "MSI", "ValueFactor"]))
    sub4 <- data.frame(na.omit(plot.data[plot.data$Status== "GS", "ValueFactor"]))
    sub5 <- data.frame(na.omit(plot.data[plot.data$Status== "EBV", "ValueFactor"]))
    percent1 <- 100*colSums(sub1[1] == 0)/nrow(sub1)
    percent2 <- 100*colSums(sub2[1] == 0)/nrow(sub2)
    percent3 <- 100*colSums(sub3[1] == 0)/nrow(sub3)
    percent4 <- 100*colSums(sub4[1] == 0)/nrow(sub4)
    percent5 <- 100*colSums(sub5[1] == 0)/nrow(sub5)
    truthy1=TRUE
    truthy2=TRUE
    truthy3=TRUE
    truthy4=TRUE
    truthy5=TRUE
    truthy6=TRUE
    truthy7=TRUE
    truthy8=TRUE
    truthy9=TRUE
    truthy10=TRUE
    if (percent1 > 50 | percent2 > 50){truthy1=FALSE}
    if (percent1 > 50 | percent3 > 50){truthy2=FALSE}
    if (percent1 > 50 | percent4 > 50){truthy3=FALSE}
    if (percent1 > 50 | percent5 > 50){truthy4=FALSE}
    if (percent2 > 50 | percent3 > 50){truthy5=FALSE}
    if (percent2 > 50 | percent4 > 50){truthy6=FALSE}
    if (percent2 > 50 | percent5 > 50){truthy7=FALSE}
    if (percent3 > 50 | percent4 > 50){truthy8=FALSE}
    if (percent3 > 50 | percent5 > 50){truthy9=FALSE}
    if (percent4 > 50 | percent5 > 50){truthy10=FALSE}
    p.values <- compare_means(ValueFactor ~ Status,  data = plot.data)$p.adj
    names=c("CIN vs. EBV", "CIN vs. MSI", "CIN vs. GS", "CIN vs. POLE", "EBV vs. MSI", "EBV vs. GS", "EBV vs. POLE", "MSI vs. GS", "MSI vs. POLE", "GS vs. POLE")
    signif=c()
    for (i in p.values){
      if(is.na(i) | i >= 0.05){
        signif=c(signif, "n.s.")
      } else if (i < 0.0001){
        signif=c(signif, "****")
      } else if (i < 0.001){
        signif=c(signif, "***")
      } else if (i < 0.01){
        signif=c(signif, "**")
      } else if (i < 0.05){
        signif=c(signif, "*")
      }
    }
    table=data.frame(names, p.values, signif)
    colnames(table)=c("Comparison Group","p-Value","Significance")
    datatable(table, options=list(scrollX=TRUE, bFilter=0, bPaginate=0, pageLength = 10, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:2))), rownames= FALSE) %>% formatSignif(columns=c(2), digits=3) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('n.s.','*','**','***','****'), c('#000000', '#540606', '#820909', '#b50b0b','#e61212')))
  })
  
  output$description <- renderText({
    req(input$searchFactor)
    show("d")
    factor <- input$searchFactor
    HTML(unlist(descript[descript[1]==factor,][2]))
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)