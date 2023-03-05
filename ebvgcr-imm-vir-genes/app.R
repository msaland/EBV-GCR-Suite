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
gc.EBV.mRNA <- readRDS("../ebvgcr-data/EBVaGC_vir_mRNA.rds")
colnames(gc.EBV.mRNA) <- gsub("\\.", "-", colnames(gc.EBV.mRNA))
EBV.miRNA <- sort(colnames(gc.EBV.mRNA[3:ncol(gc.EBV.mRNA)]))
gc.EBV.miRNA <- readRDS("../ebvgcr-data/EBVaGC_vir_miRNA.rds")
colnames(gc.EBV.miRNA) <- gsub("\\.", "-", colnames(gc.EBV.miRNA))
colnames(gc.EBV.miRNA) <- gsub("ebv-", "", colnames(gc.EBV.miRNA))
EBV.mRNA <- sort(gsub("ebv-","",colnames(gc.EBV.miRNA)[3:ncol(gc.EBV.miRNA)]))
autofill1 <- sort(c(EBV.mRNA, EBV.miRNA))
gc.imm <- readRDS("../ebvgcr-data/EBVaGC_surv-imm.rds")
autofill2 <- sort(gsub("\\.", " ", colnames(gc.imm[c(4:6,8:54,57:59)])))
colnames(gc.imm) <- gsub("\\.", "-", colnames(gc.imm))
descript <- readRDS("../ebvgcr-data/description-imm.rds")
gc.combined1 <- merge(gc.imm, gc.EBV.mRNA, by="Sample")
assign("g.gc.combined1", gc.combined1, envir = my.env)
gc.combined2 <- merge(gc.imm, gc.EBV.miRNA, by="Sample")
assign("g.gc.combined2", gc.combined2, envir = my.env)


ui <- fluidPage(title = "EBV-GCR - EBV mRNA/miRNA vs. Immune Function", theme = shinytheme("cosmo"), useShinyjs(),
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  titlePanel("EBV-GCR - Correlation of Viral mRNA/miRNA Expression and Immune Function"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between candidate EBV mRNA and miRNA expression and various calculated characteristics related to the tumor immune landscape in Epstein-Barr virus (EBV) positive tumors in the TCGA gastric carcinoma (GC) cohort. Select the candidate EBV mRNA/miRNAs by typing the name in the input box on the top right. Select the immune function/factor to analyze by selecting the desired option in the adjacent dropdown. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Correlation plots can be downloaded as PNG or PDF files and the information necessary to reproduce the scatter plots can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA GC cohort was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". EBV viral mRNA expression data were sourced from Chakravorty et al, Cancer Research 2019 (Integrated Pan-Cancer Map of EBV-Associated Neoplasms Reveals Functional Host-Virus Interactions) ",a(href="https://pubmed.ncbi.nlm.nih.gov/31481499/", "DOI: 10.1158/0008-5472.CAN-19-0615"),". EBV viral miRNA expression data were sourced from Ungerleider et al, PLOS Pathogens 2021 (EBV miRNAs are potent effectors of tumor cell transcriptome remodeling in promoting immune escape) ",a(href="https://pubmed.ncbi.nlm.nih.gov/33956915/", "DOI: 10.1371/journal.ppat.1009217"),". Immune landscape data was extracted from Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023"), "."),
  p(style="text-align: justify;", strong("Immune Landscape Features: "), "A master list of all 53 immune landscape features, put together according to Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023"), "can be found", a(href="description-imm.txt", "here"), "."),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v2", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout( sidebarPanel(width=6, 
      fluidRow(column(7, selectizeInput(inputId = 'searchGene', label = HTML('Type the mRNA/miRNA Name'),
                                        choices = NULL, selected = character(0), options = list(placeholder = 'Please type in the mRNA/miRNA name or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
               column(5, selectizeInput(inputId = 'searchFactor', label = HTML('Factor Selection'),
                                        choices = autofill2, selected = character(0), options = list(placeholder = 'Please type in the factor or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE))),
        dataTableOutput(outputId = 'tableFactor'),
        fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadFactorCSV', 'Download Factor Data'),
                        tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>')),
      dataTableOutput(outputId = 'table'), tags$div(HTML('<br/>')),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadCSV', 'Download Data'),
                      tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
    mainPanel(
      tags$div(id="d", tags$strong("Description:")), htmlOutput(outputId = "description"), tags$div(HTML('<br/>')),
      jqui_resizable(plotOutput(outputId = "scatterPlotGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadScatterP', 'Download GC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadScatterV', 'Download GC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
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
  
  output$table <- renderDT({
    hide("gcdownloadScatterP")
    hide("gcdownloadScatterV")
    hide("downloadFactorCSV")
    hide("downloadCSV")
    hide("d")
    req(input$searchGene, input$searchFactor)
    show("gcdownloadScatterP")
    show("gcdownloadScatterV")
    show("downloadCSV")
    show("downloadFactorCSV")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    assign("g.factor", factor, envir = my.env)
    if (grepl("miR", gene, fixed = TRUE)){
      gc.EBV.gene <- data.frame(na.omit(gc.combined2[gc.combined2$Status == "EBV", gene]))
      gc.EBV.factor <- data.frame(na.omit(gc.combined2[gc.combined2$Status == "EBV", factor]))
    } else {
      gc.EBV.gene <- data.frame(na.omit(gc.combined1[gc.combined1$Status == "EBV", gene]))
      gc.EBV.factor <- data.frame(na.omit(gc.combined1[gc.combined1$Status == "EBV", factor]))
    } 
    
    names(gc.EBV.gene)[1] = "Value"
    names(gc.EBV.factor)[1] = "Value"
    gc.EBV.gene.summary <- c(paste("GC EBV -", gene, sep=" "), as.numeric(count(gc.EBV.gene)$n), round(as.numeric(min(gc.EBV.gene)), digits = 3), round(as.numeric(max(gc.EBV.gene)), digits = 3), round(mean(unlist(gc.EBV.gene)), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.EBV.factor.summary <- c(paste("GC EBV -", gsub("-", " ", factor), sep=" "), as.numeric(count(gc.EBV.factor)$n), round(as.numeric(min(gc.EBV.factor)), digits = 3), round(as.numeric(max(gc.EBV.factor)), digits = 3), round(mean(unlist(gc.EBV.factor)), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    table <- rbind(gc.EBV.gene.summary, gc.EBV.factor.summary)
    colnames(table) <- c("Dataset", "# of Patients", "Min", "Max", "Mean", "1st Quartile", "Median", "3rd Quartile")
    row.names(table) <- c(paste("GC EBV -", gene, sep=" "), paste("GC EBV -", gsub("-", " ", factor), sep=" "))
    assign("g.table", table, envir = my.env)
    table
  }, options=list(scrollX=TRUE, bFilter=0, bPaginate=0, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:7))), rownames= FALSE)
  
  output$tableFactor <- renderDT({
    hide("gcdownloadScatterP")
    hide("gcdownloadScatterV")
    hide("downloadFactorCSV")
    hide("downloadCSV")
    hide("d")
    req(input$searchGene)
    show("downloadFactorCSV")
    gene <- input$searchGene
    listFactor <- sort(gsub("\\.", " ", colnames(gc.imm[c(4:6,8:54,57:59)])))
    assign("g.gene", gene, envir = my.env)
    pval.EBV = c()
    psig.EBV = c()
    rho.EBV = c()
    names = c()
    for (i in 1:length(listFactor)){
      gc.plot.data <- makePlotDataScatter(gene, listFactor[i], "GC")
      percent1 <- 100*colSums(na.omit(gc.plot.data[1]) == 0)/nrow(gc.plot.data)
      truthy1=TRUE
      if (percent1 > 50){truthy1=FALSE}
      names = c(names, paste(gene, "vs.", gsub("-", " ", listFactor[i]), sep=" "))
      if (listFactor[i] == "Indel-Neoantigens" | listFactor[i] == "SNV-Neoantigens"){
        pval.EBV = c(pval.EBV, 1)
        psig.EBV = c(psig.EBV, "n.s.")
        rho.EBV = c(rho.EBV, 0)
        next
      }
      EBV <- cor.test(as.numeric(unlist(na.omit(gc.plot.data$ValueGene))), as.numeric(unlist(na.omit(gc.plot.data$ValueFactor))), method = "spearman", conf.level = 0.95, exact=FALSE)
      p.EBV <- EBV$p.value
      r.EBV <- EBV$estimate
      if (is.na(p.EBV) | p.EBV > 0.05 | truthy1==FALSE){
        p.EBV.text <- "n.s."
      } else if (p.EBV < 0.0001) {
        p.EBV.text <- "****"
      } else if (p.EBV < 0.001) {
        p.EBV.text <- "***"
      } else if (p.EBV < 0.01) {
        p.EBV.text <- "**"
      } else if (p.EBV < 0.05) {
        p.EBV.text <- "*"
      } 
      pval.EBV = c(pval.EBV, p.EBV)
      psig.EBV = c(psig.EBV, p.EBV.text)
      rho.EBV = c(rho.EBV, r.EBV)
    }
    tablePRN = data.frame(names, rho.EBV, pval.EBV, psig.EBV)
    colnames(tablePRN) <- c("Comparison Group", "GC EBV Spearman ρ", "GC EBV p-Val.", "GC EBV Signif.")
    assign("g.tableFactor", tablePRN, envir = my.env)
    datatable(tablePRN, options=list(scrollX=TRUE, bFilter=0, pageLength = 25, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:3))), rownames= FALSE) %>% formatSignif(columns=c(2,3), digits=3) %>%  formatStyle(columns = c('GC EBV Signif.'), color = styleEqual(c('n.s.', '*', '**', '***', '****'), c('#000000', '#521919', '#8a1d1d', '#ab1b1b', '#e61212')))
  })
  
  drawPlotScatter <- function(plot.data, gene, factor, titles) {
    my.plot <- ggplot(data = plot.data, aes(x=ValueFactor, y=ValueGene, color=Status)) + 
      geom_point() + 
      xlab(paste(gsub("-"," ",factor), sep="")) +
      ylab(paste("mRNA expression levels of", gene, sep=" ")) +
      ggtitle(titles) +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_color_manual(labels = c("EBV"), values = c("red")) +
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
      stat_cor(method = "spearman") +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=10, color='black'), axis.title.x = element_text(size=10, color='black'))
    return(my.plot)
  }
  
  output$scatterPlotGC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    show("gcdownloadScatterP")
    show("gcdownloadScatterV")
    show("downloadCSV")
    show("d")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataScatter(gene, factor, "GC")
    gc.plotScatter <- drawPlotScatter(plot.data, gene, factor, "GC Dataset - Immune Function Correlations")
    assign("g.gc.plotScatter", gc.plotScatter, envir = my.env)
    gc.plotScatter
  })
  
  makePlotDataScatter <- function(gene, factor, mode) {
    if (mode == "GC" & grepl("miR", gene, fixed = TRUE)){
      EBV <- data.frame(gc.combined2[gc.combined2$Status == "EBV", c(gene,factor)])
    } else {
      EBV <- data.frame(gc.combined1[gc.combined1$Status == "EBV", c(gene,factor)])
    } 
    EBV['Status'] = "EBV"
    names(EBV)[1] = "ValueGene"
    names(EBV)[2] = "ValueFactor"
    plot.data <- EBV
    return(plot.data)
  }
  
  output$description <- renderText({
    hide("d")
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