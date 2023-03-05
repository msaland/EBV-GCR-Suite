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
autofill <- sort(readRDS("../ebvgcr-data/autofill-miRNA.rds"))
pqvalues <- readRDS("pq-values-miRNA.rds")

ui <- fluidPage(title = "EBV-GCR - miRNA vs. EBV Status", theme = shinytheme("cosmo"), useShinyjs(), 
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  fluidRow(style="margin-left: 1px;", titlePanel("EBV-GCR - Correlation of Cellular miRNA Expression and EBV Status")),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to compare expression of candidate cellular miRNAs between Epstein-Barr virus (EBV) positive, microsatellite-unstable/instability (MSI), chromosomal instability (CIN), genomically stable (GS), mutant polymerase epsilon (POLE), and Normal control tissue from the TCGA gastric carcinoma (GC) cohort. Select the candidate mRNA by typing the name in the input box on the top right. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Box plots can be downloaded as PNG or PDF files and the information necessary to reproduce the box plots can be downloaded as a CSV file. Level 3 miRNA expression data from the TCGA GC cohort was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". "),
  p(style="text-align: justify;", strong("Download the Differentially Expressed Gene (DEG) Analysis: "), a(href="DEG-combined-miRNA.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v1", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout( sidebarPanel(width=6, selectizeInput(
    inputId = 'search', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
    choices = NULL,selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE),
    dataTableOutput(outputId = 'tablePQ'), tags$div(HTML('<br/>')),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadPQ', 'Download Data'),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>')),
    dataTableOutput(outputId = 'tableStats'), tags$div(HTML('<br/>')),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadCSV', 'Download Data'),
       tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
  mainPanel(
    jqui_resizable(plotOutput(outputId = "boxPlotGC")),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadP', 'Download GC Plot (png)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadV', 'Download GC Plot (pdf)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'search', choices = autofill, server = TRUE, selected = character(0))
  
  output$downloadPQ <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-pq.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePQ, filename, row.names=FALSE)
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.tableStats, filename, row.names=FALSE)
  })
  
  output$gcdownloadP <- downloadHandler(
    filename = function() {paste("GC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot, width = 5*input$boxPlotGC_size$width, height = 5*input$boxPlotGC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$gcdownloadV <- downloadHandler(
    filename = function() {paste("GC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot, width = 5*input$boxPlotGC_size$width, height = 5*input$boxPlotGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$tablePQ <- renderDT({
    hide("gcdownloadP")
    hide("gcdownloadV")
    hide("downloadCSV")
    hide("downloadPQ")
    req(input$search)
    show("gcdownloadP")
    show("gcdownloadV")
    show("downloadCSV")
    show("downloadPQ")
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    gc.mRNA <- readRDS(paste("../ebvgcr-data/mirna-genes/GC-", gene, ".rds", sep=""))
    assign("g.gc.mRNA", gc.mRNA, envir = my.env)
    temp <- pqvalues[pqvalues$Gene == gene,]
    pval <- c(temp[2], temp[4], temp[6], temp[8], temp[10], temp[12], temp[14], temp[16], temp[18], temp[20], temp[22], temp[24], temp[26], temp[28], temp[30])
    pval <- signif(as.numeric(pval), digits = 3)
    qval <- c(temp[3], temp[5], temp[7], temp[9], temp[11], temp[13], temp[15], temp[17], temp[19], temp[21], temp[23], temp[25], temp[27], temp[29], temp[31])
    qval <- signif(as.numeric(qval), digits = 3)
    pval.s <- c()
    gc.plot.data <- makePlotData(gene, "GC")
    percent1 <- 100*length(which(gc.plot.data[1] == 0))/nrow(gc.plot.data)
    truthy1=TRUE
    if (percent1 > 50){truthy1=FALSE}
    for (i in 1:length(pval)){
      q <- as.numeric(qval[i])
      if (is.na(q) | q > 0.1 | truthy1==FALSE){
        p.text <- "NO"
      } else{
        p.text <- "YES"
      }
      pval.s <- c(pval.s, p.text)
    }
    names <- c("GC - CIN vs. EBV", "GC - CIN vs. MSI", "GC - CIN vs. GS", "GC - CIN vs. POLE", "GC - CIN vs. Normal", "GC - EBV vs. MSI", "GC - EBV vs. GS", "GC - EBV vs. POLE", "GC - EBV vs. Normal", "GC - MSI vs. GS", "GC - MSI vs. POLE", "GC - MSI vs. Normal", "GC - GS vs. POLE", "GC - GS vs. Normal", "GC - POLE vs. Normal")
    tablePQ <- data.frame(cbind(names, pval, qval, pval.s))
    colnames(tablePQ) <- c("Comparison Group", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.tablePQ", tablePQ, envir = my.env)
    datatable(tablePQ, options=list(scrollX=TRUE, bFilter=0, paginate=FALSE, bLengthChange=0, bInfo=0, autoWidth=FALSE, columnDefs=list(list(className = 'dt-center', targets = 0:3))), rownames=FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212')))
  })
  
  output$tableStats <- renderDT({
    req(input$search)
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    gc.mRNA <- my.env$g.gc.mRNA
    gc.CIN <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "STAD_CIN", gene]))
    names(gc.CIN)[1] = "Value"
    gc.CIN.summary <- c("GC - CIN", as.numeric(count(gc.CIN)$n), round(as.numeric(min(gc.CIN)), digits = 3), round(as.numeric(max(gc.CIN)), digits = 3), round(mean(unlist(gc.CIN)), digits = 3), round(as.numeric(summarize_all(gc.CIN, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.CIN, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.EBV <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "STAD_EBV", gene]))
    names(gc.EBV)[1] = "Value"
    gc.EBV.summary <- c("GC - EBV", as.numeric(count(gc.EBV)$n), round(as.numeric(min(gc.EBV)), digits = 3), round(as.numeric(max(gc.EBV)), digits = 3), round(mean(unlist(gc.EBV)), digits = 3), round(as.numeric(summarize_all(gc.EBV, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.EBV, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.MSI <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "STAD_MSI", gene]))
    names(gc.MSI)[1] = "Value"
    gc.MSI.summary <- c("GC - MSI", as.numeric(count(gc.MSI)$n), round(as.numeric(min(gc.MSI)), digits = 3), round(as.numeric(max(gc.MSI)), digits = 3), round(mean(unlist(gc.MSI)), digits = 3), round(as.numeric(summarize_all(gc.MSI, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.MSI, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.GS <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "STAD_GS", gene]))
    names(gc.GS)[1] = "Value"
    gc.GS.summary <- c("GC - GS", as.numeric(count(gc.GS)$n), round(as.numeric(min(gc.GS)), digits = 3), round(as.numeric(max(gc.GS)), digits = 3), round(mean(unlist(gc.GS)), digits = 3), round(as.numeric(summarize_all(gc.GS, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.GS, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.POLE <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "STAD_POLE", gene]))
    names(gc.POLE)[1] = "Value"
    gc.POLE.summary <- c("GC - POLE", as.numeric(count(gc.POLE)$n), round(as.numeric(min(gc.POLE)), digits = 3), round(as.numeric(max(gc.POLE)), digits = 3), round(mean(unlist(gc.POLE)), digits = 3), round(as.numeric(summarize_all(gc.POLE, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.POLE, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    gc.Nrm <- data.frame(na.omit(gc.mRNA[gc.mRNA["Status"] == "Normal", gene]))
    names(gc.Nrm)[1] = "Value"
    gc.Nrm.summary <- c("GC - Normal", as.numeric(count(gc.Nrm)$n), round(as.numeric(min(gc.Nrm)), digits = 3), round(as.numeric(max(gc.Nrm)), digits = 3), round(mean(unlist(gc.Nrm)), digits = 3), round(as.numeric(summarize_all(gc.Nrm, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(gc.Nrm, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(gc.Nrm, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    tableStats <- rbind(gc.CIN.summary, gc.EBV.summary, gc.MSI.summary, gc.GS.summary, gc.POLE.summary, gc.Nrm.summary)
    colnames(tableStats) <- c("Dataset", "# of Patients", "Min", "Max", "Mean", "1st Quartile", "Median", "3rd Quartile")
    row.names(tableStats) <- c("GC - CIN", "GC - EBV", "GC - MSI", "GC - GS", "GC - POLE", "GC - Normal")
    assign("g.tableStats", tableStats, envir = my.env)
    tableStats
  }, options=list(scrollX = TRUE, bFilter=0, bPaginate=0, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:7))), rownames= FALSE)
  
  drawPlot <- function(plot.data, gene, titles) {
    CIN=na.omit(unlist(plot.data[plot.data[2]=="CIN",][1]))
    EBV=na.omit(unlist(plot.data[plot.data[2]=="EBV",][1]))
    MSI=na.omit(unlist(plot.data[plot.data[2]=="MSI",][1]))
    GS=na.omit(unlist(plot.data[plot.data[2]=="GS",][1]))
    POLE=na.omit(unlist(plot.data[plot.data[2]=="POLE",][1]))
    Nrm=na.omit(unlist(plot.data[plot.data[2]=="Normal",][1]))
    transform.CIN=data.frame(signif(max(CIN),3), signif(min(CIN),3), signif(median(CIN),3), quantile(CIN, c(0.25,0.75))[1], quantile(CIN, c(0.25,0.75))[2], "CIN")
    colnames(transform.CIN)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.EBV=data.frame(signif(max(EBV),3), signif(min(EBV),3), signif(median(EBV),3), quantile(EBV, c(0.25,0.75))[1], quantile(EBV, c(0.25,0.75))[2], "EBV")
    colnames(transform.EBV)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.MSI=data.frame(signif(max(MSI),3), signif(min(MSI),3), signif(median(MSI),3), quantile(MSI, c(0.25,0.75))[1], quantile(MSI, c(0.25,0.75))[2], "MSI")
    colnames(transform.MSI)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.GS=data.frame(signif(max(GS),3), signif(min(GS),3), signif(median(GS),3), quantile(GS, c(0.25,0.75))[1], quantile(GS, c(0.25,0.75))[2], "GS")
    colnames(transform.GS)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.POLE=data.frame(signif(max(POLE),3), signif(min(POLE),3), signif(median(POLE),3), quantile(POLE, c(0.25,0.75))[1], quantile(POLE, c(0.25,0.75))[2], "POLE")
    colnames(transform.POLE)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.Nrm=data.frame(signif(max(Nrm),3), signif(min(Nrm),3), signif(median(Nrm),3), quantile(Nrm, c(0.25,0.75))[1], quantile(Nrm, c(0.25,0.75))[2], "Normal")
    colnames(transform.Nrm)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.data=rbind(transform.CIN, transform.EBV, transform.MSI, transform.GS, transform.POLE, transform.Nrm)
    transform.data$Status <- factor(transform.data$Status , levels=c("CIN", "EBV", "MSI", "GS", "POLE", "Normal"))
    my.plot <- ggplot(transform.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("CIN","EBV","MSI", "GS", "POLE", "Normal"), name="Sample Status") +
      ylab(paste("mRNA expression levels of", gsub("hsa-","",gene), sep=" ")) +
      ggtitle(titles) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    return(my.plot)
  }
  
  output$boxPlotGC <- renderPlot({
    req(input$search)
    gene <- input$search
    plot.data <- makePlotData(gene, "GC")
    gc.plot <- drawPlot(plot.data, gene, paste("GC Dataset -",gsub("hsa-","",gene),sep=" "))
    assign("g.gc.plot", gc.plot, envir = my.env)
    gc.plot
  })
  
  makePlotData <- function(gene, mode) {
    if(mode == "GC") {
      gc.mRNA <- my.env$g.gc.mRNA
      CIN <- data.frame(gc.mRNA[gc.mRNA["Status"] == "STAD_CIN", gene])
      EBV <- data.frame(gc.mRNA[gc.mRNA["Status"] == "STAD_EBV", gene])
      MSI <- data.frame(gc.mRNA[gc.mRNA["Status"] =="STAD_MSI", gene])
      GS <- data.frame(gc.mRNA[gc.mRNA["Status"] =="STAD_GS", gene])
      POLE <- data.frame(gc.mRNA[gc.mRNA["Status"] =="STAD_POLE", gene])
      Nrm <- data.frame(gc.mRNA[gc.mRNA["Status"] =="Normal", gene])
    }
    CIN['Status'] = "CIN"
    names(CIN)[1] = "Value"
    EBV['Status'] = "EBV"
    names(EBV)[1] = "Value"
    MSI['Status'] = "MSI"
    names(MSI)[1] = "Value"
    GS['Status'] = "GS"
    names(GS)[1] = "Value"
    POLE['Status'] = "POLE"
    names(POLE)[1] = "Value"
    Nrm['Status'] = "Normal"
    names(Nrm)[1] = "Value"
    plot.data <- rbind(CIN, EBV, MSI, GS, POLE, Nrm)
    return(plot.data)
  } 
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)