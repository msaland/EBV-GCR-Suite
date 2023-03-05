library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

gene.orient <- readRDS("gene-orient.rds")
pre.autofill <- sort(readRDS("list_options.rds")$V1)
autofill <- intersect(unlist(gene.orient[1]), pre.autofill)
autofill2 <- vector()
my.env <- new.env()

ui <- fluidPage(title = "EBV-GCR - Probe Methylation vs. Genomic Loci", theme = shinytheme("cosmo"), useShinyjs(), 
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  tags$head( tags$style(HTML(" .shiny-output-error-validation { color: red; } ")) ),
  titlePanel("EBV-GCR - Correlation of Cellular Loci and Probe Methylation by EBV Status"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to compare genomic methylation across the loci of candidate cellular genes. Using this tool, researchers can easily investigate the available TCGA DNA methylation data at individual CpGs in relation to their precise genomic location, while identifying differences in DNA methylation between Epstein-Barr virus (EBV) positive, microsatellite-unstable/instability (MSI), chromosomal instability (CIN), genomically stable (GS), and mutant polymerase epsilon (POLE) tissue from the TCGA gastric carcinoma (GC) cohort. Select the candidate cellular gene by typing the name in the input box on the top right. The slider allows modification of the genomic region under investigation. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Images of graphs can be downloaded as a PNG file and a statistical analysis comparing relative methylation for all probes in the defined region is generated to allow 'at a glance' identification of probes demonstrating statistically significant methylation. Individual box plots comparing relative methylation for a given probe can be downloaded as PNG or PDF files and the information necessary to reproduce the box plots can be downloaded as a CSV file. Level 3 Infinium HumanMethylation450 BeadChip array data for the TCGA GC cohorts was downloaded from the Broad Genome Data Analysis Centers Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), "."),
  p(style="text-align: justify;", strong("Download the Differentially Methylated Probe (DMP) Analysis: "), a(href="DMP-methylation.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v5", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout(sidebarPanel( 
      fluidRow(column(4, selectizeInput(inputId = 'searchGene', label = HTML('Select or Type Gene Name'), choices = NULL, selected = character(0), options = list(placeholder = 'Please select a gene from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
              column(8, sliderInput("rangeBP", label = "Selection Range (bp)", min = 0, max = 0, value = c(0,0), step = 1))),
      fluidRow(column(6, selectizeInput(inputId = 'searchProbe', label = HTML('Select Probe associated with Gene'), choices = autofill2, selected = character(0), options = list(placeholder = 'Please select a probe from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE))),
      fluidRow(column(12,dataTableOutput(outputId ="signifProbe"))),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadData', 'Download Table'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      width=6),
    mainPanel(width = 6,
      jqui_resizable(plotOutput(outputId = "linePlotGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'GCdownloadLineP', 'Download GC Lineplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'GCdownloadLineV', 'Download GC Lineplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "boxGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'GCdownloadBoxP', 'Download GC Boxplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'GCdownloadBoxV', 'Download GC Boxplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))
      )),HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'searchGene', choices = autofill, server = TRUE, selected = character(0))
  
  output$GCdownloadLineP <- downloadHandler(
    filename = function() {paste("GC-lineplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotGC, width = 5*input$linePlotGC_size$width, height = 5*input$linePlotGC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$GCdownloadBoxP <- downloadHandler(
    filename = function() {paste("GC-boxplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotGC, width = 5*input$boxGC_size$width, height = 5*input$boxGC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$GCdownloadLineV <- downloadHandler(
    filename = function() {paste("GC-lineplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotGC, width = 5*input$linePlotGC_size$width, height = 5*input$linePlotGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$GCdownloadBoxV <- downloadHandler(
    filename = function() {paste("GC-boxplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotGC, width = 5*input$boxGC_size$width, height = 5*input$boxGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("Methylation-Data-", my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePR, filename, row.names=FALSE)
  })
  
  observeEvent(input$rangeBP,{
    req(input$searchGene)
    part.data.gc=data.frame(my.env$g.data.active.gc[my.env$g.data.active.gc[4] > input$rangeBP[1] & my.env$g.data.active.gc[4] < input$rangeBP[2],])
    assign("g.part.data.gc", part.data.gc, envir = my.env)
    autofill2=as.vector(my.env$g.part.data.gc[1])
    updateSelectizeInput(session, "searchProbe", choices = autofill2)
    rm(part.data.gc)

    if(nrow(my.env$g.part.data.gc)<2){
      hide("GCdownloadLine")
      hide("downloadData")
    } else{
      show("GCdownloadLine")
      show("downloadData")
    }
  })
  
  observeEvent(input$searchGene,{
    hide("GCdownloadLineV")
    hide("GCdownloadBoxV")
    hide("GCdownloadLineP")
    hide("GCdownloadBoxP")
    hide("downloadData")
    hide("renderTable")
    req(input$searchGene)
    show("GCdownloadLineV")
    show("GCdownloadLineP")
    show("downloadData")
    show("renderTable")
    assign("g.gene", input$searchGene, envir = my.env)
    gene <- input$searchGene
    data.active.gc <- readRDS(paste("../ebvgcr-data/gene-segments/gc-methyl-",gene,".rds",sep=""))
    assign("g.data.active.gc", data.active.gc, envir = my.env)
    maxVal=as.numeric(gene.orient[gene.orient[1]==gene][5])
    minVal=as.numeric(gene.orient[gene.orient[1]==gene][4])
    updateSliderInput(session, "rangeBP", label = "Selection Range (bp)", max=maxVal+100000, min=minVal-100000,value=c(minVal, maxVal))
    part.data.gc=data.frame(my.env$g.data.active.gc[my.env$g.data.active.gc[4] > minVal & my.env$g.data.active.gc[4] < maxVal,])
    assign("g.part.data.gc", part.data.gc, envir = my.env)
    rm(part.data.gc)
  })
  
  observeEvent(input$searchProbe,{
    hide("GCdownloadBoxV")
    hide("GCdownloadBoxP")
    req(input$searchGene, input$rangeBP, input$searchProbe)
    show("GCdownloadBoxV")
    show("GCdownloadBoxP")
    assign("g.probe", input$searchProbe, envir = my.env)
  })
  
  output$boxGC <- renderPlot({
    req(input$searchProbe)
    probe=input$searchProbe
    plot.data <- dataBox(my.env$g.part.data.gc[my.env$g.part.data.gc[1]==probe,])
    plot.data$Status <- factor(plot.data$Status , levels=c("CIN", "EBV", "MSI", "GS", "POLE", "Normal"))
    my.plot <- ggplot(plot.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("CIN","EBV","MSI","GS","POLE", "Normal"), name="EBV Status") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      ylab(paste("Methylation (Beta Values)")) +
      ggtitle(paste("GC Data - ",probe,sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    assign("g.boxplotGC", my.plot, envir = my.env)
    my.plot
  })
  
  dataBox <- function(g.probe.data){
    CIN <- data.frame(g.probe.data[6],g.probe.data[7], g.probe.data[8], g.probe.data[9], g.probe.data[10], "CIN")
    colnames(CIN) <- c("Median", "Min", "Max", "One", "Three", "Status")
    EBV <- data.frame(g.probe.data[12],g.probe.data[13], g.probe.data[14], g.probe.data[15], g.probe.data[16], "EBV")
    colnames(EBV) <- c("Median", "Min", "Max", "One", "Three", "Status")
    MSI <- data.frame(g.probe.data[18],g.probe.data[19], g.probe.data[20], g.probe.data[21], g.probe.data[22], "MSI")
    colnames(MSI) <- c("Median", "Min", "Max", "One", "Three", "Status")
    GS <- data.frame(g.probe.data[24],g.probe.data[25], g.probe.data[26], g.probe.data[27], g.probe.data[28], "GS")
    colnames(GS) <- c("Median", "Min", "Max", "One", "Three", "Status")
    POLE <- data.frame(g.probe.data[30],g.probe.data[31], g.probe.data[32], g.probe.data[33], g.probe.data[34], "POLE")
    colnames(POLE) <- c("Median", "Min", "Max", "One", "Three", "Status")
    Nrm <- data.frame(g.probe.data[36],g.probe.data[37], g.probe.data[38], g.probe.data[39], g.probe.data[40], "Normal")
    colnames(Nrm) <- c("Median", "Min", "Max", "One", "Three", "Status")
    plot.data <- rbind(CIN, EBV, MSI, GS, POLE, Nrm)
    return(plot.data)
  }
  
  dataLinePlot <- function(g.part.data){
    if(nrow(g.part.data) > 1){
      CIN <- g.part.data[c(4,5)]
      CIN["Status"]="CIN"
      colnames(CIN) = c("GenCoord", "MeanMethyl", "Status")
      EBV <- g.part.data[c(4,11)]
      EBV["Status"]="EBV"
      colnames(EBV) = c("GenCoord", "MeanMethyl", "Status")
      MSI <- g.part.data[c(4,17)]
      MSI["Status"]="MSI"
      colnames(MSI) = c("GenCoord", "MeanMethyl", "Status")
      GS <- g.part.data[c(4,23)]
      GS["Status"]="GS"
      colnames(GS) = c("GenCoord", "MeanMethyl", "Status")
      POLE <- g.part.data[c(4,29)]
      POLE["Status"]="POLE"
      colnames(POLE) = c("GenCoord", "MeanMethyl", "Status")
      NORM <- g.part.data[c(4,35)]
      NORM["Status"]="NORMAL"
      colnames(NORM) = c("GenCoord", "MeanMethyl", "Status")
      plot.data = rbind(CIN, EBV, MSI, GS, POLE, NORM)
      return(plot.data)
    }
    return(NULL)
  }
  
  output$linePlotGC <- renderPlot({
    req(input$searchGene, input$rangeBP[1]>0)
    validate(need(nrow(my.env$g.part.data.gc)>1, "No probes in selected region."))
    plot.data <- dataLinePlot(my.env$g.part.data.gc)
    numVals = my.env$g.part.data.gc[4]
    maxVal=max(numVals)
    minVal=min(numVals)
    gene=input$searchGene
    maxVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][5])
    minVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][4])
    numVals.segment = input$rangeBP[c(1,2)]
    maxVal.segment=max(numVals.segment)
    minVal.segment=min(numVals.segment)
    if (ncol(my.env$g.part.data.gc>2)){
      my.plot <- ggplot(data=plot.data, aes(x=GenCoord, y=MeanMethyl, color=Status)) +
        geom_line(size=1.25) +
        scale_x_continuous(sec.axis=sec_axis(~.,breaks=unname(unlist(my.env$g.part.data.gc[4])), labels=unname(unlist(my.env$g.part.data.gc[1]))), limits = c(input$rangeBP[1],input$rangeBP[2])) +
        ylim(0,1) + xlab("Genomic Coordinates") + ylab("Mean Methylation (Beta Values)") + ggtitle("GC Data - Mean Methylation by Probe") +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'), axis.text.x.top = element_text(size=10, color='black', angle = 45, hjust = 0)) +
        geom_point() 
      name <- my.env$g.data.active.gc[grepl(my.env$g.gene, unlist(my.env$g.data.active.gc[2])),1][1]
      if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        print("")
      } else if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.segment, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      }  else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.segment, yend = 0), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        print("")
      }
      assign("g.lineplotGC", my.plot, envir = my.env, )
      my.plot
    }
  })
  
  output$signifProbe <- renderDT({
    req(input$searchGene, input$rangeBP[1]>0)
    gene <- input$searchGene
    tablePRfull <- readRDS(paste("../ebvgcr-data/methyl-stats/methyl-", gene, ".rds", sep=""))
    tablePR <- tablePRfull[tablePRfull[4] >= input$rangeBP[1] & tablePRfull[4] <= input$rangeBP[2],]
    validate(need(nrow(tablePR)>1, "No probes in selected region."))
    gc.pval.CE=c()
    gc.pval.CM=c()
    gc.pval.CG=c()
    gc.pval.CP=c()
    gc.pval.CN=c()
    gc.pval.EM=c()
    gc.pval.EG=c()
    gc.pval.EP=c()
    gc.pval.EN=c()
    gc.pval.MG=c()
    gc.pval.MP=c()
    gc.pval.MN=c()
    gc.pval.GP=c()
    gc.pval.GN=c()
    gc.pval.PN=c()
    for (i in 1:length(unlist(tablePR[1]))){
      q <- as.numeric(tablePR[i,6])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.CE <- c(gc.pval.CE, p.text)
      q <- as.numeric(tablePR[i,8])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.CM <- c(gc.pval.CM, p.text)
      q <- as.numeric(tablePR[i,10])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.CG <- c(gc.pval.CG, p.text)
      q <- as.numeric(tablePR[i,12])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.CP <- c(gc.pval.CP, p.text)
      q <- as.numeric(tablePR[i,14])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.CN <- c(gc.pval.CN, p.text)
      q <- as.numeric(tablePR[i,16])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.EM <- c(gc.pval.EM, p.text)
      q <- as.numeric(tablePR[i,18])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.EG <- c(gc.pval.EG, p.text)
      q <- as.numeric(tablePR[i,20])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.EP <- c(gc.pval.EP, p.text)
      q <- as.numeric(tablePR[i,22])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.EN <- c(gc.pval.EN, p.text)
      q <- as.numeric(tablePR[i,24])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.MG <- c(gc.pval.MG, p.text)
      q <- as.numeric(tablePR[i,26])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.MP <- c(gc.pval.MP, p.text)
      q <- as.numeric(tablePR[i,28])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.GP <- c(gc.pval.GP, p.text)
      q <- as.numeric(tablePR[i,30])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.MN <- c(gc.pval.MN, p.text)
      q <- as.numeric(tablePR[i,32])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.GN <- c(gc.pval.GN, p.text)
      q <- as.numeric(tablePR[i,34])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      gc.pval.PN <- c(gc.pval.PN, p.text)
    }
    tableDraw=data.frame(tablePR[1],tablePR[2],tablePR[3],tablePR[4],tablePR[5],tablePR[6],gc.pval.CE,tablePR[7],tablePR[8],gc.pval.CM,tablePR[9],tablePR[10],gc.pval.CG,tablePR[11],tablePR[12],gc.pval.CP,tablePR[13],tablePR[14],gc.pval.CN,tablePR[15],tablePR[16],gc.pval.EM,tablePR[17],tablePR[18],gc.pval.EG,tablePR[19],tablePR[20],gc.pval.EP,tablePR[21],tablePR[22],gc.pval.EN,tablePR[23],tablePR[24],gc.pval.MG,tablePR[25],tablePR[26],gc.pval.MP,tablePR[27],tablePR[28],gc.pval.MN,tablePR[29],tablePR[30],gc.pval.GP,tablePR[31],tablePR[32],gc.pval.GN,tablePR[33],tablePR[34],gc.pval.PN)
    colnames(tableDraw)=c("Probe", "Gene", "Chromosome", "Coordinate", "GC CIN vs. EBV p-Val.", "GC CIN vs. EBV q-Val. (FDR=0.1)", "GC CIN vs. EBV Signif.", "GC CIN vs. MSI p-Val.", "GC CIN vs. MSI q-Val. (FDR=0.1)", "GC CIN vs. MSI Signif.", "GC CIN vs. GS p-Val.", "GC CIN vs. GS q-Val. (FDR=0.1)", "GC CIN vs. GS Signif.", "GC CIN vs. POLE p-Val.", "GC CIN vs. POLE q-Val. (FDR=0.1)", "GC CIN vs. POLE Signif.", "GC CIN vs. Normal p-Val.", "GC CIN vs. Normal q-Val. (FDR=0.1)", "GC CIN vs. Normal Signif.", "GC EBV vs. MSI p-Val.", "GC EBV vs. MSI q-Val. (FDR=0.1)", "GC EBV vs. MSI Signif.", "GC EBV vs. GS p-Val.", "GC EBV vs. GS q-Val. (FDR=0.1)", "GC EBV vs. GS Signif.", "GC EBV vs. POLE p-Val.", "GC EBV vs. POLE q-Val. (FDR=0.1)", "GC EBV vs. POLE Signif.", "GC EBV vs. Normal p-Val.", "GC EBV vs. Normal q-Val. (FDR=0.1)", "GC EBV vs. Normal Signif.", "GC MSI vs. GS p-Val.", "GC MSI vs. GS q-Val. (FDR=0.1)", "GC MSI vs. GS Signif.", "GC MSI vs. POLE p-Val.", "GC MSI vs. POLE q-Val. (FDR=0.1)", "GC MSI vs. POLE Signif.", "GC MSI vs. Normal p-Val.", "GC MSI vs. Normal q-Val. (FDR=0.1)", "GC MSI vs. Normal Signif.", "GC GS vs. POLE p-Val.", "GC GS vs. POLE q-Val. (FDR=0.1)", "GC GS vs. POLE Signif.", "GC GS vs. Normal p-Val.", "GC GS vs. Normal q-Val. (FDR=0.1)", "GC GS vs. Normal Signif.", "GC POLE vs. Normal p-Val.", "GC POLE vs. Normal q-Val. (FDR=0.1)", "GC POLE vs. Normal Signif.")
    assign("g.tablePR", tableDraw, envir = my.env)
    datatable(my.env$g.tablePR, options=list(scrollX=TRUE, bFilter=0, pageLength = 15, bLengthChange=0, bInfo=0, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0:48))), rownames= FALSE) %>%  formatStyle(columns = c('GC CIN vs. EBV Signif.', 'GC CIN vs. MSI Signif.', 'GC CIN vs. GS Signif.', 'GC CIN vs. POLE Signif.', 'GC CIN vs. Normal Signif.', 'GC EBV vs. MSI Signif.', 'GC EBV vs. GS Signif.', 'GC EBV vs. POLE Signif.', 'GC EBV vs. Normal Signif.', 'GC MSI vs. GS Signif.', 'GC MSI vs. POLE Signif.', 'GC MSI vs. Normal Signif.', 'GC GS vs. POLE Signif.', 'GC GS vs. Normal Signif.', 'GC POLE vs. Normal Signif.'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30,32,33,35,36,38,39,41,42,44,45,47,48), digits=3)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)