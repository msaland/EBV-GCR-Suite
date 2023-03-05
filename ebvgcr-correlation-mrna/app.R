library(corrplot)
library(DT)
library(cowplot)
library(gdata)
library(ggplot2)
library(ggpubr)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

my.env <- new.env()
autofillCell <- sort(readRDS("../ebvgcr-data/autofill-mRNA.rds"))
gc.EBV.genes <- readRDS("../ebvgcr-data/EBVaGC_vir_miRNA.rds")
colnames(gc.EBV.genes) <- gsub("\\.", "-", colnames(gc.EBV.genes))
EBV.miRNA <- sort(gsub("ebv-","",colnames(gc.EBV.genes)[3:ncol(gc.EBV.genes)]))

ui <- fluidPage(title = "EBV-GCR - EBV miRNA vs. Cellular mRNA", theme = shinytheme("cosmo"), useShinyjs(),
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("EBV-GCR - The EBV Gastric Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Available Tools:", tabPanel(title="☲"), 
              navbarMenu(title="THInCR", tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/THInCR/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
              navbarMenu(title="EBV-GCR", tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-home/", "Home")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmrna/", "Cellular mRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-correlation-mmirna/", "Cellular miRNA vs. EBV mRNA Expression")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-imm-vir-genes/", "Viral mRNA/miRNA vs. Immune Function")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-methyl-all/", "Cellular loci vs. Methylation Status")), tabPanel(a(href="https://mymryklab.ca/EBV-GCR/ebvgcr-single-cell/", "Single-Cell Analysis")))),
  titlePanel("EBV-GCR - Correlation of EBV miRNA and Cellular mRNA Expression"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between viral gene miRNA expression and expression of candidate cellular mRNAs in EBV positive gastric carcinomas (GC). Select the candidate cellular mRNAs by typing the name in the input box on the top right to perform a Spearman's correlation analysis. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Heat maps can be downloaded as PNG or PDF files and the significance matrix data can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA GC cohort was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". EBV viral miRNA expression data were sourced from Ungerleider et al, PLOS Pathogens 2021 (EBV miRNAs are potent effectors of tumor cell transcriptome remodeling in promoting immune escape) ",a(href="https://pubmed.ncbi.nlm.nih.gov/33956915/", "DOI: 10.1371/journal.ppat.1009217")),
  p(style="text-align: justify;", strong("Download the Correlation Master List: "), a(href="Correlation-cell_mRNA-vs-vir_miRNA.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v3", "Not Updated")),
  p(style="text-align: justify;", strong("Citing EBV-GCR: "),"Not Updated"),
  sidebarLayout( sidebarPanel( 
    selectizeInput(inputId = 'searchCell', label = HTML('Type the Cellular Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
                   choices = NULL, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE), width=6, 
    tags$div(HTML('<br/>')), tags$div(tags$strong("Pairwise comparisons for GC Data:")), tags$div(HTML('<br/>')), dataTableOutput(outputId = 'tableGC'), br(), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'GCdownloadCSV', 'Download GC Data'),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
    mainPanel(
      fluidRow(column(6, align="center", offset = 3, dataTableOutput(outputId = 'equiv'))), br(),
      jqui_resizable(plotOutput(outputId = "corrGC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadP', 'Download GC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'gcdownloadV', 'Download GC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'searchCell', choices = autofillCell, server = TRUE, selected = character(0))

  output$GCdownloadCSV <- downloadHandler(
    filename = function() {paste("GC-", my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.gc.table, filename, row.names=FALSE)
  })
  
  output$gcdownloadP <- downloadHandler(
    filename = function() {paste("GC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot, width = 5*input$corrGC_size$width, height = 5*input$corrGC_size$height, dpi = 300, units = "px", device = "png")
  })

  output$gcdownloadV <- downloadHandler(
    filename = function() {paste("GC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.gc.plot, width = 5*input$corrGC_size$width, height = 5*input$corrGC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$equiv <- renderDT({
    hide("GCdownloadCSV")
    hide("gcdownloadP")
    hide("gcdownloadV")
    req(input$searchCell)
    show("GCdownloadCSV")
    show("gcdownloadP")
    show("gcdownloadV")
    show("equiv")
    temp = readRDS("equiv.rds")
    colnames(temp)=c("Symbol", "p-Value Equivalent")
    datatable(temp, options=list(scrollX=TRUE, bFilter=0, paginate=0, pageLength = 5, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:1))), rownames= FALSE) 
  })
  
  output$tableGC <- renderDT({
    req(input$searchCell)
    geneCell <- input$searchCell
    assign("g.gene", geneCell, envir = my.env)
    tablePre <- readRDS(paste("../ebvgcr-data/corr-stats-mrna/gc-", geneCell, ".rds", sep=""))
    names=paste(gsub('(.*)-\\w+','\\1',geneCell),"vs.",rownames(tablePre),sep=" ")
    s=c()
    for (i in 1:37){
      if (is.na(tablePre[i,3]) | tablePre[i,3] > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      s <- c(s, p.text)
    }
    data=cbind(names, tablePre[1], tablePre[2], tablePre[3], s)
    colnames(data) <- c("Comparison Group", "Spearman ρ", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.gc.table", data, envir = my.env)
    datatable(data, options=list(scrollX=TRUE, bFilter=0, pageLength = 15, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))), rownames= FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212')))
  })
  
  drawPlot <- function(plot.data, titles, geneCell, mode) {
    var1=c()
    var2=c()
    correl=c()
    pval=c()
    for(i in 5:41){
      var1=c(var1, gsub('(.*)-\\w+','\\1',colnames(plot.data[3])))
      var2=c(var2, gsub("ebv-","",colnames(plot.data[i])))
      correl=c(correl, signif(cor.test(unlist(na.omit(plot.data[3])), unlist(na.omit(plot.data[i])), method = "spearman", conf.level = 0.95, exact = FALSE)$estimate,3))
      p=cor.test(unlist(na.omit(plot.data[3])), unlist(na.omit(plot.data[i])), method = "spearman", conf.level = 0.95, exact = FALSE)$p.value
      if (is.na(p) | p>0.05){
        pval=c(pval, "n.s.")
      } else if (p<0.0001){
        pval=c(pval, "****")
      } else if (p<0.001){
        pval=c(pval, "***")
      } else if (p<0.01){
        pval=c(pval, "**")
      } else if (p<0.05){
        pval=c(pval, "*")
      }
    }
    data=data.frame(var1, var2, correl, pval)
    data=data[order(var2),]
    data1=data[c(1:13),]
    data2=data[c(14:25),]
    data3=data[c(26:37),]
    my.plot1 <- ggplot(data = data1, aes(var1, var2, fill = correl)) +
      geom_tile(color = "white") +
      scale_y_discrete(labels=EBV.miRNA[c(1:13)]) +
      scale_x_discrete(labels=gsub('(.*)-\\w+','\\1',geneCell)) +
      geom_text(aes(var1, var2, label = correl), color="black", size=5, vjust = -0.1) +
      geom_text(aes(label=pval), color="black", size=5, vjust = 0.9) + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") + theme_minimal() + 
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), axis.text.y=element_text(size=12, color='black'), axis.text.x=element_text(size=12, color='black', angle=45, hjust=1, vjust=1)) +
      ggtitle(" ") + guides(fill="none")
    my.plot2 <- ggplot(data = data2, aes(var1, var2, fill = correl)) +
      geom_tile(color = "white") +
      scale_y_discrete(labels=EBV.miRNA[c(14:25)]) +
      scale_x_discrete(labels=gsub('(.*)-\\w+','\\1',geneCell)) +
      geom_text(aes(var1, var2, label = correl), color="black", size=5, vjust = -0.1) +
      geom_text(aes(label=pval), color="black", size=5, vjust = 0.9) + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") + theme_minimal() + 
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), axis.text.y=element_text(size=12, color='black'), axis.text.x=element_text(size=12, color='black', angle=45, hjust=1, vjust=1)) +
      ggtitle(titles) + guides(fill="none")
    my.plot3 <- ggplot(data = data3, aes(var1, var2, fill = correl)) +
      geom_tile(color = "white") +
      scale_y_discrete(labels=EBV.miRNA[c(26:37)]) +
      scale_x_discrete(labels=gsub('(.*)-\\w+','\\1',geneCell)) +
      geom_text(aes(var1, var2, label = correl), color="black", size=5, vjust = -0.1) +
      geom_text(aes(label=pval), color="black", size=5, vjust = 0.9) + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") + theme_minimal() + 
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), axis.text.y=element_text(size=12, color='black'), axis.text.x=element_text(size=12, color='black', angle=45, hjust=1, vjust=1)) +
      ggtitle(" ") + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 6,  title.position = "top", title.hjust = 0.5))
    my.plot=plot_grid(my.plot1, my.plot2, my.plot3, ncol=3, nrow=1, rel_widths = c(3/10,3/10,4/10))
    return(my.plot)
  }
  
  output$corrGC <- renderPlot({
    req(input$searchCell)
    geneCell <- input$searchCell
    plot.data <- makePlotData(geneCell, "GC")
    gc.plot <- drawPlot(plot.data, "GC Dataset - Correlations", geneCell, "GC")
    assign("g.gc.plot", gc.plot, envir = my.env)
    gc.plot
  })
  
  makePlotData <- function(geneCell, mode) {
    if (mode == "GC") {
      gc.mRNA <- readRDS(paste("../ebvgcr-data/mrna-genes/GC-", geneCell, ".rds", sep=""))
      assign("g.gc.mRNA", gc.mRNA, envir = my.env)
      merged=merge(gc.mRNA, gc.EBV.genes, by="Sample")
    }
    return(merged)
  }
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)