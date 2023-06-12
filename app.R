#app.R
#Shiny App for TLFs in clinical studies
#Author: Hengwei Liu




options(shiny.maxRequestSize = 30*1024^2)
library(haven)

library(plotly)
library(shinydashboard)
library(DT)
library(tidyr)
library(shiny)
library(ggplot2)

library(dplyr)
library(stringi)
library(shinythemes)
library(psych)
library(gt)


ui <-
  dashboardPage(
    skin="blue",
    dashboardHeader(title="Data Review"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Demographics", tabName="demog"),
        menuItem("Adverse Events", tabName="adverse"),
        menuItem("Lab Data", tabName="lab"),
        menuItem("Efficacy", tabName="eff")
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="demog",
                fluidPage(
                  theme = shinytheme("cerulean"),
                  navbarPage(
                    theme = "cerulean", 
                    "Demographics",
                    
                    
                    
                    
                    tabPanel("Table", 
                             mainPanel(tableOutput("table_demog"))
                    ),
                    tabPanel("Listing", 
                             mainPanel(tableOutput("listing_demog"))
                    ),
                    tabPanel("Pie Chart", 
                             mainPanel(plotOutput("figure_demog")), 
                             inputPanel( selectInput("Variable","Select a variable:", c( "Race", "Sex")))
                             
                             )
                             
                             
                    
                    
                  ) 
                )
        ),
        tabItem(tabName="adverse",
                fluidPage(
                  theme = shinytheme("united"),
                  navbarPage(
                    theme = "United", 
                    "Adverse Events",
                    
                    
                    
                    
                    tabPanel("Table", 
                             mainPanel(tableOutput("table_adverse"))
                    ),
                    tabPanel("Listing", 
                             mainPanel(tableOutput("listing_adverse"))
                    )
                    
                  ) 
                )
        ),
        tabItem(tabName="lab",
                fluidPage(
                  theme = shinytheme("united"),
                  navbarPage(
                    theme = "United", 
                    "Lab Data",
                    
                    
                    
                    
                    
                    tabPanel("Table", 
                             mainPanel(tableOutput("table_lab"))
                    ),
                    tabPanel("Listing", 
                             mainPanel(tableOutput("listing_lab"))
                    ),
                    tabPanel("Box plot", 
                             
                             radioButtons("Parameter","Select a parameter:", c("Hemoglobin(g/L)", "Lymphocytes(10^9/L)")), 
                             
                             
                             mainPanel(plotOutput("figure_lab"))
                    )
                    
                  ) 
                )
        ),
        tabItem(tabName="eff",
                fluidPage(
                  theme = shinytheme("united"),
                  navbarPage(
                    theme = "United", 
                    "Efficacy",
                    
                    
                    
                    
                    
            
                    tabPanel("Waterfall Plot", 
                             radioButtons("cohort","Select a cohort for plot:", c("Cohort 1", "Cohort 2")), 
                             mainPanel(plotOutput("figure1_eff"))
                    ),
                    tabPanel("spider Plot", 
                             radioButtons("Cohort","Select a cohort for plot:", c("Cohort 1", "Cohort 2")),
                             mainPanel(plotOutput("figure2_eff"))
                    )
                    
                  ) 
                )
        )
      )))

server <- function(input, output) {
  
  
  returnTable1 <- reactive({
    
  
    
    summary <- function(df, my_var, classvar, decimal) {
      # get the descriptive statistics
      ht <- describeBy(df[[my_var]], group=df[[classvar]], mat=TRUE)
      
      # handle the decimals
      ht$n <- format(ht$n, nsamll=0)
      ht$mean <- format(round(ht$mean,decimal+1), nsmall=decimal+1)
      ht$sd <- format(round(ht$sd,decimal+2), nsmall=decimal+2)
      ht$median <- format(round(ht$median,decimal+1), nsmall=decimal+1)
      ht$min <- format(round(ht$min,decimal), nsmall=decimal)
      ht$max <- format(round(ht$max,decimal), nsmall=decimal)
      
      # create a variable minmax, and do transpose
      ht$minmax <- paste(ht$min, ',', ht$max) 
      ht <- ht[c("n","mean","sd","median","minmax")]
      ht2 <- t(ht)
      
      # create a new column called statistics and get the final data for reporting 
      rownames(ht2)  <- c("n","mean","sd","median","min, max")
      
      data.frame(statistics=rownames(ht2), ht2)
      
    }
    
    cat_stat <- function(df, my_var, classvar) {
      freq <- table(df[[my_var]], group=df[[classvar]])
      prop <- 100*prop.table(table(df[[my_var]], df[[classvar]]), 2)
      
      # combine count and percentage
      X11 <- paste(freq[,1], '(', format(prop[,1], digit=3), ')')
      X12 <- paste(freq[,2], '(', format(prop[,2], digit=3), ')')
      sex <- cbind(X11, X12)
      
      
      
      # create a new column called statistics and get the final data for reporting 
      rownames(sex) <- c(sort(unique(df[[my_var]])))
      
      
      data.frame(statistics=rownames(sex), sex)
      
    }
    
   
    
    the_date <- as.character(Sys.Date())
    
    
    # read in the data
    class <- read_sas("class.sas7bdat")
    
    
    # get the count and percentage
    bign <- table(group=class$trt)
    bign1 <- as.numeric(bign[1])
    bign2 <- as.numeric(bign[2])
    
    
    ht3 <- summary(df=class, my_var='Height', classvar='trt', decimal=1)
    wt3 <- summary(df=class, my_var='Weight', classvar='trt', decimal=2)
    sex3 <- cat_stat(df=class, my_var='Sex', classvar='trt')
    race3 <- cat_stat(df=class, my_var='Race', classvar='trt')
    
   
    
    
    final <- rbind(ht3, wt3, sex3, race3)
  
    
    # use gt to do the reporting 
    tab_html <- final %>% 
      gt() %>%
      tab_row_group(
        label = "Race",
        rows = 13:15
      ) %>%
      
      
      tab_row_group(
        label = "Sex",
        rows = 11:12
      ) %>%
      
      tab_row_group(
        label = "Weight (kg)",
        rows = 6:10
      ) %>%
      
      tab_row_group(
        label = "Height (in)",
        rows = 1:5
      ) %>%
      
      tab_header(
        title = "Table 14.1 Demographics and Baseline Characteristics",
        subtitle = "Safety Population"
      ) %>%
      tab_source_note(
        source_note = "Note: the source data is class."
      ) %>%
      
      tab_source_note(
        source_note = paste('Program Source: demog.R            Executed: (Draft)',  the_date)
      ) %>%
      
      cols_label(
        X11 = html(paste("Treatment A <br> (N=", bign1, ")")),
        X12 = html(paste("Treatment B <br> (N=", bign2, ")"))
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      )
    
  
    
    return(tab_html)
    
    
  })
  
  
  
  
  returnTable2 <- reactive({
    class <- read_sas("class.sas7bdat")
    
    gt_tbl <- gt(class)
    
    return(gt_tbl)
    
  })
  
  
  returnTable4 <- reactive({
    ae <- read_sas("adae.sas7bdat")
  
    gt_tbl <- gt(ae)
    
    return(gt_tbl)
    
  })
  
  
  returnTable6 <- reactive({
    lb <- read_sas("adlb2.sas7bdat")
    
    gt_tbl <- gt(lb)
    
    return(gt_tbl)
    
  })
  
  
  returnTable3 <- reactive({
    
    
  
    
    soc_pt <- function(df, trtvar, trtlist){
      # get the number of subjects with at least one TEAE
      
      group_by_grp <- 
        df %>%                   
        group_by({{trtvar}}) %>%
        summarise(unique_subj = n_distinct(USUBJID))
      
      # get the count by System Organ class
      
      group_by_grp1 <- 
        df %>%                   
        group_by({{trtvar}}, AEBODSYS) %>%
        summarise(unique_subj = n_distinct(USUBJID))
      # get the count by preferred term 
      
      group_by_grp2 <- 
        df %>%                   
        group_by({{trtvar}}, AEBODSYS, AEDECOD) %>%
        summarise(unique_subj = n_distinct(USUBJID))
      
      
      
      # do the transpose 
      
      a1 <- group_by_grp %>% 
        pivot_wider(names_from={{trtvar}}, values_from=unique_subj)
      
      a2 <- group_by_grp1 %>% 
        pivot_wider(id_cols=c(AEBODSYS), names_from={{trtvar}}, values_from=unique_subj)
      
      a3 <- group_by_grp2 %>% 
        pivot_wider(id_cols=c(AEBODSYS, AEDECOD), names_from={{trtvar}}, values_from=unique_subj)
      
      a1$term <- "Subjects with at least one TEAE"
      a1$AEBODSYS <- " "
      a2$term <- a2$AEBODSYS
      a3$term <- a3$AEDECOD
      
      
      a1 <- a1[c("AEBODSYS", "term", trtlist)]
      a2 <- a2[c("AEBODSYS", "term", trtlist)]
      a3 <- a3[c("AEBODSYS", "term", trtlist)]
      
      rbind(a1, a2, a3)
      
      
      
    }
    
    
    the_date <- as.character(Sys.Date())
    
    adsl <- read_sas("adsl.sas7bdat")
    adae <- read_sas("adae.sas7bdat")
    
    # create adsl4 with treatment variable grp
    
    adsl1 <- adsl[(adsl$trt01p=='Placebo' & adsl$SAFFL=='Y'), ]
    adsl1$grp <- 'grp1'
    
    adsl2 <- adsl[(adsl$trt01p=='Active' & adsl$SAFFL=='Y'), ]
    adsl2$grp <- 'grp2'
    
    adsl3 <- rbind(adsl1, adsl2)
    adsl3$grp <- 'grp3'
    
    adsl4 <- rbind(adsl1, adsl2, adsl3)
    
    # create adae4 with treatment variable grp 
    
    adae1 <- adae[(adae$trt01p=='Placebo' & adae$SAFFL=='Y' & adae$TRTEMFL=='Y'), ]
    adae1$grp <- 'grp1'
    
    adae2 <- adae[(adae$trt01p=='Active' & adae$SAFFL=='Y' & adae$TRTEMFL=='Y'), ]
    adae2$grp <- 'grp2'
    
    adae3 <- rbind(adae1, adae2)
    adae3$grp <- 'grp3'
    
    adae4 <- rbind(adae1, adae2, adae3)
    
    # get the big N in column headers from adsl4
    bign <- table(group=adsl4$grp)
    
    
    final <- soc_pt(df=adae4, trtvar=grp,  trtlist=c('grp1','grp2','grp3'))
    
    #sort the ae data by AEBODSYS and descending order in the total column
    df <-final[order(final$AEBODSYS, -final$grp3),]
    
    #create the final data df for reporting
    
    df$perc1 <- 100*df$grp1/bign[1]
    df$perc1 <- format(round(df$perc1, 1), nsmall = 1)
    df$grp1_c <- paste(df$grp1, "(", df$perc1, ")")
    df$grp1_c <- ifelse(is.na(df$grp1),"0", df$grp1_c)
    
    df$perc2 <- 100*df$grp2/bign[2]
    df$perc2 <- format(round(df$perc2, 1), nsmall = 1)
    df$grp2_c <- paste(df$grp2, "(", df$perc2, ")")
    df$grp2_c <- ifelse(is.na(df$grp2),"0", df$grp2_c)
    
    df$perc3 <- 100*df$grp3/bign[3] 
    df$perc3 <- format(round(df$perc3, 1), nsmall = 1)
    df$grp3_c <- paste(df$grp3, "(", df$perc3, ")")
    df$grp3_c <- ifelse(is.na(df$grp3),"0", df$grp3_c)
    
    df <- df[c("AEBODSYS", "term", "grp1_c","grp2_c","grp3_c")]
    
    # use gt to do the reporting 
    tab_html <- df %>% 
      gt() %>%
      
      tab_header(
        title = "Table 14.3.1 Treatment Emergent Adverse Events by System Organ Class and Preferred Term",
        subtitle = "Safety Population"
      ) %>%
      tab_source_note(
        source_note = "Note: TEAE is defined to be the AEs with start date >= first dose date and <= last dose date + 30."
      ) %>%
      tab_source_note(
        source_note = "System organ class is in light gray. The table is sorted by system organ class and descending order in the total column."
      ) %>%
      
      tab_source_note(
        source_note = paste('Program Source: ae.R            Executed: (Draft)',  the_date)
      ) %>%
      
      cols_label(
        term = html("System Organ Class <br> Preferred Term"),
        grp1_c = html(paste("Placebo <br> (N=", bign[1], ")")),
        grp2_c = html(paste("Active <br> (N=", bign[2], ")")),
        grp3_c = html(paste("Total <br> (N=", bign[3], ")"))
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      ) %>%
      
      tab_style(
        style = list(
          cell_fill(color = "#D3D3D3")
        ),
        locations = cells_body(
          
          rows = AEBODSYS==term
        )
      ) %>%
      cols_hide(
        columns = c(AEBODSYS)
      ) 
    
    return(tab_html)
    
  })
  
  
  returnTable5 <- reactive({
    
   
    
    
    do_shift <- function(mydf, mytrt, qmytrt){
      
      # get the post-baseline data in ADLB
      post <- mydf[(mydf$ADT>mydf$TRTSDT),]
      
      # sort post by USUBJID, PARAM and ATOXGRN
      post <- post[order(post$USUBJID,post$PARAM, post$ATOXGRN),]
      post$lastdot <- !duplicated(post[c("USUBJID","PARAM")],fromLast=TRUE)
      
      # keep the most severe record for each USUBJID, PARAM
      post <- post[(post$lastdot==TRUE),]
      
      
      # do the counting by TRT01A, PARAM, BTOXGRN, ATOXGRN
      
      count0 <- 
        post %>%                   
        group_by({{mytrt}}, PARAM, BTOXGRN, ATOXGRN) %>%
        summarise(unique_subj = n_distinct(USUBJID))
      
      # generate a frame data called comb
      # it has all the combinations of ATOXGRN, BTOXGRN, PARAM and TRT01A
      
      mat <- matrix(NA, nrow = 25, ncol = 4)
      xy <- data.frame(mat)
      xy1 <- data.frame(mat)
      
      for (i in seq(1,25)){
        xy[[i,1]] <- data.frame(X1=i%%5)
        xy[[i,2]] <- data.frame(X2=ceiling(i/5)-1)
      }
      
      for (i in seq(1:length(unique(count0$PARAM)))){
        xy$X3 <- unique(count0$PARAM)[i]
        assign(paste0('xy', i, sep=''),  xy)
      }
      
      comb <- do.call("rbind", mget(sprintf("xy%d", 1:length(unique(count0$PARAM)))))
      
      for (i in seq(1:length(unique(count0[[qmytrt]])))){
        comb$X4 <- unique(count0[[qmytrt]])[i]
        assign(paste0('comb', i, sep=''),  comb)
      }
      
      comb <- do.call("rbind", mget(sprintf("comb%d", 1:length(unique(count0[[qmytrt]])))))
      
      comb$PARAM <- comb$X3
      comb[[qmytrt]] <- comb$X4
      comb$BTOXGRN <- as.numeric(unlist(comb$X1))
      comb$ATOXGRN <- as.numeric(unlist(comb$X2))
      
      # merge comb with count0
      m_count0 <- merge(count0, comb, by=c({{qmytrt}}, "PARAM", "BTOXGRN", "ATOXGRN"), all=TRUE)
      m_count0$denom <- ifelse(m_count0[[qmytrt]]=='grp1', bign[1], bign[2])
      m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$denom, 1), nsmall = 1), ")"))
      
      
      # do the transpose 
      count1 <- m_count0[(m_count0[[qmytrt]]=="grp1"),]
      count2 <- m_count0[(m_count0[[qmytrt]]=="grp2"),]
      
      a1 <- count1 %>%
        pivot_wider(id_cols=c(PARAM, BTOXGRN), names_from = ATOXGRN, values_from = value,
                    names_prefix = "grade")
      
      a2 <- count2 %>%
        pivot_wider(id_cols=c(PARAM, BTOXGRN), names_from = ATOXGRN, values_from = value,
                    names_prefix = "grad")
      
      a1 <- a1[c("PARAM","BTOXGRN","grade0", "grade1", "grade2", "grade3", "grade4" )]
      a2 <- a2[c("grad0", "grad1", "grad2", "grad3", "grad4" )]
      
      
      # combine a1 and a2
      
      df_merge <- cbind(a1,a2)
      df_merge$prefix <- 'Grade'
      df_merge$BTOXGRC <- paste(df_merge$prefix, df_merge$BTOXGRN)
      df_merge[c("PARAM","BTOXGRC","grade0", "grade1", "grade2", "grade3", "grade4","grad0", "grad1", "grad2", "grad3", "grad4"  )]
      
      
    }
    
    
    
    the_date <- as.character(Sys.Date())
    
    # read the data
    adsl <- read_sas("adsl2.sas7bdat")
    adlb <- read_sas("adlb2.sas7bdat")
    
    
    # get the big N in column headers from adsl
    bign <- table(group=adsl$TRT01A)
    
    
    df <- do_shift(mydf=adlb, mytrt=TRT01A, qmytrt="TRT01A")
    
    
    df %>% 
      gt(groupname_col="PARAM") 
    
    
    # use gt to do the reporting 
    tab_html <- df %>% 
      gt(groupname_col="PARAM") %>%
      
      tab_header(
        title = "Table 14.3.4. Shift Table from CTCAE Grade at Baseline to the Most Severe CTCAE Grade at Post-basline",
        subtitle = "Safety Population"
      ) %>%
      
      tab_source_note(
        source_note = paste('Program Source: shift.R            Executed: (Draft)',  the_date)
      ) %>%
      
      cols_label(
        
        
        BTOXGRC = html("Baseline CTCAE Grade"),
        grade0 = html("Grade 0"), 
        grade1 = html("Grade 1"),
        grade2 = html("Grade 2"),
        grade3 = html("Grade 3"),
        grade4 = html("Grade 4"),
        grad0 = html("Grade 0"), 
        grad1 = html("Grade 1"),
        grad2 = html("Grade 2"),
        grad3 = html("Grade 3"),
        grad4 = html("Grade 4")
        
        
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      ) %>%
      tab_spanner(
        label = html(paste("Group 1 Most Severe Post-baseline <br> (N=", bign[1], ")")),
        columns = c(grade0, grade1, grade2, grade3, grade4)
      ) %>%
      tab_spanner(
        label = html(paste("Group 2 Most Severe Post-baseline <br> (N=", bign[2], ")")),
        columns = c(grad0, grad1, grad2, grad3, grad4)
      ) %>%
      
      cols_align(
        align = "left",
        columns = c(BTOXGRC)
      )
    return(tab_html)
    
    
    
  })
  
  
 
  
  
  output$table_demog <- render_gt(
    
   expr = return(returnTable1()), 
    width=px(1500)
  )
  
  output$listing_demog <- renderTable(
    
    expr = return(returnTable2()), 
    width=px(1500)
  )
  
  output$table_adverse <- render_gt(
    
    expr = return(returnTable3()), 
    width=px(1500)
  )
  
  
  output$listing_adverse <- renderTable(
    
    expr = return(returnTable4()), 
    width=px(1500)
  )
  
  
  output$table_lab <- render_gt(
    
    expr = return(returnTable5()), 
    width=px(1500)
  )
  
  output$listing_lab <- renderTable(
    
    expr = return(returnTable6()), 
    width=px(1500)
  )
  
  
 
  
  
  output$figure_demog <- renderPlot({
    vs <- read_sas("class.sas7bdat")
    vs1 <- vs
    vs2 <- vs
    
    vs1$value <- vs1$Race
    vs1$var <- 'Race'
    vs2$value <- vs1$Sex
    vs2$var <- 'Sex'
    vs3 <-rbind(vs1, vs2)
    
    
   
    data.frame(vs3)
    
    dataInput1 <- reactive({ 
      vs3[(vs3$var == input$Variable),]
      
    })
    
    
    agg <- count(dataInput1(), value)
    agg <- agg[c("value","n")]
    
    myPlot <- pie(agg$n, labels=agg$value)
    print(myPlot)
    
 

    
  })
  
  
  
  output$figure_lab <- renderPlot({
    lb <- read_sas("adlb2.sas7bdat")
    
    
    dataInput <- reactive({ 
      lb[(lb$PARAM == input$Parameter),]
      
    })
    
    x    <- dataInput()$aval
    x    <- na.omit(x)
    
    
    boxplot(x)
    
    
    
    myPlot <- boxplot(x)
    print(myPlot)
    
    
    
    
  })
  
  
  output$figure1_eff <- renderPlot({
  
    # Read in the SAS data
    adtr <- read_sas("adtr.sas7bdat")
    
   
    data.frame(adtr)
    
    
    tr <- adtr[(adtr$PARAMCD=='BESTPCHG' & adtr$PARQUAL=='CENTRAL'),]
    

    
    tr$x <- tr$SUBJID
    tr$bestpchg <- tr$AVAL
    
    tr <- tr[c("x","bestpchg", "COHORT")]
    
    
    data.frame(tr)
    
    tr$x <- factor(tr$x,                                    # Factor levels in decreasing order
                   levels = tr$x[order(tr$bestpchg, decreasing = TRUE)])
    
    tr <- tr[order(-tr$bestpchg),]
    
    print(tr)
    
    
    dataInput <- reactive({ 
      tr[(tr$COHORT == input$cohort),]
      
    })
    
    
   
    
    
      
  
      myPlot <- ggplot(dataInput(), aes( x = x, y = bestpchg)) + 
        labs(title = "Waterfall plot for best percent change from baseline in tumor size", 
             x = "Patient ID", y = "Best percent change from baseline(%)") +
        theme(axis.text.x = element_blank()) +
        geom_col( width = 0.9)
      print(myPlot)
    
    
    
    
  })
  
  
  output$figure2_eff <- renderPlot({
    
    # Read in the SAS data
    adtr <- read_sas("adtr.sas7bdat")
    
    
    data.frame(adtr)
    
    
    tr <- adtr[(adtr$PARAMCD=='SUMDIAM' & adtr$PARQUAL=='INVESTIGATOR' & adtr$ANL01FL=='Y'),]
    
    tr <- tr[c("ADY","PCHG","SUBJID","AVISITN","COHORT")]
    
    print(tr)
    
    
    tr$x <- ifelse(tr$AVISITN==1, 0, tr$ADY/7)
    tr$y <- ifelse(tr$AVISITN==1, 0, tr$PCHG)
    
    data.frame(tr)
    
    print(tr)
    
    
    dataInput <- reactive({ 
      tr[(tr$COHORT == input$Cohort),]
      
    })
    
    
    
    myPlot <- ggplot(dataInput(), aes(x = x, y = y, group=SUBJID)) + 
      labs(title = "Spider plot for percent change from baseline in tumor size", 
           x = "Treatment duration weeks", y = "Percent change from baseline(%)") +
      
      geom_line(size=1) +
      geom_point( size=2)
    print(myPlot) 
    
    
    
    
  
  })
  
  
}
shinyApp(ui=ui, server=server)
    