# 06-navlist.R

library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(tidyr)
library(shiny)
library(plotly)
library(tidyverse)
library(lemon)
library(jsonlite)
library(corrplot)
library(lattice)
library(latticeExtra)
library(GGally)

###Polecam otwierac appke w przegladarce

theme_set(theme_minimal())



#nalezy ustawic wlasny folder z wgranymi plikami z danymi
setwd("/")
#########################################################################################





#pobranie i obróbka danych historycznych o populacji krajów - total i plcie
pop_hist_MFT<-read.csv("pop_hist_MFT.csv", sep=",", dec=".", na.strings=c("","NA"))
pop_hist_MFT$Value<-str_replace_all(string=pop_hist_MFT$Value, pattern=" ", repl="")
class(pop_hist_MFT$Value)<-"numeric"

#### rozsmarowanie wszystkich danych do postacj pivotowej - M, F, T w kolumnach
pop_hist_MFT<-pivot_wider(pop_hist_MFT, names_from=SEX, values_from=Value)

### trzeba zmienic wszystkie wiersze z dlugimi nazwami 'Germany' i 'Kosovo' na krotkie
pop_hist_MFT<-transform(pop_hist_MFT, GEO=ifelse(substr(GEO,1,4)=='Germ','Germany',GEO))
pop_hist_MFT<-transform(pop_hist_MFT, GEO=ifelse(substr(GEO,1,4)=='Koso','Kosovo',GEO))


#do wykresu1

data_subset<-pop_hist_MFT[,'Total']
pop_hist_MFT<-pop_hist_MFT[complete.cases(data_subset),]

#do wykresu2


pop_hist_MFT_geo_by_col<-pop_hist_MFT
pop_hist_MFT_geo_by_col %>% select( TIME, GEO, Total) %>% 
pivot_wider(names_from = GEO, values_from = Total) -> pop_hist_MFT_geo_by_col


##do wykresu3
pop_hist_age_groups<-read.csv("pop_hist_age_groups.csv", sep=",", dec=".", na.strings=c("","NA"))
pop_hist_age_groups$Value<-str_replace_all(string=pop_hist_age_groups$Value, pattern=" ", repl="")
class(pop_hist_age_groups$Value)<-"numeric"

pop_hist_age_groups<-transform(pop_hist_age_groups, GEO=ifelse(substr(GEO,1,4)=='Germ','Germany',GEO))
pop_hist_age_groups<-transform(pop_hist_age_groups, GEO=ifelse(substr(GEO,1,4)=='Koso','Kosovo',GEO))

pop_hist_age_groups<-filter(pop_hist_age_groups, substr(GEO,1,4)!='Euro')

pop_hist_age_groups_by_col<-pop_hist_age_groups
pop_hist_age_groups_by_col %>% select( TIME, AGE, SEX, GEO, Value) %>% 
pivot_wider(names_from = GEO, values_from = Value) -> pop_hist_age_groups_by_col

pop_hist_age_groups_by_col<-filter(pop_hist_age_groups_by_col, SEX!="Total")
pop_hist_age_groups_by_col<-filter(pop_hist_age_groups_by_col, AGE!="Total" & AGE!="Unknown")

#zmiana etykiet do piramidy wieku
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="Less than 5 years"]<-"A) Less than 5 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 5 to 9 years"]<-"B) From 5 to 9 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 10 to 14 years"]<-"C) From 10 to 14 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 15 to 19 years"]<-"D) From 15 to 19 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 20 to 24 years"]<-"E) From 20 to 24 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 25 to 29 years"]<-"F) From 25 to 29 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 30 to 34 years"]<-"G) From 30 to 34 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 35 to 39 years"]<-"H) From 35 to 39 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 40 to 44 years"]<-"I) From 40 to 44 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 45 to 49 years"]<-"J) From 45 to 49 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 50 to 54 years"]<-"K) From 50 to 54 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 55 to 59 years"]<-"L) From 55 to 59 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 60 to 64 years"]<-"M) From 60 to 64 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 65 to 69 years"]<-"N) From 65 to 69 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 70 to 74 years"]<-"O) From 70 to 74 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 75 to 79 years"]<-"P) From 75 to 79 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="From 80 to 84 years"]<-"R) From 80 to 84 years"
pop_hist_age_groups_by_col[pop_hist_age_groups_by_col=="85 years or over"]<-"S) 85 years or over"

 


##do wykresu4
pop_hist_broad_age<-read.csv("pop_hist_broad_age.csv", sep=",", dec=".", na.strings=c("","NA"))
pop_hist_broad_age$Value<-str_replace_all(string=pop_hist_broad_age$Value, pattern=" ", repl="")
class(pop_hist_broad_age$Value)<-"numeric"
pop_hist_broad_age<-transform(pop_hist_broad_age, GEO=ifelse(substr(GEO,1,4)=='Germ','Germany',GEO))
pop_hist_broad_age<-transform(pop_hist_broad_age, GEO=ifelse(substr(GEO,1,4)=='Koso','Kosovo',GEO))
pop_hist_broad_age<-select(pop_hist_broad_age, TIME, GEO, AGE, Value)
pop_hist_broad_age<-filter(pop_hist_broad_age, AGE!="Total" & AGE!="Unknown")
pop_hist_broad_age<-filter(pop_hist_broad_age, Value!="NA")





##do wykresów dalszych
fertility<-read.csv("fertility.csv", sep=",", dec=".", na.strings=c("","NA"))
fertility$Value<-str_replace_all(string=fertility$Value, pattern=" ", repl="")
class(fertility$Value)<-"numeric"



abortion<-read.csv("abortion.csv", sep=",", dec=".", na.strings=c("","NA"))
abortion$Value<-str_replace_all(string=abortion$Value, pattern=" ", repl="")
class(abortion$Value)<-"numeric"


abortion<-subset(abortion, select = -c(UNIT))

###DOPISAC na wyjsciowych tabelach dane do konca

divorces<-read.csv("divorces.csv", sep=",", dec=".", na.strings=c("","NA"))
divorces$Value<-str_replace_all(string=divorces$Value, pattern=" ", repl="")
class(divorces$Value)<-"numeric"


marriages2<-read.csv("marriages2.csv", sep=",", dec=".", na.strings=c("","NA"))
marriages2$Value<-str_replace_all(string=marriages2$Value, pattern=" ", repl="")
class(marriages2$Value)<-"numeric"


pop_structure<-read.csv("pop_structure.csv", sep=",", dec=".", na.strings=c("","NA"))
pop_structure$Value<-str_replace_all(string=pop_structure$Value, pattern=" ", repl="")
class(pop_structure$Value)<-"numeric"


live_births<-read.csv("live_births.csv", sep=",", dec=".", na.strings=c("","NA"))
live_births$Value<-str_replace_all(string=live_births$Value, pattern=" ", repl="")
class(live_births$Value)<-"numeric"
live_births<-subset(live_births, select = -c(UNIT))

colnames(live_births)<-c("TIME","GEO","INDIC_DE","Value")
live_births[live_births=="Total"]<-"Live births"


#mergowanie powyzszych tabel
fertility %>% rbind(abortion) %>% rbind(divorces) %>% rbind(marriages2) %>% rbind(pop_structure) %>% rbind(live_births) -> demo_ratios
tail(demo_ratios)




demo_ratios<-transform(demo_ratios, GEO=ifelse(substr(GEO,1,4)=='Germ','Germany',GEO))
demo_ratios<-transform(demo_ratios, GEO=ifelse(substr(GEO,1,4)=='Koso','Kosovo',GEO))
#`France (metropolitan)` pozostawiam ze wzgledu na brak niektorych danych w wierszach 'France'

 demo_ratios_wide<-pivot_wider(demo_ratios, names_from=INDIC_DE, values_from=Value)



###tutaj jest dodatkowo SEX i AGE, wiec bedzie w osobnej tabeli
life_exp<-read.csv("life_exp.csv", sep=",", dec=".", na.strings=c("","NA"))
life_exp$Value<-str_replace_all(string=life_exp$Value, pattern=" ", repl="")
class(life_exp$Value)<-"numeric"


life_exp<-transform(life_exp, GEO=ifelse(substr(GEO,1,4)=='Germ','Germany',GEO))
life_exp<-transform(life_exp, GEO=ifelse(substr(GEO,1,4)=='Koso','Kosovo',GEO))

life_exp_wide<-pivot_wider(life_exp, names_from=SEX, values_from=Value)
life_exp_wide<-subset(life_exp_wide, select = -c(AGE, INDIC_DE))


life_exp_wide<-filter(life_exp_wide, Total!="NA")




demo_ratios_wide[,c("TIME", "GEO", "Live births")] -> live_births_clear
na.omit(live_births_clear)->live_births_clear



demo_ratios_wide %>% subset(select = c("TIME", "GEO",
  "Total fertility rate",
  "Mean age of women at childbirth",
  "Proportion of live births outside marriage",
  "Mean age at first marriage - females",
  "Mean age of women at birth of first child",
  "Divorces per 100 marriages",
  "Proportion of population aged 0-17 years",
  "Proportion of population aged 65 years and more",
  "Crude marriage rate"))%>% na.omit() -> demo_ratios_sel_cor

cor(subset(filter(demo_ratios_sel_cor, TIME==2017), select = -c(TIME, GEO)))->demo_ratios_sel_cor_matrix_2017
colnames(demo_ratios_sel_cor_matrix_2017)<- c("FertRate", "MeanAgeatChildbirthF", "%births outside marriage","MeanAge1marriageF", "MeanAgeat1ChildbirthF", "Divorces/100Mgs",                     
"%pop_0-17Y","%pop_65+","CrudeMarriageRate")


col3 <- colorRampPalette(c("red", "white", "blue"))



subset(life_exp_wide, select = c("TIME", "GEO", "Total"))->life_exp_wide_total 
colnames(life_exp_wide_total)<-c("TIME", "GEO", "Life expectancy")
merge(life_exp_wide_total, demo_ratios_wide, by = c("TIME", "GEO")) -> demo_ratios_life_exp




ui <- navbarPage(title = "Demography of Europe. Project by Emil Korbus",
                 tabPanel(title = "Populations",
                          plotOutput(outputId = "znacznik1"),
                          sliderInput(inputId = "year_chosen_1",
                                      label = "Select the year",
                                      min = 1960,
                                      max = 2019,
                                      value = 2000,
                                      sep="")
                 ),
                 tabPanel(title = "Populations 1on1",
                          plotOutput(outputId = "znacznik2"),
            
                          selectInput(inputId = "country_chosen_1",
                                      label = em("Select country 1", style = "color:#ff3311"),
                                      choices = sort(unique(pop_hist_MFT$GEO)),
                                      selected = "Poland"
                                      ),
                        selectInput(inputId = "country_chosen_2",
                                    label = em("Select country 2", style = "color:#3311ff"),
                                    choices = sort(unique(pop_hist_MFT$GEO))
                                    ),
                        sliderInput(inputId = "range_of_years_pop",
                                    label = "Select time range",
                                    min = 1960,
                                    max = 2019,
                                    value = c(1990,2010),
                                    sep=""
                                    )
                ),
                
                 tabPanel(title = "Age pyramid",
                          plotOutput(outputId = "pyramid"),
                          selectInput(inputId = "country_chosen_3",
                                      label = "Select country",
                                      choices = sort(unique(pop_hist_MFT$GEO)),
                                      selected = "Poland"
                                      ),
                          
                          sliderInput(inputId = "year_chosen_3",
                                      label = "Select year",
                                      min = 1960,
                                      max = 2019,
                                      value = 1990,
                                      sep=""
                                      )
                          ),
                tabPanel(title = "Key age groups",
                         plotOutput(outputId = "part_of_pyramid"),
                    
                         sliderInput(inputId = "year_chosen_4",
                                     label = "Select year",
                                     min = 1960,
                                     max = 2019,
                                     value = 1990,
                                     sep=""
                                      )
                ),
                
                tabPanel(title = "Life expectancy by country",
                         plotOutput("life_exp_by_country"),
                         plotOutput("life_exp_by_country_MF"),
                         sliderInput("life_exp_by_country_year",
                                     "Select year",
                                     min = 1960,
                                     max = 2018,
                                     value = 2000,
                                     sep="")
                ),
                
                tabPanel(title = "Live births",
                         plotOutput("births"),
                         sliderInput("births_year",
                                     "Select year",
                                     min = 1960,
                                     max = 2018,
                                     value = 2010,
                                     sep="")
                ),
                
                
                tabPanel(title = "Variables correlation review",
                         plotOutput("demo_cor")
                      
                ),
                
                

                 tabPanel(title = "Marriages vs. live births",
                          plotOutput("mar_vs_lb"),
                          checkboxGroupInput(inputId = "mar_vs_lb_countries", label = "Select countries to plot", choices = sort(unique(demo_ratios_wide$GEO)), 
                                             selected = c("Poland", "Germany", "Ukraine", "France"),
                                             inline = TRUE),
                          
            
                 ),


                tabPanel(title = "Factors influencing population size",
                         plotOutput("two_xy_with_panels2"),
                         plotOutput("two_xy_with_panels3"),
                         plotOutput("two_xy_with_panels1"),
                         checkboxGroupInput(inputId = "two_xy_countries", label = "Select countries to plot", choices = sort(unique(demo_ratios_life_exp$GEO)),
                                            selected = c("Poland", "Ukraine", "France"),
                                            inline = TRUE)
                        )
                
                
)         




server <- function(input, output) {
  
  
  
  #wykres 1 - populacje poszczegolnych krajow w danym roku z dynamicznym sortowaniem krajow na osi Y
  output$znacznik1 <- renderPlot({

    ggplot(subset(pop_hist_MFT, TIME == input$year_chosen_1), aes(x=reorder(GEO,Total), y=Total)) +
      geom_bar(stat="identity", fill=ifelse(subset(pop_hist_MFT, TIME == input$year_chosen_1)$GEO=='Poland','red', 'grey')) +
      coord_flip() +
      scale_y_continuous(labels=number, breaks = seq(0,160000000,by=20000000)) +
      labs(x="", y = "Total population", title=input$year_chosen_1) +
    theme (axis.title = element_text(size=13,face="bold"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 13)
           )
  })
  output$znacznik2 <- renderPlot({
   # kod kodujacy wykres z dwoma liniami dla dwoch krajow country_chosen_1 i country_chosen_2, subset(pop_hist_MFT, GEO == input$country_chosen_1
  
   ggplot(pop_hist_MFT_geo_by_col, aes(x=TIME)) +
      geom_line(aes(y=get(input$country_chosen_1), color = "red"), show.legend = FALSE) + 
      geom_line(aes(y=get(input$country_chosen_2), color = "blue"), show.legend = FALSE) + 
      geom_point(aes(y=get(input$country_chosen_1), color = "red"), show.legend = FALSE) + 
      geom_point(aes(y=get(input$country_chosen_2), color = "blue"), show.legend = FALSE) + 
      scale_color_manual(values=c("#9999CC", "#CC6666")) +
      scale_x_continuous(labels=number, breaks = input$range_of_years_pop) +
      scale_y_continuous(labels=number) +
      labs(x="Year", y = "Population") +
      xlim(input$range_of_years_pop) +
      ####tutaj vlookupowac wartosci w latach pocz. i konc i policzyc zmiane
      ggtitle ("Comparison of population size between countries over time") +
      theme(plot.title = element_text(color = "slategrey", face = "bold", hjust = 0.5, size = 13),
            axis.title = element_text(color = "slategrey", size=13,face="bold")) ##tutaj zmienic tytul
    
 
  })
  
  output$pyramid <- renderPlot({

   
      ggplot(subset(pop_hist_age_groups_by_col, TIME == input$year_chosen_3), aes(y = ifelse(SEX == "Males", -get(input$country_chosen_3), get(input$country_chosen_3)), x = AGE, fill = SEX)) +
        geom_bar(stat="identity") +
        # scale_x_symmetric(labels = abs) +
        labs(x = "Age group",  y="Population size of the group") +
        coord_flip() + 
        scale_y_continuous(labels=number_format()) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13, color = "slategrey"),
          axis.title = element_text(size=13,face="bold", color = "slategrey"))
    

  })
  
  
 

  

  
  output$part_of_pyramid <- renderPlot({

    
    ggplot(data = subset(pop_hist_broad_age, TIME == input$year_chosen_4), aes(fill = AGE, x = sort(GEO), y = Value)) +
      geom_bar(position="fill", stat="identity", alpha=0.7) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.major.y = element_blank(),
            axis.title = element_text(size=13,face="bold", color = "slategrey"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 13, color = "slategrey")) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",  y="Percentage of population") 
    
   
  })
  
  
  output$fert <- renderPlot({
    ggplot(data = subset(demo_ratios_wide, TIME == input$year_chosen_4), aes(x = sort(GEO), y = `Total fertility rate`)) +
      geom_point(stat="identity") +
      theme(axis.text.x = element_text(angle = 90))
    
    
  })
  
  
  output$life_exp_by_country <- renderPlot({
    #life_exp_by_country_year
    ggplot(data = subset(life_exp_wide, TIME == input$life_exp_by_country_year), aes(x=Total, y=reorder(GEO,Total))) +
      geom_point(color = rgb(0.1, 0.1, 0.7, 0.5), size = 3) +
      geom_segment(aes(x = 60, xend = Total, y = reorder(GEO, Total), yend = reorder(GEO, Total)), color = "grey") +
      scale_x_continuous(limits = c(60,90), breaks = seq(60,90, by = 5)) +
      labs(title = "Life expectancy at birth - average", x = "years", y="") +
     # theme_minimal() +
      theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      #axis.text=element_text(size=12),
      axis.title=element_text(size=12, color = "slategrey"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13, color = "slategrey")
                              )
      
  })
  
  
  output$life_exp_by_country_MF <- renderPlot({
    ggplot(data = subset(life_exp_wide, TIME == input$life_exp_by_country_year)) +
      geom_segment( aes(x=Males, xend=Females, y=reorder(GEO, Total), yend=reorder(GEO, Total)), color="grey") +
      geom_point( aes(x=Males, y=reorder(GEO,Total)), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
      geom_point( aes(x=Females, y=reorder(GEO,Total)), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
      scale_x_continuous(limits = c(60,90), breaks = seq(60,90, by=5)) +
      labs(title = "Life expectancy at birth - male vs. female", x = "years", y="") +
      #coord_flip()
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text=element_text(size=12),
        axis.title=element_text(size=12, color = "slategrey"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 13, color = "slategrey"))
  
  })
 
  
  output$births <- renderPlot({
    #pop_hist_MFT_by_year<-filter(pop_hist_MFT, TIME==input$year_chosen_1)
    ggplot(data = subset(live_births_clear, TIME == input$births_year), aes(x=reorder(GEO,`Live births`), y=`Live births`)) +
      geom_bar(stat="identity", fill= 'steelblue2', na.rm = TRUE, alpha = 0.8) +
      coord_flip() +
      scale_y_continuous(labels=number, breaks = seq(0,2000000,by=200000)) +
      geom_bar(stat="identity", fill=ifelse(subset(live_births_clear, TIME == input$births_year)$GEO=='Poland','red', 'steelblue2')) +
      labs(x="", y = "Number of live births per year", title=input$births_year) +
      theme (axis.title = element_text(color = "slategrey", size=13,face="bold"),
             plot.title = element_text(color = "slategrey", face = "bold", hjust = 0.5, size = 13)
      )
  })
  
  
  
  output$demo_cor <- renderPlot({
    corrplot(demo_ratios_sel_cor_matrix_2017, title = "Correlations between selected variables for 30 countries - 2017", method = "number",
             type="upper", order="hclust", tl.col="slategrey", mar=c(0,0,2,0), col =col3(20))
    
  })
  
 
  output$mar_vs_lb <- renderPlot({
    xyplot(`Live births`~ `Marriages`  | GEO , 
           data=demo_ratios_wide[demo_ratios_wide$GEO %in% input$mar_vs_lb_countries,] ,
           main=list(label = "Relation between number of marriages per year and number of live births", col = "slategrey"),
           ylab=list(col = "slategrey"),
           xlab=list(col = "slategrey"),
           pch=20 , cex=3 , col=rgb(0.2,0.4,0.8,0.5),
           panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x)
                     )
           }
           )
  })
  
  ##################2 ostatnie wykresy do jednej zakladki
  output$two_xy_with_panels1 <- renderPlot({
    xyplot(`Mean age at first marriage - females`  + `Mean age of women at birth of first child` ~ TIME  | GEO ,
           data=demo_ratios_life_exp[demo_ratios_life_exp$GEO %in% input$two_xy_countries,] ,
           main=list(label = "Age at birth of 1st child vs Age at 1st marriage", col = "slategrey"),
           ylab=list(col = "slategrey", label = "years"),
           xlab=list(col = "slategrey"),
           pch=19 , cex=0.8 , 
           superpose=T,
           type = c("l", "p"),
           auto.key = TRUE
           )

  })
  
  
  output$two_xy_with_panels2 <- renderPlot({
    xyplot(`Life expectancy` ~ TIME  | GEO ,
           data=demo_ratios_life_exp[demo_ratios_life_exp$GEO %in% input$two_xy_countries,] ,
           main=list(label = "Life expectancy", col = "slategrey"),
           ylab=list(col = "slategrey", label = "years"),
           xlab=list(col = "slategrey"),
           pch=19 , cex=0.8 , 
           superpose=T,
           type = c("l", "p"),
           col = "red"
    )
    
  })
  
  
  
  
  
  output$two_xy_with_panels3 <- renderPlot({
    xyplot(`Total fertility rate` ~ TIME  | GEO ,
           data=demo_ratios_life_exp[demo_ratios_life_exp$GEO %in% input$two_xy_countries,] ,
           main=list(label = "Total fertility rate", col = "slategrey"),
           ylab=list(col = "slategrey", label = "# of children who would be born on avg by 1 woman"),
           xlab=list(col = "slategrey"),
           pch=19 , cex=0.8 , 
           superpose=T,
           type = c("l", "p"),
           col = c("green")
    )
    
  })
  
 
  
  
  
}

shinyApp(server = server, ui = ui)