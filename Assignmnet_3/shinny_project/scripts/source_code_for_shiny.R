library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(ggvis)
library(plotly)
library(metathis)
library(formattable)
# devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)
library(here)

options(scipen=999)
theme_set(theme_minimal())
data <- readr::read_csv(here("data/final_data_mri.csv"))

### FUNCTIONS & TEXT

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text8
    )
  )
}

text0 <- HTML("<span style='font-size:20px'> Background: </span>
              <br><br> 
              <p>  A cohort study of adults aged 65 years and older was conducted to observe the incidence of cardiovascular disease (especially heart attacks and congestive heart failure) and cerebrovascular disease (especially strokes) in the elderly over an 11 year period, and to relate the incidence of those diseases to various risk factors measured in the population on a regular basis. This is of particular importance, because there is increasing evidence that some of the associations observed between cardiovascular or cerebrovascular disease and various risk factors in middle aged adults are not observed in older adults. In this study, elderly, generally healthy, adults were randomly selected from Medicare rolls. Agreement to participate was high, and thus the sample can be regarded as a fairly accurate representation of healthy older Americans. At the time of study enrollment, and on annual visits over the length of the study, the participants' data regarding various behavioral (e.g., smoking, alcohol consumption), functional (e.g., ability to perform routine tasks), and clinical (e.g., blood pressure, laboratory tests, forced exhaled volume) measures are recorded. For this analysis and visualization I will focus on the measure of forced expiratory volume (FEV). <br><br> A measure (in liters per second) of forced expiratory volume in the participant at the time of MRI. FEV measures the volume of air that can be forcibly exhaled within 1 second. Normal FEV measurements depend upon the size of the lungs, which in turn is usually proportional to body size. In addition, FEV is highly impacted by behavioral characteristics such as smoking.

<br><br>In this work I will visually explore the magnitude and type of association between FEV and body size approximated by height (centimeters) and and how they change when considering other variables such as, smoking behavior and birth assigned gender.<p>")

text1 <- HTML("<H2> Female smokers </H2>
              <br> <p> For <font color='#A00042'>Female smokers</font> population there seems to be an incarease 
              in FEV with height.<p>")

text2 <- HTML("<H2> Female non-smokers </H2>
              <br> <p>For <font color='#F56C42'> female non-smokers </font> population there seems to be an increasing first trend pattern.
              Comparing female <font color='#A00042'> smokers</font> with <font color='#F56C42'>non-smokers</font>, the non-smokers have higher mean FEV.
              However, the difference in the estimated mean for two groups of females differ in one centimeter of height, in which one group is smoker and the other non-smoker 
              is the same.
              <p>")

text3 <- HTML("<H2> Male smokers </H2>
              <br> <p>For <font color='#008640'>male smokers</font> there seems to be an increasing first trend pattern. 
              <p>")

text4 <- HTML("<H2> Male non-smokers </H2>
              <br> <p>For <font color='#3487BD'>male non-smokers </font> there seems to be an increasing first trend pattern.
              <Comparing male <font color='#008640'> smokers</font> with <font color='#3487BD'>non-smokers</font>, the non-smokers have higher mean FEV.
              However, the difference in the estimated mean for two groups of males differ in one centimeter of height, in which one group is smoker and the other non-smoker 
              is the same. <p>")
text8 <- HTML("<H2> Overall </H2>
               <p>Male study participants have higher height in centimeters compared to female participants.
              The rate of change of FEV is higher in male compared to female when looking at change in FEV between two groups of the same gender but differ in height by one centimeter.
              <p>")

concludingtext <- HTML("
<hr>
<p><span style='font-size:24px'><b>Interpretations</b></span>
                        <br>
                            <span style='font-size:18px'>The following are key points I expect the reader to gain from the plot:</span>
                        <br>
                        <ul>
                        <li>
                             The first linear trend of suggestive an increasing FEV with increasing height.
                        </li>
                            <li> The change in slope and intercept in the two fits, when considering the gender, clearly implies that gender is an effect modifier for the relationship between FEV and height. Therefore, it is necessary to use modeling methods that incorporate an interaction term.
</li>
                            <li>Male participants tend to be taller than female participants.
                       </li><li> There are more male smokers as compered to female smokers.<br>
                        </li><li> The estimated mean difference between two female study groups who varies by one centimeter of height is less than the estimated mean difference between two male study groups who varies by the same height.<br>
                       </li><li> Regardless of gender and height smokers tend to have smaller FEV<br>
                       </li><li> Looking at female data distributions, the difference in the slope of female smokers and non-smokers is very small. This implies, smoking is more associated to FEV rather height. This means smoking behavior explains more of the relationship between FEV and height making it a precision variable. This seems true for male study groups as well. However, I will take greater caution from concluding as such, since the slope for smoker and non-smoker males seems to be different (p-value will help in deciding this)</li></ul></p>")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Additional note</i></span><br>
                <br>
                <span style='font-size:12px'>
                The data has a total observation of 735.
                <br>
                 The data has a total of 57 missing values. No missing value imputation were performed in this work.
                 <br>
                New column <i>smoking_label</i> was created for representing female and male smokers and non-smokers.
                <br>
                <br>
                <b>The variables: </b>
                <ul>
                <li>
                <i>sex</i>: The sex of the participant. Only Male and Female are represented. In thus study Male would be represented by a value of 1, where as female by a value of zero. 
                </li>
                <li>
                <i>smoking_history</i>: Gender specific smoking habit. The variable is constructed from number of pack years smoked (pack years: number of packs of smoked per day for one year). 
                </li>
                 <li>
                <i>crt</i>: Measure of creatinine level measured by mg/dl. This is measured in labratory during the enrollement of study participants. 
                </li>
                <li>
                <i>height:</i> Height of the study participants measured in centimeters. 
                </li>
                <li>
                <i>fev:</i> This represents the Forced expiratory volume (FEV) measurement in litesr per second. This variable is a proxy indicator for how healthy participant's lung are. 
                </li>
                <li>
                <i>age:</i> Age of the study participants measured in years. 
                </li>
                <ul/>
                <br>
                <br>
                Some of the packages I have used are listed below: 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


### ALL PLOT OBJECTS

# helpers for all plots:
cols <- c('Female smoker' = '#A00042','Female non-smoker' = '#F56C42',
          "Male smoker" = '#008640', "Male non-smoker" = '#3487BD') 

legend_ord <- levels(with(data, reorder(smoking_label, reveal)))

## Intro plot
# Intro static ggplot
introggPlot <- data %>% 
  ggplot(aes(x=height, y=fev)) +
  geom_point(mapping=aes(#size=crt,
                         alpha= 1/7, col=smoking_label,
                         text = glue::glue('<span style = "font-size:1.0em">Patient ID: {ptid}</span><br>
                                                <i>Forced Expiratory Volume (FEV) </i>: {comma(fev, digits = 0)} liters/second
                                                <i>Height</i>: {comma(height, digits = 0)} centimeters
                                                <i>Age</i>: {comma(age, digits = 0)} years
                                                <i>Creatinine level</i>: {comma(crt)} milligrams / decilitre
                                                <i>Smoking and gender category</i>: {smoking_label}'))) +
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nHeight in centimeters") +
  ylab("Forced expiratory volume (FEV) \n liters per second") +
  geom_smooth()+
  labs(size= "", col= "", alpha = "") + 
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous( limits = c(138,200)) +
  scale_y_continuous( limits = c(0,5)) +

  theme(axis.line.x = ggplot2::element_line(colour = NULL, 
                                            size = NULL, linetype = NULL, lineend = NULL), 
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()) 

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.85, y = 0.985),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)
