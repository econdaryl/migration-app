library(readr)
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(sp)
library(rgdal)
library(gpclib)
library(maptools)
source("theme_map.R")
imm_dens <- read_csv("imm_dens.csv")
county <- map_data("county")
educ_state <- read_csv("educ_state.csv")
educ_state_new <- read_csv("educ_state_new.csv")
educ_state_region <- read_csv("educ_state_region.csv")
educ_state_region_new <- read_csv("educ_state_region_new.csv")
educ_state_region_sex <- read_csv("educ_state_region_sex.csv")
educ_state_region_sex_new <- read_csv("educ_state_region_sex_new.csv")
educ_state_sex <- read_csv("educ_state_sex.csv")
educ_state_sex_new <- read_csv("educ_state_sex_new.csv")

ui <- navbarPage("Immigration Data",
  tabPanel("Main",
           verbatimTextOutput("message")
  ),
  tabPanel("Where do they live?",
           sidebarLayout(
             sidebarPanel(
               selectInput("origin1", "Select Origin", c(levels(as.factor(imm_dens$origin))), "Total")
             ),
           mainPanel(plotOutput("densityMap"),
                     textOutput("mapnote"))
           )
  ),
  tabPanel("Education",
           sidebarLayout(
             sidebarPanel(
               selectInput("area", "Select Area", levels(as.factor(educ_state$statefip)), "National"),
               radioButtons("sex", "Sex", c("All", "Male", "Female")),
               radioButtons("new", "Arrival", c("All", "Recent Migrants")),
               selectInput("year", "Select Years", c("All", rev(levels(as.factor(educ_state$year))))),
               selectInput("region", "Select Region", c("All", levels(as.factor(educ_state_region$region))))
           ),
           mainPanel(plotOutput("plot"))
           )
  )
)

server <- function(input, output, session){
  ## Main
  output$message <- renderText("Hello! This is a shiny app to easily view immigration data on a variety of subjects.
  If you find errors or would like me to add something else, please let me know at 
  daryl (at) larsen (dot) org.")
  
  ## Where do they live
  temp <- reactive({
    imm_dens %>%
      full_join(county.fips, by = "fips") %>%
      separate(polyname, into = c("region", "subregion"), sep = ",") %>%
      full_join(county, c("region", "subregion")) %>%
      filter(origin == input$origin1)
  })
  pretty_breaks <- reactive({
    if (input$origin1 == "Total"){
      pretty_breaks <- c(1, 1.5, 2.5, 4, 7.5)
    } else if (input$origin1 == "Africa"){
      pretty_breaks <- c(0.000000001, 0.000000002, 0.03, 0.1, 0.25)
    } else if (input$origin1 == "Asia"){
      pretty_breaks <- c(0.01, 0.25, 0.5, 0.75, 1.5)
    } else if (input$origin1 == "Europe"){
      pretty_breaks <- c(0.1, 0.25, 0.4, 0.65, 1.5)
    } else if (input$origin1 == "Latin America"){
      pretty_breaks <- c(0.2, 0.5, 1, 2, 4.5)
    } else if (input$origin1 == "Northern America"){
      pretty_breaks <- c(0.0000001, 0.05, 0.01, 0.2, 0.35)
    } else if (input$origin1 == "Oceania"){
      pretty_breaks <- c(0.00000001, 0.00000002, 0.00000003, 0.02, 0.05)
    }
  })
  brks <- reactive({
    minVal <- min(temp()$imm_dens, na.rm = TRUE)
    maxVal <- max(temp()$imm_dens, na.rm = TRUE)
    c(minVal, pretty_breaks(), maxVal)
  })
  labels <- reactive({
    temp1 <- c()
    for (idx in 1:length(brks())){
      temp1 <- c(temp1, round(brks()[idx+1],2))
    }
    temp1[1:length(temp1)-1]
  })
  mapd <- reactive({
    temp() %>%
      mutate(brks = cut(temp()$imm_dens,
                    breaks = brks(),
                    labels = labels(),
                    include.lowest = TRUE))
  })
  brks_scale <- reactive({
    levels(mapd()$brks)
  })
  labels_scale <- reactive({
    rev(brks_scale())
  })
  output$densityMap <- renderPlot({
    ggplot() +
      geom_polygon(data = mapd(), aes(fill = brks,
                                    x = long,
                                    y = lat,
                                    group = group)) +
      geom_path(data = mapd(), aes(x = long,
                                 y = lat,
                                 group = group),
                color = "white", size = 0.075) +
      coord_equal() +
      theme_map() +
      theme(legend.position = "bottom") +
      labs(x = NULL,
           y = NULL,
           title = "Immigrants in the United States",
           subtitle = "Number per 100 People by County",
           caption = "Source: Census") +
      scale_fill_manual(
        values = rev(c("#004c6d","#346888","#5886a5","#7aa6c2","#9dc6e0","#c1e7ff")),
        breaks = rev(brks_scale()),
        name = "Percent Immigrants",
        drop = FALSE,
        labels = labels_scale(),
        guide = guide_legend(
          direction = "horizontal",
          keyheight = unit(2, units = "mm"),
          keywidth = unit(70 / length(labels()), units = "mm"),
          title.position = 'top',
          title.hjust = 0.5,
          label.hjust = 1,
          nrow = 1,
          byrow = T,
          reverse = T,
          label.position = "bottom"
        )
      )
  })
  output$mapnote <- renderText({
    "Note: Pay attention to the changing scale. These maps are meant to show regional variation, so a dark blue in the 'Total' map means something different than a dark blue in the 'Africa' map. To show variation better, each map has been cut into sextiles."
  })
  
  ## Education
  lined <- reactive({
    educ_state %>%
      filter(statefip == input$area) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linesd <- reactive({
    educ_state_sex %>%
      filter(statefip == input$area,
             sex == input$sex) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linerd <- reactive({
    educ_state_region %>%
      filter(statefip == input$area,
             region == input$region | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linend <- reactive({
    educ_state_new %>%
      filter(statefip == input$area,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linesrd <- reactive({
    educ_state_region_sex %>%
      filter(statefip == input$area,
             sex == input$sex,
             region == input$region | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linesnd <- reactive({
    educ_state_sex_new %>%
      filter(statefip == input$area,
             sex == input$sex,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linernd <- reactive({
    educ_state_region_new %>%
      filter(statefip == input$area,
             region == input$region | immigrant == 0,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  linesrnd <- reactive({
    educ_state_region_sex_new %>%
      filter(statefip == input$area,
             sex == input$sex,
             region == input$region | immigrant == 0,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Level", "Percent", college) %>%
      mutate(Percent = 100*Percent)
  })
  bard <- reactive({
    educ_state %>%
      filter(statefip == input$area,
             year == input$year) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barsd <- reactive({
    educ_state_sex %>%
      filter(statefip == input$area,
             year == input$year,
             sex == input$sex) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barrd <- reactive({
    educ_state_region %>%
      filter(statefip == input$area,
             year == input$year,
             region == input$region | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barnd <- reactive({
    educ_state_new %>%
      filter(statefip == input$area,
             year == input$year,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barsrd <- reactive({
    educ_state_region_sex %>%
      filter(statefip == input$area,
             year == input$year,
             sex == input$sex,
             region == input$region | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barsnd <- reactive({
    educ_state_sex_new %>%
      filter(statefip == input$area,
             year == input$year,
             sex == input$sex,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barrnd <- reactive({
    educ_state_region_new %>%
      filter(statefip == input$area,
             year == input$year,
             region == input$region | immigrant == 0,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  barsrnd <- reactive({
    educ_state_region_sex_new %>%
      filter(statefip == input$area,
             year == input$year,
             sex == input$sex,
             region == input$region | immigrant == 0,
             new_migrant == 1 | immigrant == 0) %>%
      gather("Education", "Percent", c(lths, hs, some_college, bachelor, graduate)) %>%
      mutate(Percent = 100*Percent)
  })
  output$plot <- renderPlot({
    if (input$year == "All" & input$sex == "All" & input$region == "All" & input$new == "All"){
      ggplot(data=lined(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex == "All" & input$region == "All" & input$new == "All"){
      ggplot(data=bard(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex != "All" & input$region == "All" & input$new == "All"){
      ggplot(data=linesd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex != "All" & input$region == "All" & input$new == "All"){
      ggplot(data=barsd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex == "All" & input$region != "All" & input$new == "All"){
      ggplot(data=linerd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex == "All" & input$region != "All" & input$new == "All"){
      ggplot(data=barrd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex == "All" & input$region == "All" & input$new != "All"){
      ggplot(data=linend(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex == "All" & input$region == "All" & input$new != "All"){
      ggplot(data=barnd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex != "All" & input$region != "All" & input$new == "All"){
      ggplot(data=linesrd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex != "All" & input$region != "All" & input$new == "All"){
      ggplot(data=barsrd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex != "All" & input$region == "All" & input$new != "All"){
      ggplot(data=linesnd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex != "All" & input$region == "All" & input$new != "All"){
      ggplot(data=barsnd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex == "All" & input$region != "All" & input$new != "All"){
      ggplot(data=linernd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex == "All" & input$region != "All" & input$new != "All"){
      ggplot(data=barrnd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    } else if (input$year == "All" & input$sex != "All" & input$region != "All" & input$new != "All"){
      ggplot(data=linesrnd(), aes(x = year, y = Percent, color = as.factor(immigrant))) +
        geom_line() +
        scale_color_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Year") +
        ylab("") +
        ggtitle("Percent with College Degree by Migration Status") +
        theme_classic()
    } else if (input$year != "All" & input$sex != "All" & input$region != "All" & input$new != "All"){
      ggplot(data=barsrnd(), aes(x = as.factor(Education), y = Percent, fill = as.factor(immigrant))) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_x_discrete(limits = c("lths", "hs", "some_college", "bachelor", "graduate"), labels = c("No Diploma", "High School Degree", "Some College", "Bachelor's Degree", "Advanced Degree")) +
        scale_fill_discrete(name = "Immigrant Status", labels = c("Non-Immigrant", "Immigrant")) +
        xlab("Level of Education") +
        ylab("") +
        ggtitle("Levels of Education by Migration Status") +
        theme_bw()
    }
  })
}

shinyApp(ui = ui, server = server)
