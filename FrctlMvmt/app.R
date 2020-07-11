#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(reshape2)


# DATA ####
# sh.get <- sheets_get("https://docs.google.com/spreadsheets/d/1CRMO5nmankLgRJb9FOMWUS9uA_MdAMtJ-Xh2zBxlEb4/edit#gid=0")
# mvmts.0 <- sheets_read(ss = sh.get$spreadsheet_url, sheet = 1, skip = 1)
# saveRDS(mvmts.0, "./FrctlMvmt/RDS/mvmts.0.RDS")
message(getwd())
mvmts.0 <- 
    #mvmts.0 %>% 
    readRDS("./RDS/mvmts.0.RDS") %>% 
    filter(Keep) %>% 
    select(-Archetype, -Keep)

mvmts.mat <- 
    mvmts.0 %>% 
    select(-Movement) %>% 
    as.matrix()
rownames(mvmts.mat) <- mvmts.0$Movement
#mvmts.mat


# > DISTANCE ####
mvmts.dist <- 
    mvmts.mat %>% 
    dist(method = "binary", diag = TRUE, upper = TRUE)
#str(mvmts.dist)

# Note: the dist matrix computes all pairwise distances between cases
#choose(nrow(mvmts.0), 2)

mvmts.dist.df <- 
    mvmts.dist %>% 
    as.matrix() %>% 
    as.data.frame()


# > CLUSTER ####
# mvmts.dist %>%
#     #hclust(method = "ward.D") %>%
#     hclust(method = "complete") %>%
#     plot()

mvmts.hc <- 
    mvmts.dist %>%
    hclust(method = "complete")

colors = c("red", "blue", "green", "black", "orange", "purple")
clus4 = cutree(mvmts.hc, 6)
ape::plot.phylo(ape::as.phylo(mvmts.hc), type = "fan", tip.color = colors[clus4], label.offset = 0, cex = 0.7)

# > MARKOV ####
# simulate discrete Markov chains according to transition matrix P
runMC <- function(P = as.matrix(mvmts.dist), init_state = NULL, num_iterations = 8) {
    
    # number of possible states
    num_states <- nrow(P)
    
    # stores the states X_t through time
    states <- numeric(num_iterations)
    
    # initialize variable for first state 
    if (is.null(init_state)) {
        states[1] <- floor(runif(1, min = 1, max = dim(P)[1]))
    } else {
        states[1] <- init_state    
    }
    
    for(t in 2:num_iterations) {
        
        # probability vector to simulate next state X_{t+1}
        p <- P[states[t-1], ]
        
        ## draw from multinomial and determine state
        states[t] <- which(rmultinom(1, 1, p) == 1)
    }
    
    states.f <- rownames(P)[states]
    
    return(states.f)
}

# P <- as.matrix(mvmts.dist)
# num_chains <- 10
# num_iterations <- 4
# #init_state <- 7 #start with Straddle Planche
# init_state <- NULL
# 
# chains <-
#     seq(10000) %>% 
#     map_dfr(
#         ~tibble(mvmt = runMC(num_iterations = 5)),
#         .id = "iter"
#     )
# 
# chains %>% 
#     count(mvmt) %>% 
#     ggplot(aes(x = reorder(mvmt, n), y = n)) +
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     ggdark::dark_theme_classic()

# each column stores the sequence of states for a single chains
# chains <- matrix(NA, ncol = num_chains, nrow = num_iterations)

# simulate chains
# for(c in seq_len(num_chains)){
#     chains[,c] <- runMC(P, init_state = init_state, num_iterations = num_iterations)
# }

#matplot(chains, type='l', lty=1, col=1:5, ylim=c(1,dim(P)[1]), ylab='state', xlab='time')

# chains.df <- 
#     chains %>% 
#     as.data.frame() %>% 
#     rename_all(~gsub("V", "", .)) %>% 
#     melt(id.var = NULL, variable.name = "iteration", value.name = "Movement_Index") %>% 
#     left_join(
#         mvmts.0 %>% 
#             select(Movement) %>% 
#             mutate(Movement_Index = row_number())
#     )




# UI ####
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("frctl mvmt"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "select_mvmt", 
                label = h3("Starting movement:"), 
                choices = mvmts.0$Movement,
                selected = floor(runif(1, min = 1, max = nrow(mvmts.0)+1))
                #TODO: updateSelectInput()
            ),
            #hr(),
            sliderInput(
                inputId = "slider_sets",
                label = "Number of sets:",
                min = 1,
                max = 5,
                value = 2
            ),
            sliderInput(
                inputId = "slider_mvmts",
                label = "Number of movements:",
                min = 2,
                max = 10,
                value = 4
            ),
            actionButton(
                inputId = "cmd_randomStart",
                label = "Random Start"
            ),
            actionButton(
                inputId = "cmd_run",
                label = "Go",
                icon = icon("refresh")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("table_data")
            #fluidRow(column(1, verbatimTextOutput("value")))
            #plotOutput("clusterPlot"),
            #plotOutput("rasterPlot")
        )
    )
)


# SERVER ####
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        if (input$cmd_randomStart)
            updateSelectInput(
                session, 
                "select_mvmt", 
                selected = mvmts.0$Movement[floor(runif(1, min = 1, max = nrow(mvmts.0)+1))]
            )
    })
    
    mvmt.df <-
        reactive({
            if (input$cmd_run)
                seq(input$slider_sets) %>% 
                #seq(1, 3) %>% 
                map_dfr(
                    ~tibble(
                        mvmt = runMC(
                            init_state = which(mvmts.0$Movement == input$select_mvmt),
                            num_iterations = input$slider_mvmts
                        ),
                        Sequence = 1:input$slider_mvmts
                    ),
                    .id = "Set"
                ) %>% 
                mutate(Set = paste("Set", Set)) %>% 
                dcast(Sequence ~ Set, value.var = "mvmt")
        })

    output$table_data <- renderTable(mvmt.df())
  
    # output$clusterPlot <- renderPlot({
    #     mvmts.dist %>%
    #         #hclust(method = "ward.D") %>%
    #         hclust(method = "complete") %>%
    #         plot()
    # })
    

    # output$rasterPlot <- renderPlot({
    #     mvmts.hclust <- 
    #         mvmts.dist %>% 
    #         hclust(method = "complete")
    #     #length(mvmts.hclust$order)
    #     mvmts.dist.df %>% 
    #         rownames_to_column("mvmt_a") %>% 
    #         melt(id.var = "mvmt_a", variable.name = "mvmt_b", value.name = "distance") %>% 
    #         # mutate(
    #         #     flag_selected = mvmt_a %in% mvmt.df$`Set 1`
    #         # ) %>% 
    #         #ggplot(aes(x = reorder(mvmt_a, mvmts.hclust$order), y = reorder(mvmt_b, mvmts.hclust$order), fill = distance)) +
    #         ggplot(aes(x = reorder(mvmt_a, distance), y = reorder(mvmt_b, distance), fill = distance)) +            
    #         geom_raster() +
    #         theme(axis.text.x = element_text(angle = 90, hjust = 1))
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
