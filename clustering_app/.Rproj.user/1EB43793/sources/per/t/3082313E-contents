library(shiny)
library(ggplot2)
library(ClusterR)
library(FD)

# Set UI
ui <- fluidPage(
    h1("Clustering on Test Dataset"),
    br(),
    column(3,
        h3("Select Clustering Method"),
       selectInput("method", "",
        c("Select Method" = "none",
          "K-Means" = "kmeans",
          "K-Medoids" = "kmedoids",
          "Gaussian Mixture" = "gmm"
          )
       ),
       br(),
       br(),
       actionButton("run", "Run", class = "btn-success")
    ),
    column(3,
       conditionalPanel(
           condition = "input.method == 'kmeans'",
           div(h3("K-Means Selected")),
           br(),
           div(h5("Enter Parameters: ")),
           numericInput("kmeans_num", "Number of Clusters:", 2, min = 1, max = 10),
           numericInput("kmeans_iter", "Number of k-means Iterations:", 10, min = 1, max = 1000000)
       ),
       conditionalPanel(
           condition = "input.method == 'kmedoids'",
           div(h3("K-Medoids Selected")),
           br(),
           div(h5("Enter Parameters: ")),
           numericInput("kmedoids_num", "Number of Clusters:", 2, min = 1, max = 10),
           selectInput("kmedoids_swap_phase", "Swap Phase:", 
                       c("True" = "tt",
                         "False" = "ff"))
       ),
       conditionalPanel(
           condition = "input.method == 'gmm'",
           div(h3("Gaussian Mixture Model Selected")),
           br(),
           div(h5("Enter Parameters: ")),
           numericInput("gmm_num", "Number of Mixture Components:", 2, min = 1, max = 10),
           selectInput("gmm_dist_mode", "Distance Mode:", 
                         c("Euclidean" = "eucl",
                           "Mahalanobis" = "maha")),
           numericInput("gmm_km_iter", "Number of k-means Iterations:", 10, min = 1, max = 1000000),
           numericInput("gmm_em_iter", "Number of Expectation Maximization Iterations:", 5, min = 1, max = 1000000)
       )
    ),
    column(6,
        plotOutput("plt")
    )
)

# Define server logic
server <- function(input, output) {
    
    fileimport = read.table("./clustering_data.txt")
    data = fileimport[c(1,2)]
    classes = fileimport[c(3)]
    
    output$plt = renderPlot({
        qplot(data$V1, data$V2)
    })
    
    generate_plot = function(data){
        if(input$method == "kmeans"){
            dat = center_scale(data, mean_center = T, sd_scale = T)
            km = KMeans_arma(dat, input$kmeans_num, n_iter=input$kmeans_iter, seed_mode="random_subset", verbose=F)
            pr = predict_KMeans(dat, km)
            class(km) = 'matrix'
            output$plt = renderPlot({
                plot_2d(dat, as.vector(pr), as.matrix(km))
            })
        }else{
            if(input$method == "kmedoids"){
                swap = switch(input$kmedoids_swap_phase,
                              tt = TRUE,
                              ff = FALSE,
                              TRUE)
                dat = center_scale(data, mean_center = T, sd_scale = T)
                cm = Cluster_Medoids(dat, input$kmedoids_num, swap_phase = swap, verbose=F)
                output$plt = renderPlot({
                    plot_2d(data, cm$clusters, cm$medoids)
                })
            }else{
                if(input$method == "gmm"){
                    dist = switch(input$gmm_dist_mode,
                                  eucl = "eucl_dist",
                                  maha = "maha_dist",
                                  eucl_dist)
                    dat = center_scale(data, mean_center = T, sd_scale = T)
                    gmm = GMM(dat, input$gmm_num, dist_mode=dist, seed_mode="random_subset", km_iter=input$gmm_km_iter, em_iter=input$gmm_em_iter, verbose=F)
                    pr = predict_GMM(dat, gmm$centroids, gmm$covariance_matrices, gmm$weights)
                    output$plt = renderPlot({
                        plot_2d(dat, pr$cluster_labels, gmm$centroids)
                    })
                }
            }
        }
    }
    
    observeEvent(input$run,
        generate_plot(data)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
