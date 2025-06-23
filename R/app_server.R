#' Server for FIRSTkit
app_server <- function(input, output, session) {
  intro_server(input, output, session)
  firstkit.data <- data_server(input,output,session)
  descriptive_stats_server(input,output,session,firstkit.data)
  probability_server(input, output, session)
  stats_inference_server(input,output,session,firstkit.data)
  linear_regression_server(input,output,session,firstkit.data)
}
