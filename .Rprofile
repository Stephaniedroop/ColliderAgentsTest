source("renv/activate.R")

options(repos = c(CRAN = "https://lib.stat.cmu.edu/R/CRAN/"))

options(.gander_chat = ellmer::chat_anthropic())
options(.gander_style = "Remove backticks when giving pieces of R code. 
        Also assume I am using base R pipes and give me the native forward pipe syntax |> instead of magrittr pipe")

if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  readRenviron("~/.Renviron")
}
