### get config packages, colors, etc
source("Benchmarking/tasks/config.R")

### scrap data from Wahlrecht.de
#source("Benchmarking/tasks/scrape-wahlrechtde-umfragen.R")

# calculate mov average and error
#source("tasks/calculations-gauss-error.R")

### rolling average of latest poll by each institute, weighted by number of interviews
#source("tasks/calculations-latest_polls_weights.R")

### generate two charts
source("Benchmarking/tasks/chart-longterm-polls.R")
#source("Benchmarking/tasks/chart-sunday-polls.R")
