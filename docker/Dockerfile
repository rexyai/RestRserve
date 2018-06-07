FROM r-base:3.5.0

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev

RUN Rscript -e "install.packages( c('Rserve', 'R6', 'uuid', 'base64enc', 'yaml', 'swagger', 'mime'), \
    repos = c('https://cran.rstudio.com/', 'http://www.rforge.net/'))"

RUN Rscript -e "install.packages('RestRserve', repos = 'https://dselivanov.github.io/drat/')"

CMD ["Rscript", "-e", "source(system.file('fib.R', package = 'RestRserve'), keep.source = TRUE)"]
