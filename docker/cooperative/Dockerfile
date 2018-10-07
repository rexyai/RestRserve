FROM r-base:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev

RUN mkdir -p ~/.R/ && \
  echo 'PKG_CFLAGS += -DCOOPERATIVE' > ~/.R/Makevars

RUN Rscript -e "install.packages( c( \
    'Rserve', \
    'R6', \
    'uuid', \
    'base64enc', \
    'yaml', \
    'swagger', 'mime', 'remotes'), \
  repos = c('https://cran.rstudio.com/', 'http://www.rforge.net/'))"

RUN Rscript -e "remotes::install_github('dselivanov/RestRserve')"

CMD ["Rscript", "-e", "source(system.file('fib.R', package = 'RestRserve'), keep.source = TRUE)"]
