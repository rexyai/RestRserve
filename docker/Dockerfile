FROM r-base:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev libjemalloc-dev

ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so

RUN Rscript -e "install.packages( c( \
    'Rserve', \
    'checkmate', \
    'R6', \
    'uuid', \
    'base64enc', \
    'yaml', \
    'swagger', \
    'mime'), \
  repos = c('https://cran.rstudio.com/', 'http://www.rforge.net/'))"

COPY . /RestRserve

RUN R CMD build /RestRserve
RUN R CMD INSTALL RestRserve*.tar.gz && rm -rf RestRserve*

CMD ["Rscript", "-e", "source(system.file('fib.R', package = 'RestRserve'), keep.source = TRUE)"]
