FROM r-base:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev libjemalloc1

ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so.1

RUN Rscript -e "install.packages( c( \
    'Rserve', \
    'R6', \
    'uuid', \
    'base64enc', \
    'yaml', \
    'swagger', 'mime', 'remotes'), \
  repos = c('https://cran.rstudio.com/', 'http://www.rforge.net/'))"

COPY . /RestRserve

RUN R CMD build /RestRserve
RUN R CMD INSTALL RestRserve*.tar.gz && rm -rf RestRserve*

CMD ["Rscript", "-e", "source(system.file('fib.R', package = 'RestRserve'), keep.source = TRUE)"]
