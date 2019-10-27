FROM r-base:3.6.1

ENV R_FORGE_PKGS Rserve
ENV R_CRAN_PKGS Rcpp R6 uuid checkmate mime jsonlite

COPY . /tmp/RestRserve

ENV STRIP "-- --strip"

RUN apt-get update && \
    apt-get install -y --no-install-recommends libssl-dev libjemalloc-dev && \
    install2.r -r http://www.rforge.net/ $STRIP $R_FORGE_PKGS && \
    install2.r  $STRIP $R_CRAN_PKGS && \
    R CMD build --no-manual --no-build-vignettes /tmp/RestRserve && \
    R CMD INSTALL $STRIP /RestRserve*.tar.gz && rm -rf /tmp/RestRserve* && rm /RestRserve*.tar.gz

ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so

EXPOSE 8080

CMD ["Rscript", "-e", "source(system.file('examples', 'hello', 'app.R', package = 'RestRserve')); backend$start(app, http_port = 8080)"]
