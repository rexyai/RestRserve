FROM rocker/r-ver:3.6.2

ENV R_FORGE_PKGS Rserve
ENV R_CRAN_PKGS Rcpp R6 uuid checkmate mime jsonlite

RUN apt-get update && \
    apt-get install -y --no-install-recommends libssl-dev libjemalloc-dev && \
    install2.r -r http://www.rforge.net/ $R_FORGE_PKGS && \
    install2.r $R_CRAN_PKGS

RUN cd /tmp && \
    wget http://www.haproxy.org/download/2.0/src/haproxy-2.0.8.tar.gz && \
    tar -xvzf haproxy-2.0.8.tar.gz && \
    cd haproxy-2.0.8/ && \
    make TARGET=linux-glibc && \
    chmod +x haproxy && \
    cp haproxy /bin/ && \
    rm -rf /tmp/*

COPY . /tmp/RestRserve

RUN R CMD build --no-manual --no-build-vignettes /tmp/RestRserve && \
    R CMD INSTALL /RestRserve*.tar.gz && \
    rm -rf /tmp/RestRserve* && \
    rm /RestRserve*.tar.gz

WORKDIR /

ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so

EXPOSE 8080

CMD ["Rscript", "-e", "source(system.file('examples', 'hello', 'app.R', package = 'RestRserve')); backend$start(app, http_port = 8080)"]
