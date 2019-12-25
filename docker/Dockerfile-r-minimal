FROM rexyai/r-minimal

ENV R_CRAN_PKGS Rcpp R6 uuid checkmate mime jsonlite

COPY . /tmp/RestRserve

RUN apk update && \
    apk add --no-cache --virtual .build-deps gcc g++ musl-dev openssl-dev && \
    installr -d $R_CRAN_PKGS && \
    Rscript -e "install.packages('Rserve', repos = 'http://www.rforge.net/')" && \
    R CMD build --no-manual --no-build-vignettes /tmp/RestRserve && \
    R CMD INSTALL /RestRserve*.tar.gz && \
    rm -rf /tmp/RestRserve* && \
    rm /RestRserve*.tar.gz && \
    apk del .build-deps

WORKDIR /

EXPOSE 8080

CMD ["Rscript", "-e", "source(system.file('examples', 'hello', 'app.R', package = 'RestRserve')); backend$start(app, http_port = 8080)"]
