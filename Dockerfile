FROM rocker/shiny-verse:3.6.1

RUN apt-get update && apt-get install -y \
  r-base-core \
  coinor-libsymphony-dev \
  coinor-libcgl-dev \
  libglpk-dev

RUN R -e "install.packages(c( \
  'Rsymphony', \
  'Rglpk' \
  ))"

COPY ./ /strand-src
RUN R CMD INSTALL /strand-src
RUN mkdir -p /srv/shiny-server/data
RUN tar -C /srv/shiny-server/data -zxf /strand-src/sample_data.tar.gz

CMD ["/usr/bin/shiny-server.sh"]

