# CREATE A DOCKER IMAGE OF THE SITS PACKAGE.
# docker build -t sits

FROM rocker/geospatial:4.0.2 
MAINTAINER alber sanchez <alber.ipia@inpe.br>
RUN apt update
RUN Rscript -e "devtools::install_github('e-sensing/sits', dependencies = TRUE)"
