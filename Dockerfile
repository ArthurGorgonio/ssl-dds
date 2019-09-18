FROM alpine:latest

MAINTAINER Artem Klevtsov a.a.klevtsov@gmail.com

ARG R_VERSION
# Enviroment Variables
ENV R_VERSION ${R_VERSION:-3.6.1}
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
# JAVA Variables
ENV JAVA /usr/bin/java
ENV JAVAC /usr/lib/jvm/java-1.8-openjdk/jre/../bin/javac
ENV JAVAH /usr/lib/jvm/java-1.8-openjdk/jre/../bin/javah
ENV JAR /usr/lib/jvm/java-1.8-openjdk/jre/../bin/jar
ENV JAVA_LIBS -L/usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64/server -ljvm
ENV JAVA_CPPFLAGS -I/usr/lib/jvm/java-1.8-openjdk/jre/../include -I/usr/lib/jvm/java-1.8-openjdk/jre/../include/linux
ENV JAVA_LD_LIBRARY_PATH /usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64/server
ENV PATH=$JAVA_HOME/bin:$PATH

# R runtime dependencies
RUN apk --no-cache add \
        gcc \
        gfortran \
        libexecinfo \
        libexecinfo-dev \
        g++ \
        make \
        readline-dev \
        icu-dev \
        bzip2-dev \
        xz-dev \
        pcre-dev \
        libjpeg-turbo-dev \
        libpng-dev \
        tiff-dev  \
        curl-dev \
        zip \
        file \
        coreutils \
        openjdk8 \
        git \
        bash && \
# R build dependencies
    apk --no-cache add --virtual build-deps \
        curl \
        perl \
        pango-dev \
        cairo-dev \
        tcl-dev \
        tk-dev && \
    cd /tmp && \
# Download source code
    curl -O https://cran.r-project.org/src/base/R-${R_VERSION:0:1}/R-${R_VERSION}.tar.gz && \
# Extract source code
    tar -xf R-${R_VERSION}.tar.gz && \
    cd R-${R_VERSION} && \
# Sect compiler flags
    CFLAGS="-g -O2 -fstack-protector-strong -D_DEFAULT_SOURCE -D__USE_MISC" \
    CXXFLAGS="-g -O2 -fstack-protector-strong -D_FORTIFY_SOURCE=2 -D__MUSL__" \
# configure script options
    ./configure --prefix=/usr \
                --sysconfdir=/etc/R \
                --localstatedir=/var \
                rdocdir=/usr/share/doc/R \
                rincludedir=/usr/include/R \
                rsharedir=/usr/share/R \
                --enable-memory-profiling \
                --enable-R-shlib \
                --disable-nls \
                --without-x \
                --without-recommended-packages && \
# Build and install R
    make && \
    make install && \
    cd src/nmath/standalone && \
    make && \
    make install && \
# Remove build dependencies
    apk del --purge --rdepends build-deps && \
    rm -f /usr/lib/R/bin/R && \
    ln -s /usr/bin/R /usr/lib/R/bin/R && \
# Fis library path
    echo "R_LIBS_SITE=\${R_LIBS_SITE-'/usr/local/lib/R/site-library:/usr/lib/R/library'}" >> /usr/lib/R/etc/Renviron && \
# Add default CRAN mirror
    echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"))' >> /usr/lib/R/etc/Rprofile.site && \
# Add symlinks for the config ifile in /etc/R
    mkdir -p /etc/R && \
    ln -s /usr/lib/R/etc/* /etc/R/ && \
# Add library directory
    mkdir -p /usr/local/lib/R/site-library && \
    chgrp users /usr/local/lib/R/site-library && \
# Clean up
    rm -rf /usr/lib/R/library/translations && \
# Installing the xgboost dependencies
    R -e "install.packages(c('Matrix', 'data.table', 'devtools', 'magrittr', 'stringi'))" && \
# Installing the xgboost for R
    cd /tmp && git clone --recursive -b v0.81  https://github.com/dmlc/xgboost && \
    sed -i '/#define DMLC_LOG_STACK_TRACE 1/d' xgboost/dmlc-core/include/dmlc/base.h && \
    sed -i '/#define DMLC_LOG_STACK_TRACE 1/d' xgboost/rabit/include/dmlc/base.h && \
    cd xgboost/R-package && \
    R CMD INSTALL . && \
    cd ../../.. && \
    rm -rf /tmp/*
# Setup the dirs
RUN mkdir -p workspace/ssl-dds/ && \
    cd workspace/ssl-dds

# Copiando tudo da pasta para lá
# COPY flexcon_c/ R/karliane/projeto_karliane/flexcon_c
# COPY bases/ R/karliane/projeto_karliane/bases
# Seting a workdir
WORKDIR workspace/ssl-dds/

COPY ./datasets ./datasets

COPY ./main.R ./main.R

COPY ./src ./src

# R Configuring JAVA enviroment
RUN R CMD javareconf

# R installing the all need packages to run the experiment
RUN R -e "source('src/utils.R')"

# The basic state of this container is running the experiment with NaiveBayes classifier
CMD ["Rscript", "main.R", "1"]
