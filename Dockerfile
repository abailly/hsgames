# Dockerfile for building Haskell code
FROM ubuntu:xenial
MAINTAINER arnaud@gorillaspace.co

ENV DEBIAN_FRONTEND noninteractive

# localize me
ENV LANG C.UTF-8
RUN locale-gen en_US.UTF-8

# necessary for add-apt-repository
RUN apt-get update && \
    apt-get install -y software-properties-common curl git build-essential

# install stack
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list && \
    # hvr keys
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list && \
    # fpco keys
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442 &&  \
    apt-get update && apt-get install -y --no-install-recommends cabal-install-1.24 ghc-8.0.1 happy-1.19.5 alex-3.1.7 ca-certificates netbase build-essential zlib1g-dev libtinfo-dev stack

ENV PATH /opt/cabal/1.24/bin:/opt/ghc/8.0.1/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH

# install Elm

# use downloaded pre-build binary instead of package manager
# from https://github.com/nodejs/docker-node/blob/7cbea391f22678de5d828b1a38c27a25c951795f/4.2/Dockerfile
# gpg keys listed at https://github.com/nodejs/node
RUN set -ex \
  && for key in \
    9554F04D7259F04124DE6B476D5A82AC7E37093B \
    94AE36675C464D64BAFA68DD7434390BDBE9B9C5 \
    0034A06D9D9B0064CE8ADF6BF1747F4AD2306D93 \
    FD3A5288F042B6850C66B31F09FE44734EB7990E \
    71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 \
    DD8F2338BAE7501E3DD5AC78C273792F7D83545D \
  ; do \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
  done

ENV NPM_CONFIG_LOGLEVEL info
ENV NODE_VERSION 4.2.4

RUN curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.gz" \
  && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
  && gpg --verify SHASUMS256.txt.asc \
  && grep " node-v$NODE_VERSION-linux-x64.tar.gz\$" SHASUMS256.txt.asc | sha256sum -c - \
  && tar -xzf "node-v$NODE_VERSION-linux-x64.tar.gz" -C /usr/local --strip-components=1 \
  && rm "node-v$NODE_VERSION-linux-x64.tar.gz" SHASUMS256.txt.asc    

# install Elm
RUN npm install -g elm elm-live

# link node to nodejs... WTF?
RUN ln -s /usr/bin/nodejs /usr/bin/node

# build app
COPY . .
RUN ./build.sh

EXPOSE 8080

CMD stack exec server -- 8080
