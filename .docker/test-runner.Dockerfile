FROM debian:bullseye-slim

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    make \
    ca-certificates \
    curl

ARG DART_VERSION=2.19.6
ARG DART_DEB=dart_${DART_VERSION}-1_amd64.deb

RUN curl -fL -o /tmp/${DART_DEB} \
    "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/linux_packages/${DART_DEB}" && \
    dpkg -i /tmp/${DART_DEB} && \
    rm /tmp/${DART_DEB}

ARG RACKET_VERSION=9.0

RUN curl -fsSL -o /tmp/racket-installer.sh \
    "https://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-buster-cs.sh" && \
    bash /tmp/racket-installer.sh --create-dir --in-place --dest /usr/racket && \
    rm /tmp/racket-installer.sh

ENV PATH="/usr/racket/bin:/usr/lib/dart/bin:${PATH}"
ENV PUB_CACHE="/opt/pub-cache"

COPY craftinginterpreters/ /opt/craftinginterpreters/

WORKDIR /opt/craftinginterpreters
RUN mkdir -p "${PUB_CACHE}" && \
    make get && \
    chmod -R a+rX "${PUB_CACHE}" /opt/craftinginterpreters

WORKDIR /workspace
