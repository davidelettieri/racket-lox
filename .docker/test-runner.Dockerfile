FROM debian:bullseye

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    bash \
    build-essential \
    ca-certificates \
    curl \
    git \
    make \
    wget && \
    rm -rf /var/lib/apt/lists/*

ARG DART_VERSION=2.19.6
ARG DART_DEB=dart_${DART_VERSION}-1_amd64.deb

RUN wget -O /tmp/${DART_DEB} \
    "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/linux_packages/${DART_DEB}" && \
    dpkg -i /tmp/${DART_DEB} && \
    rm /tmp/${DART_DEB}

ARG RACKET_VERSION=9.0

RUN set -eux; \
        installer=""; \
        for distro in bullseye buster; do \
            candidate="racket-${RACKET_VERSION}-x86_64-linux-${distro}-cs.sh"; \
            url="https://download.racket-lang.org/installers/${RACKET_VERSION}/${candidate}"; \
            if curl -fsSL -o /tmp/racket-installer.sh "$url"; then \
                installer="$candidate"; \
                break; \
            fi; \
        done; \
        test -n "$installer"; \
        bash /tmp/racket-installer.sh --create-dir --in-place --dest /usr/racket; \
        rm /tmp/racket-installer.sh

ENV PATH="/usr/racket/bin:/usr/lib/dart/bin:${PATH}"
ENV PUB_CACHE="/opt/pub-cache"

COPY craftinginterpreters/ /opt/craftinginterpreters/

WORKDIR /opt/craftinginterpreters
RUN mkdir -p "${PUB_CACHE}" && \
    make get && \
    chmod -R a+rX "${PUB_CACHE}" /opt/craftinginterpreters

WORKDIR /workspace
