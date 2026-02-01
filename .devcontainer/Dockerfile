# Stage 1: Base image with Dart
FROM debian:bullseye AS dart-base

RUN apt-get update -y && apt-get install -y \
    wget \
    git \
    build-essential \
    ca-certificates \
    curl --no-install-recommends && \  
    rm -rf /var/lib/apt/lists/*  

ARG DART_VERSION=2.19.6
ARG DART_DEB=dart_${DART_VERSION}-1_amd64.deb

RUN wget -O /tmp/${DART_DEB} \
    "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/linux_packages/${DART_DEB}" && \
    dpkg -i /tmp/${DART_DEB} && \
    rm /tmp/${DART_DEB}

ENV PATH="/usr/lib/dart/bin:${PATH}"