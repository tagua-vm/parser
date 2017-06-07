FROM debian:jessie

ENV ARCH=x86_64-unknown-linux-gnu
ENV RUST_RELEASE=1.17.0
ENV CARGO_RELEASE=nightly
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

RUN apt-get update && \
    apt-get install -y curl vim gcc libssl-dev libedit-dev libstdc++-4.9-dev && \
    find /usr/bin -executable -iname llvm* | xargs -n1 -I file echo ln -s file file | sed s/-$LLVM_RELEASE$// | bash

RUN curl -sL https://static.rust-lang.org/dist/rust-$RUST_RELEASE-$ARCH.tar.gz | tar xvz -C /tmp && \
    /tmp/rust-$RUST_RELEASE-$ARCH/install.sh && \
    rm -rf /tmp/rust-$RUST_RELEASE-$ARCH

RUN curl -sL https://static.rust-lang.org/cargo-dist/cargo-$CARGO_RELEASE-$ARCH.tar.gz | tar xvz -C /tmp && \
    /tmp/cargo-$CARGO_RELEASE-$ARCH/install.sh && \
    rm -rf /tmp/cargo-$CARGO_RELEASE-$ARCH

RUN apt-get remove --purge -y curl && \
    apt-get autoclean && apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

VOLUME /source
WORKDIR /source
