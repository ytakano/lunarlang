FROM ubuntu:18.10

RUN apt-get update && \
    apt-get upgrade && \
    apt-get install -y zsh build-essential clang lld git cmake ninja-build libboost-all-dev vim llvm-dev && \
    ln -s /usr/bin/lld /usr/local/bin/ld

COPY zshenv /root/.zshenv
COPY zshrc /root/.zshrc

ENTRYPOINT ["zsh"]
