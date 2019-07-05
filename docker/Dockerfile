FROM ubuntu:19.04

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y zsh build-essential clang lld git cmake ninja-build libboost-all-dev vim llvm-dev emacs jq lldb && \
    ln -s /usr/bin/lld /usr/local/bin/ld

COPY zshenv /root/.zshenv
COPY zshrc /root/.zshrc
COPY emacs /root/.emacs

ENTRYPOINT ["zsh"]
