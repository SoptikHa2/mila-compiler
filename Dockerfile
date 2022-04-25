FROM haskell

RUN mkdir /app
RUN apt update
RUN apt install -y cmake wget python3 alex happy
COPY ./installScript.sh /tmp/install.sh
RUN chmod +x /tmp/install.sh
RUN /tmp/install.sh
# RUN cabal update && cabal install llvm-hs -j4
