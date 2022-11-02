FROM debian:latest
RUN apt-get update -yq \
&& apt-get install opam -yq

ARG UID
ARG GID

RUN groupadd -g $GID -o user
RUN useradd -m -u $UID -g $GID -o -s /bin/bash user
USER user

RUN opam init --disable-sandboxing --yes \
&& opam switch create ocaml-system.4.11.1 \
&& eval $(opam env)

RUN opam install dune terminal_size \
&& eval $(opam env)

ADD . /app/
WORKDIR /app

CMD eval $(opam env) && dune exec ./oditor.exe
