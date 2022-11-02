FROM debian:latest
RUN apt-get update -yq \
&& apt-get install opam -yq

RUN opam init --disable-sandboxing --yes \
&& opam switch create ocaml-system.4.11.1 \
&& eval $(opam env)

RUN opam install dune terminal_size \
&& eval $(opam env)

ADD . /app/
WORKDIR /app

CMD eval $(opam env) && dune exec ./oditor.exe
