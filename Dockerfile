FROM ocaml/opam

RUN opam install dune terminal_size \
&& eval $(opam env)

ADD . /app/
WORKDIR /app

CMD dune exec ./oditor.exe
