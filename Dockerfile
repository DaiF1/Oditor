FROM debian:9
RUN apt-get update -yq \
&& apt-get install opam -yq \
&& opam init \
&& opam switch create 4.11.1 \
&& eval 'opam config env'

ADD . /app/
WORKDIR /app
RUN which ocaml

EXPOSE 2368
VOLUME /app/logs

CMD dune exec ./oditor.exe
