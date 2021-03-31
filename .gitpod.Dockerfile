FROM ocamlpro/ocaml:4.11

RUN sudo chown -R 33333:33333 /home/ocaml/.opam
RUN sudo apk add g++ inotify-tools
