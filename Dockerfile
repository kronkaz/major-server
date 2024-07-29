# https://github.com/bobbypriam/haversiner/tree/master

FROM ocaml/opam:alpine AS base
WORKDIR major
ADD major.opam .
RUN opam pin add -yn major . && \
    opam depext major && \
    opam install --deps-only major
ADD . .
RUN sudo chown -R opam:nogroup . && \
    opam config exec dune build @install && \
    opam depext -ln major | egrep -o "\-\s.*" | sed "s/- //" > depexts

FROM alpine
WORKDIR /app
USER root
RUN apk add libev gmp
COPY --from=base /home/opam/major/_build/default/bin/main.exe major.exe
COPY --from=base /home/opam/major/depexts depexts
RUN cat depexts | xargs apk --update add && rm -rf /var/cache/apk/*
EXPOSE 8000
ENTRYPOINT ["./major.exe"]
