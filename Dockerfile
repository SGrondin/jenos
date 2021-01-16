FROM asemio/mountain-caravan:1.1.0 AS build
WORKDIR /app
RUN sudo apk update \
  && sudo apk add --no-cache perl cmake gmp-dev

COPY jenos.opam .

RUN opam update \
  && OPAMYES=1 opam install . --deps-only \
  && OPAMYES=1 opam upgrade

COPY . .

RUN OPAMYES=1 opam pin add discord 'git+https://github.com/SGrondin/discord.git#7398a57'

ENV DUNE_PROFILE release

RUN sudo chown -R opam /app \
  && opam exec -- dune build src/app.exe \
  && cp /app/_build/default/src/app.exe . \
  && chmod 755 app.exe \
  && strip app.exe

RUN mkdir lib \
  && ldd app.exe \
  | awk '$2 == "=>" && $3 !~ /ld-musl/ {print $1, $3}' \
  | sort | uniq | awk '{print $2}' \
  | xargs -n1 -I{} -- cp {} lib/

##############################
FROM alpine:3.12
WORKDIR /app
COPY --from=build /app/app.exe .
COPY --from=build /app/config.json .
COPY --from=build /app/lib ./lib/
ENV LD_LIBRARY_PATH ./lib
RUN ./app.exe -h

ENTRYPOINT [ "/app/app.exe" ]
