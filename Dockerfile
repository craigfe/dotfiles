FROM koalaman/shellcheck-alpine
MAINTAINER "Craig Ferguson <me@craigfe.io>"

RUN apk add --no-cache \
  file \
  bash \
  git
