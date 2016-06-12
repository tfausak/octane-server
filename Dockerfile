FROM debian:jessie

COPY . /code
RUN \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
  echo deb http://download.fpcomplete.com/debian jessie main > /etc/apt/sources.list.d/fpco.list && \
  apt-get update && \
  apt-get --assume-yes install sqlite3 stack && \
  cd /code && \
  stack setup && \
  stack --local-bin-path /usr/local/bin build --copy-bins && \
  rm -r $(stack path --stack-root) && \
  cd - && \
  rm -r /code && \
  apt-get --assume-yes purge stack && \
  apt-get --assume-yes autoremove --purge && \
  apt-get --assume-yes autoclean

ENV PORT 80
EXPOSE 80
CMD octane-server
