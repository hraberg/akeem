FROM ubuntu:14.04

RUN apt-get update && apt-get install -y gcc make rlwrap && apt-get clean && rm -rf /tmp/* /var/lib/apt/lists/* /var/tmp/

WORKDIR /usr/src/akeem
COPY . /usr/src/akeem

RUN make release

CMD ["rlwrap", "./akeem"]
