FROM ruby:3.1.4

RUN apt update && apt install -y build-essential squashfs-tools automake libtool byacc bison
RUN cat /etc/issue && \
          uname -a && \
          uname -p && \
          uname -m && \
          lscpu && \
          which mksquashfs && \
          mksquashfs -version
WORKDIR /app
COPY . .
RUN bundle install
RUN bundle exec rake