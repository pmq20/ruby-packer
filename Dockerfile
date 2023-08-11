FROM you54f/traveling-ruby-builder:latest AS rubyc_builder
RUN yum -y update && yum install -y squashfs-tools bison texinfo
RUN cat /etc/issue && \
          uname -a && \
          uname -p && \
          uname -m && \
          lscpu && \
          which mksquashfs && \
          mksquashfs -version
RUN source /hbb_shlib/activate
ENV PATH="/root/.travelling-ruby/bin/:$PATH"
RUN curl -fsSL https://raw.githubusercontent.com/you54f/traveling-ruby/main/cli.sh | TRAVELING_RUBY_VERSION=3.2.2 sh
RUN ruby --version
RUN bundler --version
WORKDIR /app
COPY . .
# ENV LD_LIBRARY_PATH="/temp/rubyc/local/lib:$LD_LIBRARY_PATH"
# ENTRYPOINT [ "bin/rubyc" ]
# CMD [ "bin/rubyc", "-o", "rubyc" ]
RUN bin/rubyc bin/rubyc -o rubyc

RUN wget https://www.openssl.org/source/openssl-1.1.1o.tar.gz && \
    tar -xvf openssl-1.1.1o.tar.gz && \
    cd openssl-1.1.1o && \
    # ./config --prefix=/usr/local/ssl --openssldir=/usr/local/ssl shared zlib && \
    ./config && \
    make && \
    make test || true && \
    make install

FROM centos:7

COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
COPY --from=rubyc_builder /tmp/rubyc/local/lib/libssl.so.1.1 /usr/lib64/libssl.so.1.1
COPY --from=rubyc_builder /tmp/rubyc/local/lib/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1

ENTRYPOINT [ "rubyc" ]
CMD [ "--help" ]