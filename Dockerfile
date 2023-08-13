FROM you54f/traveling-ruby-builder:latest AS rubyc_builder
RUN yum -y update && yum install -y squashfs-tools bison texinfo perl-IPC-Cmd
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
RUN ./patch_ruby_source.sh
RUN bin/rubyc bin/rubyc -o rubyc
RUN LD_LIBRARY_PATH=/tmp/rubyc/local/lib:$LD_LIBRARY_PATH ./rubyc --help
FROM centos:7

COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
COPY --from=rubyc_builder /tmp/rubyc/local/lib/libssl.so.1.1 /usr/lib64/libssl.so.1.1
COPY --from=rubyc_builder /tmp/rubyc/local/lib/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1

ENTRYPOINT [ "rubyc" ]
CMD [ "--help" ]

# perl-IPC-Cmd is needed for rubyc to work compiling with openssl 3.0.x
# https://dev.to/nikolastojilj12/update-openssl-to-3-0-on-centos7-150o
# https://bugs.ruby-lang.org/issues/18658
# https://gist.github.com/yob/08d53a003181aa0fcce9812b1b533870