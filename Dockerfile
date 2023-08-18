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
# The latest Traveling ruby 3.2.2 has bundler 2.4.18 installed
RUN bundle _2.4.18_ install
# ARG OPEN_SSL_VERSION=1.1.1v
# ENV OPEN_SSL_VERSION=$OPEN_SSL_VERSION
RUN bundle exec rake patch_ruby_source 
# RUN bundle exec rake rubyc
RUN bin/rubyc bin/rubyc -o rubyc
RUN LD_LIBRARY_PATH=/tmp/rubyc/local/lib:$LD_LIBRARY_PATH ./rubyc --help


# # With OpenSSL 1.1.x
# FROM centos:7 as openssl_1_1_builder

# # Install build tools and dependencies
# RUN yum update -y && \
#     yum install -y wget gcc make zlib-devel perl

# ENV OPENSSL_VERSION=1.1.1l

# # Download and extract OpenSSL source code
# RUN wget https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz && \
#     tar -xzf openssl-${OPENSSL_VERSION}.tar.gz && \
#     rm openssl-${OPENSSL_VERSION}.tar.gz

# # Configure, build, and install OpenSSL
# RUN cd openssl-${OPENSSL_VERSION} && \
#     ./config --prefix=/usr/local/openssl --openssldir=/usr/local/openssl shared zlib && \
#     make && \
#     make install && \
#     cd .. && \
#     rm -rf openssl-${OPENSSL_VERSION}

# # Update shared library cache
# RUN ldconfig

# # Set environment variables
# ENV PATH="/usr/local/openssl/bin:${PATH}"
# ENV LD_LIBRARY_PATH="/usr/local/openssl/lib:${LD_LIBRARY_PATH}"

# # Verify installation
# RUN openssl version

# COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
# RUN rubyc --help
# ENTRYPOINT [ "rubyc" ]
# CMD [ "--help" ]


# FROM centos:7 as openssl_1_1_tester

# COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
# COPY --from=openssl_1_1_builder /usr/local/openssl/lib/libssl.so.1.1 /usr/lib64/libssl.so.1.1
# COPY --from=openssl_1_1_builder /usr/local/openssl/liblibcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
# RUN rubyc --help
# ENTRYPOINT [ "rubyc" ]
# CMD [ "--help" ]


# With OpenSSL 3.0.x
# perl-IPC-Cmd is needed for rubyc to work compiling with openssl 3.0.x
# https://dev.to/nikolastojilj12/update-openssl-to-3-0-on-centos7-150o
# https://bugs.ruby-lang.org/issues/18658
# https://gist.github.com/yob/08d53a003181aa0fcce9812b1b533870
FROM centos:7 as openssl_3_0_builder

# Install build tools and dependencies
RUN yum update -y && \
    yum install -y wget gcc make zlib-devel perl perl-IPC-Cmd perl-Test-Simple

ENV OPENSSL_VERSION=3.0.10
# Download and extract OpenSSL source code
RUN wget https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz && \
    tar -xzf openssl-${OPENSSL_VERSION}.tar.gz && \
    rm openssl-${OPENSSL_VERSION}.tar.gz

# Configure, build, and install OpenSSL
RUN cd openssl-${OPENSSL_VERSION} && \
    ./config --prefix=/usr/local/openssl --openssldir=/usr/local/openssl shared zlib && \
    make && \
    make test && \
    make install && \
    cd .. && \
    rm -rf openssl-${OPENSSL_VERSION}

# Update shared library cache
RUN ldconfig

# Set environment variables
ENV PATH="/usr/local/openssl/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/openssl/lib:/usr/local/openssl/lib64:${LD_LIBRARY_PATH}"

# Verify installation
RUN openssl version
RUN find / -name libssl.so | xargs cp -t /tmp && \
    find / -name libssl.so.3 | xargs cp -t /tmp && \
    find / -name libcrypto.so | xargs cp -t /tmp && \
    find / -name libcrypto.so.3 | xargs cp -t /tmp

COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
RUN rubyc --help
ENTRYPOINT [ "rubyc" ]
CMD [ "--help" ]

FROM centos:7 as openssl_3_0_tester

COPY --from=rubyc_builder /app/rubyc /usr/local/bin/rubyc
COPY --from=openssl_3_0_builder /tmp/libcrypto.so.3 /usr/lib64/libcrypto.so.3
COPY --from=openssl_3_0_builder /tmp/libcrypto.so /usr/lib64/libcrypto.so
COPY --from=openssl_3_0_builder /tmp/libssl.so.3 /usr/lib64/libssl.so.3
COPY --from=openssl_3_0_builder /tmp/libssl.so /usr/lib64/libssl.so
RUN rubyc --help
ENTRYPOINT [ "rubyc" ]
CMD [ "--help" ]

