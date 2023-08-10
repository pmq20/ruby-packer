ARG ARCH
FROM you54f/traveling-ruby-builder-${arch:-arm64}:next

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
RUN curl -fsSL https://raw.githubusercontent.com/you54f/traveling-ruby/main/cli.sh | TRAVELING_RUBY_VERSION=3.1.2 sh
RUN ruby --version
RUN bundler --version
WORKDIR /app
COPY . .
RUN bundle update --bundler
RUN bundle install
RUN ruby --version
RUN bundle exec rake