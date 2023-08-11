FROM you54f/traveling-ruby-builder:latest
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
# RUN bundle update --bundler
# RUN bundle install
# RUN bundle exec rake
RUN bin/rubyc bin/rubyc -o rubyc
ENV LD_LIBRARY_PATH="/tmp/rubyc/local/lib:$LD_LIBRARY_PATH"
ENTRYPOINT [ "/app/rubyc" ]