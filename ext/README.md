# Precompilled native extensioons

### PG

```bash
wget "https://github.com/postgres/postgres/archive/refs/tags/REL_13_4.zip"
unzip REL_13_4.zip

mv postgres-REL_13_4/ ./postgres

cd ./postgres

./configure --with-openssl

make

cd src/interfaces
make

cd src/common
make CFLAGS=-fPIC

cd src/port
# vim Makefile - add -fPIC to CPPFLAGS
make

cd ruby_pg

rake compile

cd tmp.....

/usr/bin/clang-10 -shared -o pg_ext.so gvl_wrappers.o pg.o pg_binary_decoder.o pg_binary_encoder.o pg_coder.o pg_connection.o pg_copy_coder.o pg_errors.o pg_record_coder.o pg_result.o pg_text_decoder.o pg_text_encoder.o pg_tuple.o pg_type_map.o pg_type_map_all_strings.o pg_type_map_by_class.o pg_type_map_by_column.o pg_type_map_by_mri_type.o pg_type_map_by_oid.o pg_type_map_in_ruby.o pg_util.o /workspaces/postgres/src/interfaces/libpq/libpq.a  /workspaces/postgres/src/port/libpgport.a /workspaces/postgres/src/common/libpgcommon.a -L. -L/usr/local/rvm/rubies/ruby-3.1.0/lib -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/lib/x86_64-linux-gnu -Wl,-rpath,/usr/lib/x86_64-linux-gnu -L. -fstack-protector-strong -rdynamic -Wl,-export-dynamic -Wl,--no-as-needed -Wl,--compress-debug-sections=zlib    -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/local/rvm/rubies/ruby-3.1.0/lib -lcrypt -lssl
```

### MYSQL

```bash
sudo apt install libmysqlclient-dev

cd mysql2

rake compile

cd tmp.....

/usr/bin/clang-10 -shared -o mysql2.so client.o infile.o mysql2_ext.o result.o statement.o /usr/lib/x86_64-linux-gnu/libmysqlclient.a -L. -L/usr/local/rvm/rubies/ruby-3.1.0/lib -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/lib/x86_64-linux-gnu -Wl,-rpath,/usr/lib/x86_64-linux-gnu -L. -fstack-protector-strong -rdynamic -Wl,-export-dynamic -Wl,--no-as-needed -Wl,--compress-debug-sections=zlib    -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/lib/x86_64-linux-gnu -lpthread -ldl -lz -lssl -lcrypto -lresolv -lm -lrt -lstdc++
```


### SQLServer
```bash
git clone --depth=1 https://github.com/FreeTDS/freetds.git

cd freetds

autoconf

./configure  --enable-static --disable-shared

make CFLAGS=-fPIC


cd tiny_tds

rake compile

cd tmp.....

/usr/bin/clang-10 -shared -o tiny_tds.so client.o result.o tiny_tds_ext.o /workspaces/freetds-1.3.3/src/dblib/.libs/libsybdb.a -L. -L/usr/local/rvm/rubies/ruby-3.1.0/lib -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/opt/local/lib -Wl,-rpath,/opt/local/lib -L/opt/local/lib/freetds -Wl,-rpath,/opt/local/lib/freetds -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib/freetds -Wl,-rpath,/usr/local/lib/freetds -L. -fstack-protector-strong -rdynamic -Wl,-export-dynamic -Wl,--no-as-needed -Wl,--compress-debug-sections=zlib    -Wl,-rpath,/usr/local/rvm/rubies/ruby-3.1.0/lib -L/usr/local/rvm/rubies/ruby-3.1.0/lib -lssl
```

