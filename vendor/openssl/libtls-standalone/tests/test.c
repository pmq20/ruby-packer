#include <stdio.h>
#include <tls.h>

int main()
{
	struct tls *tls;
	struct tls_config *tls_config;
	ssize_t written, read;
	char buf[4096];

	if (tls_init() != 0) {
		fprintf(stderr, "tls_init failed");
		return 1;
	}

	if ((tls = tls_client()) == NULL)
		goto err;

	if ((tls_config = tls_config_new()) == NULL)
		goto err;

	if (tls_config_set_ciphers(tls_config, "compat") != 0)
		goto err;

	tls_config_insecure_noverifycert(tls_config);
	tls_config_insecure_noverifyname(tls_config);

	if (tls_configure(tls, tls_config) != 0)
		goto err;

	if (tls_connect(tls, "google.com", "443") != 0)
		goto err;

	if ((written = tls_write(tls, "GET /\r\n", 7)) < 0)
		goto err;

	if ((read = tls_read(tls, buf, sizeof(buf))) < 0)
		goto err;

	buf[read - 1] = '\0';
	puts(buf);

	if (tls_close(tls) != 0)
		goto err;

	return 0;

err:
	fprintf(stderr, "%s\n", tls_error(tls));
	return 1;
}
