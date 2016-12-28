/*
 * Copyright (c) 2016-2017 Minqi Pan and Shengyuan Liu
 *
 * This file is part of libsquash, distributed under the MIT License
 * For full terms see the included LICENSE file
 */

#include "squash.h"
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

int squash_scandir(sqfs_err *error, sqfs *fs, const char *dirname, struct dirent ***namelist,
	int (*select)(const struct dirent *),
	int (*compar)(const struct dirent **, const struct dirent **))
{
	// TODO @sounder
	return -1;
}
