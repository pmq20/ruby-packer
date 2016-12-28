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

ssize_t squash_readlink(sqfs_err *error, sqfs *fs, const char *path, char *buf, size_t bufsize)
{
	// TODO @sounder
	// Push Test
	return -1;
}
