/*	$OpenBSD: helper.c,v 1.17 2017/10/23 14:33:07 millert Exp $ */

/*
 * Copyright (c) 2000 Poul-Henning Kamp <phk@FreeBSD.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * If we meet some day, and you think this stuff is worth it, you
 * can buy me a beer in return. Poul-Henning Kamp
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <rmd160.h>

#define MINIMUM(a, b)	(((a) < (b)) ? (a) : (b))

char *
RMD160End(RMD160_CTX *ctx, char *buf)
{
	int i;
	u_int8_t digest[RMD160_DIGEST_LENGTH];
	static const char hex[] = "0123456789abcdef";

	if (buf == NULL && (buf = malloc(RMD160_DIGEST_STRING_LENGTH)) == NULL)
		return (NULL);

	RMD160Final(digest, ctx);
	for (i = 0; i < RMD160_DIGEST_LENGTH; i++) {
		buf[i + i] = hex[digest[i] >> 4];
		buf[i + i + 1] = hex[digest[i] & 0x0f];
	}
	buf[i + i] = '\0';
	explicit_bzero(digest, sizeof(digest));
	return (buf);
}
DEF_WEAK(RMD160End);

char *
RMD160FileChunk(const char *filename, char *buf, off_t off, off_t len)
{
	struct stat sb;
	u_char buffer[BUFSIZ];
	RMD160_CTX ctx;
	int fd, save_errno;
	ssize_t nr;

	RMD160Init(&ctx);

	if ((fd = open(filename, O_RDONLY)) < 0)
		return (NULL);
	if (len == 0) {
		if (fstat(fd, &sb) == -1) {
			save_errno = errno;
			close(fd);
			errno = save_errno;
			return (NULL);
		}
		len = sb.st_size;
	}
	if (off > 0 && lseek(fd, off, SEEK_SET) < 0) {
		save_errno = errno;
		close(fd);
		errno = save_errno;
		return (NULL);
	}

	while ((nr = read(fd, buffer, MINIMUM(sizeof(buffer), len))) > 0) {
		RMD160Update(&ctx, buffer, nr);
		if (len > 0 && (len -= nr) == 0)
			break;
	}

	save_errno = errno;
	close(fd);
	errno = save_errno;
	return (nr < 0 ? NULL : RMD160End(&ctx, buf));
}
DEF_WEAK(RMD160FileChunk);

char *
RMD160File(const char *filename, char *buf)
{
	return (RMD160FileChunk(filename, buf, 0, 0));
}
DEF_WEAK(RMD160File);

char *
RMD160Data(const u_char *data, size_t len, char *buf)
{
	RMD160_CTX ctx;

	RMD160Init(&ctx);
	RMD160Update(&ctx, data, len);
	return (RMD160End(&ctx, buf));
}
DEF_WEAK(RMD160Data);
