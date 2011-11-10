#include "uuid.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <inttypes.h>

# include <sys/socket.h>
# include <sys/uio.h>
# include <netinet/in.h>
# include <netdb.h>

typedef enum {
    BU_LITTLE_ENDIAN = 1234, /* LSB first: i386, VAX order */
    BU_BIG_ENDIAN    = 4321, /* MSB first: 68000, IBM, network order */
    BU_PDP_ENDIAN    = 3412  /* LSB first in word, MSW first in long */
} bu_endian_t;

inline bu_endian_t
bu_byteorder()
{
    const union bob {
        unsigned long i;
        unsigned char c[sizeof(unsigned long)];
    } b = {1};

/* give run-time test preference to compile-time endian, tested much
 * faster than stashing in a static.
 */
#ifdef WORDS_BIGENDIAN
    if (b.c[sizeof(unsigned long)-1])
        return BU_BIG_ENDIAN;
    if (b.c[0])
        return BU_LITTLE_ENDIAN;
#else
    if (b.c[0])
        return BU_LITTLE_ENDIAN;
    if (b.c[sizeof(unsigned long)-1])
        return BU_BIG_ENDIAN;
#endif
    if (b.c[1])
        return BU_PDP_ENDIAN;

    return (bu_endian_t)0;
}


/* provide for 64-bit network/host conversions using ntohl() */
#ifndef HAVE_NTOHLL
#  define ntohll(_val) ((bu_byteorder() == BU_LITTLE_ENDIAN) ?                          \
                        ((((uint64_t)ntohl((_val))) << 32) + ntohl((_val) >> 32)) : \
                        (_val)) /* sorry pdp-endian */
#endif
#ifndef HAVE_HTONLL
#  define htonll(_val) ntohll(_val)
#endif

#define PKG_MAGIC 0x41FE
#define GSMSG_MAGIC 0x5309
#define GS_MAGIC  0x41FE5309

/* Define Message Types for GS Protocol */

#define GSRUALIVE 0x0042 /*Test if server is up*/
#define GSIMALIVE 0x0043 /*Expected response from running server to GSRUALIVE*/
#define GSFAIL    0x0050 /*Failure*/
#define GSOK      0x0051 /*Success*/
#define GSPING	  0x0060 /*Ping*/
#define GSPONG	  0x0062 /*Pong*/
#define GSRNNSET  0x0100 /*GS Remote Nodename Set*/
#define GSDR      0x0150 /*Disconnect Request*/
#define GSNNNET	  0x0200 /*New Node on Network*/
#define GSFNLR    0x0250 /*Full Nodename List Request (Not implemented yet)*/
#define GSFNL     0x0255 /*Full Nodename List (Not implemented yet)*/
#define GSNSR     0x0300 /*New Session Request*/
#define GSINFO    0x0305 /*Session Information*/
#define GSGR      0x0400 /*Geometry Request*/
#define GSGM      0x0405 /*Geometry Manifest*/
#define GSGC      0x0410 /*Geometry Chunk*/

char *
typename(int type) {
    switch(type) {
	case GSRUALIVE: return "GSRUALIVE";
	case GSIMALIVE: return "GSIMALIVE";
	case GSFAIL: return "GSFAIL";
	case GSOK: return "GSOK";
	case GSPING: return "GSPING";
	case GSPONG: return "GSPONG";
	case GSRNNSET: return "GSRNNSET";
	case GSDR: return "GSDR";
	case GSNNNET: return "GSNNNET";
	case GSFNLR: return "GSFNLR";
	case GSFNL: return "GSFNL";
	case GSNSR: return "GSNSR";
	case GSINFO: return "GSINFO";
	case GSGR: return "GSGR";
	case GSGM: return "GSGM";
	case GSGC: return "GSGC";
	default: return "Unknown";
    }
}

size_t
make_uuid(char *buf) {
    uuid_t *msguuid;
    size_t len = BUFSIZ;
    uuid_create(&msguuid);
    uuid_make(msguuid, UUID_MAKE_V4);
    uuid_export(msguuid, UUID_FMT_STR, &buf, &len);
    buf[len] = 0;
    uuid_destroy(msguuid);
    return len;
}

uint64_t
getbigtime()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1e6 + tv.tv_usec;
}

#define APPEND(name, type, func) int append_##name(char **buf, type val) { *(type*)*buf = func(val); (*buf)+=sizeof(type); return sizeof(type); }
APPEND(byte, uint8_t, );
APPEND(shrt, uint16_t, htons);
APPEND(long, uint32_t, htonl);
APPEND(ll, uint64_t, htonll);
#undef APPEND

int
append_str(char **buf, char *str) {
    int len = strlen(str);
    append_long(buf, len);
    strcpy((char *)*buf, str);
    (*buf)+=len;
    return len+4;
}

/* generate a login request in the provided (and allocated) buff. */
int
make_login(char *buf, char *username, char *password) {
    char uuid[40];
    int len = 0;
    char *bufp = buf;

    memset(buf, 0, BUFSIZ);
    make_uuid(uuid);

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSNSR);
    len += append_str(&bufp, uuid);
    len += append_byte(&bufp, 0);	/* no response uuid */
    len += append_str(&bufp, username);
    len += append_str(&bufp, password);

    bufp = buf + 4;
    append_long(&bufp, len-8);
    return len;
}

/* generate a login request in the provided (and allocated) buff. */
int
make_hello(char *buf, char *nodename) {
    char uuid[40];
    int len = 0;
    char *bufp = buf;

    memset(buf, 0, BUFSIZ);
    make_uuid(uuid);

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSRNNSET);
    len += append_str(&bufp, uuid);
    len += append_byte(&bufp, 0);	/* no response uuid */
    len += append_str(&bufp, nodename);

    bufp = buf + 4;
    append_long(&bufp, len-8);
    return len;
}

/* generate a ping request in the provided (and allocated) buff. */
int
make_ping(char *buf) {
    char uuid[40];
    int len = 0;
    char *bufp = buf;

    make_uuid(uuid);

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSPING);
    len += append_str(&bufp, uuid);
    len += append_byte(&bufp, 0);	/* no response uuid */
    len += append_ll(&bufp, getbigtime());

    bufp = buf + 4;
    append_long(&bufp, len-8);
    return len;
}

int
make_disconnect(char *buf) {
    int len = 0;
    char *bufp = buf;

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSDR);

    bufp = buf + 4;
    append_long(&bufp, len-8);
    return len;
}

int
print_packet(char *buf, int len)
{
    unsigned char sbuf[BUFSIZ];
    int slen, type, i;

    printf("Read %d byte\n", len);
    printf("\tMagic: 0x%X\n", ntohl(*(uint32_t*)buf)); buf+=4; len-4;
    printf("\tlength: %d\n", ntohl(*(uint32_t*)buf)); buf+=4; len-4;

    type = ntohs(*(uint16_t*)buf); buf+=2; len-=2;
    printf("\ttype  : 0x%X %s\n", type, typename(type));
    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
    memcpy(sbuf, buf, slen); sbuf[slen] = 0;
    buf += slen; len -= slen;
    printf("\tuuid(%d): %s\n", slen, sbuf);
    if(*buf) {
	printf("\thas reuuid: %d\t", *buf);
	buf++; len--;
	slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
	memcpy(sbuf, buf, slen); buf[slen] = 0;
	buf+=slen; len-=slen;
	printf("\t(%d): %s\n", slen, sbuf);
    } else {
	buf++;  len--;
	printf("\tno reuuid\n");
    }

    switch(type) {
	case GSNSR:
	    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
	    memcpy(sbuf, buf, slen); sbuf[slen] = 0;
	    buf += slen; len -= slen;
	    printf("\tusername(%d):\t%s\n", slen, sbuf);
	    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
	    memcpy(sbuf, buf, slen); sbuf[slen] = 0;
	    buf += slen; len -= slen;
	    printf("\tpassword(%d):\t%s\n", slen, sbuf);
	    printf("%s\n", sbuf);
	    break;
	case GSRNNSET:
	case GSINFO:
	    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
	    memset(sbuf, 0, BUFSIZ);
	    memcpy(sbuf, buf, slen); sbuf[slen] = 0;
	    buf += slen; len -= slen;
	    printf("\tpayload(%d):\t", slen);
	    printf("%s\n", sbuf);
	    break;
	case GSPING:
	    printf("\tsend time:\t%lu microseconds\n", ntohll(*(uint64_t*)buf)); buf+=8; len-=8;
	    break;
	case GSPONG:
	    printf("\tdelta time:\t%lu microseconds\n", getbigtime()-ntohll(*(uint64_t*)buf)); buf+=8; len-=8;
	    break;
	default:
	    printf("\tpayload:\t");
	    for(i=0;i<len;i++)
		printf("%02X ", buf[i]&0xff);
	    buf+=len; len=0;
	    printf("\n");
	    break;
    }
    if(len != 0)
	printf("Remainding garbage: %d\n", len);
    return 0;
}

int
main(int argc, char **argv)
{
    char buf[BUFSIZ], *bufp, sbuf[BUFSIZ], *host = "localhost";
    int sock, i, len=0, rval = EXIT_SUCCESS;
    unsigned short port = 5309;
    bufp = buf;

    if(argc == 2)
	host = argv[1];

    /* make the connection */
    {
	struct sockaddr_in s;
	struct sockaddr *ss = (struct sockaddr *)&s;
	struct hostent *h;

	memset (&s, 0, sizeof (s));
	if((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
	    perror("socket");
	    return -1;
	}

	if((h = gethostbyname(host)) == NULL) {
	    herror("gethostbyname");
	    return -1;
	}
	s.sin_family = AF_INET;
	s.sin_port = htons(port);
	s.sin_addr = *((struct in_addr *)h->h_addr_list[0]);
	if(connect(sock, ss, sizeof(struct sockaddr)) == -1) {
	    perror("connect");
	    return -1;
	}
    }

    /* server says hello */
    printf("reading node name:\n");
    len = read(sock, buf, BUFSIZ);
    print_packet(buf, len);

    printf("Making hello\n");
    len = make_hello(buf, "Ghost");
    print_packet(buf, len);
    if(write(sock, buf, len) != len)
	perror("Writing hello (GSRNNSET) to socket\n");

    printf("Making login\n");
    len = make_login(buf, "Guest", "Guest");
    print_packet(buf, len);
    if(write(sock, buf, len) != len)
	perror("Writing login to socket\n");

    /* recv login response (new session info), print results */
    printf("Reading login response\n");
    len = read(sock, buf, BUFSIZ);
    print_packet(buf, len);

    /* send ping */
    len = make_ping(buf);
    printf("Sending a ping packet:\n");
    print_packet(buf, len);
    if(write(sock, buf, len) != len)
	perror("Writing ping to socket\n");

    /* recv ping response, print results */
    printf("Reading ping response\n");
    len = read(sock, buf, BUFSIZ);
    print_packet(buf, len);

    /* send disconnect */
    printf("Sending disconnect request\n");
    len = make_disconnect(buf);
    print_packet(buf, len);
    if(write(sock, buf, len) != len)
	perror("Writing disconnect to socket\n");

    sleep(3);

EXIT:
    shutdown(sock, SHUT_RDWR);
    sleep(1);
    close(sock);

    return 0;
}
