#include "uuid.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <inttypes.h>

# include <sys/socket.h>
# include <sys/uio.h>
# include <netinet/in.h>
# include <netdb.h>


#include "bu.h"
#include "pkg.h"

#define GS_MAGIC  0x41FE5309

/* Define Message Types for GS Protocol */

#define GSRUALIVE 0x0042 /*Test if server is up*/
#define GSIMALIVE 0x0043 /*Expected response from running server to GSRUALIVE*/
#define GSFAIL    0x0050 /*Failure*/
#define GSOK      0x0051 /*Success*/
#define GSPING	  0x0060 /*Ping*/
#define GSPONG	  0x0061 /*Pong*/
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

#define APPEND(name, type, func) int append_##name(char **buf, type val) { *(type*)*buf = func(val); (*buf)+=sizeof(type); return sizeof(type); }
APPEND(byte, uint8_t, );
APPEND(shrt, uint16_t, htons);
APPEND(long, uint32_t, htonl);
#undef APPEND

int
append_str(char **buf, char *str) {
    int len = strlen(str);
    append_long(buf, strlen(str));
    strcpy((char *)*buf, str);
    (*buf)+=len;
    return len+4;
}

/* generate a ping request in the provided (and allocated) buff. */
int
make_ping(char *buf) {
    char uuid[40];
    int len = 0;
    char *bufp = buf;
    make_uuid(uuid);

    printf("making ping with uuid: %s\n", uuid);

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSPING);
    len += append_str(&bufp, uuid);
    len += append_byte(&bufp, 0);	/* no response uuid */

    bufp = buf + 4;
    append_long(&bufp, len);
    return len;
}

int
make_disconnect(char *buf) {
    int len = 0;
    char *bufp = buf;

    printf("making disconnect\n");

    /* pkg header */
    len += append_long(&bufp, GS_MAGIC);
    bufp += 4; len += 4;

    /* GS header */
    len += append_shrt(&bufp, GSDR);

    bufp = buf + 4;
    append_long(&bufp, len);
    return len;
}

int
print_packet(char *buf, int len)
{
    char sbuf[BUFSIZ];
    int slen;
    printf("\tMagic: 0x%X\n", ntohl(*(uint32_t*)buf)); buf+=4; len-=4;
    printf("\tlength: %d\n", ntohl(*(uint32_t*)buf)); buf+=4; len-=4;
    printf("\ttype  : 0x%X\n", ntohs(*(uint16_t*)buf)); buf+=2; len-=2;
    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
    memcpy(sbuf, buf, slen); buf[slen] = 0;
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
    slen = ntohl(*(uint32_t*)buf); buf+=4; len-=4;
    memset(sbuf, 0, BUFSIZ);
    memcpy(sbuf, buf, slen); buf[slen] = 0;
    buf += slen; len -= slen;
    printf("\tpayload(%d): %s\n", slen, sbuf);
    printf("\tleft: %d\n", len);
    return 0;
}

int
main(int argc, char **argv)
{
    char buf[BUFSIZ], *bufp, sbuf[BUFSIZ], *host = "localhost";
    int sock, i, len=0;
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

    /* send ping */
    len = make_ping(buf);
    if(write(sock, buf, len) != len) 
	perror("Writing to socket\n");

    /* recv ping response, print results */
    len = read(sock, buf, BUFSIZ);
    printf("Got a return packet:\n");
    print_packet(buf, len);

    /* send disconnect */
    len = make_disconnect(buf);
    if(write(sock, buf, len) != len)
	perror("Writting disconnect to socket\n");
    /* no disc ack */

    sleep(3);

    shutdown(sock, SHUT_RDWR);
    close(sock);

    return 0;
}
