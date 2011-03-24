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

#define PKG_MAGIC 0x41FE
#define GS_MAGIC  0x5309

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

    /* pkg header */
    len += append_shrt(&bufp, PKG_MAGIC);
    len += append_shrt(&bufp, GS_MAGIC);
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
main(int argc, char **argv)
{
    char buf[BUFSIZ], *bufp, sbuf[BUFSIZ], *host = "localhost";
    int sock, i, len=0;
    unsigned short port = 5309;
    bufp = buf;

    if(argc == 2)
	host = argv[2];

    memset(buf, 0, BUFSIZ);
    len = make_ping(buf);

    printf("Host: %s\n", host);

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
	printf("sock: %d, %X\n", sock, (unsigned int)(s.sin_addr.s_addr));
	if(connect(sock, ss, sizeof(struct sockaddr)) == -1) {
	    perror("connect");
	    return -1;
	}
    }

    if(write(sock, buf, len) != len) 
	perror("Writing to socket\n");
    len = read(sock, buf, BUFSIZ);
    printf("response: %d bytes\n", len);

    for(i=0;i<len;i++)
	if(isprint(buf[i]))
	    printf("%c", buf[i]);
	else
	    printf("[%x]", buf[i]&0xff);

    printf("\n");

    return 0;
}
