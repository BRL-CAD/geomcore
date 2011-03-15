#include <stdlib.h>
#include <uuid/uuid.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <inttypes.h>
#include "tpl.h"

#define MAGIC1 0x41FE
#define MAGIC2 0x5309

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

/* DO THIS FIRST - ALL messages currently need to be full GSNet msgs with UUIDs and such,
 * so define the necssary struct and UUID code up front */

struct gs_msg_struct {
	uint16_t magic1;
	uint16_t magic2;
	uint16_t msgtype;
	char *msguuid;
	char *msgreuuid;
	void *data;
};

struct gs_msg_serialized {
	uint32_t length;
	void *serialized;
};

struct gs_msg_struct *create_new_msg(uint16_t mtype, char *msgreuuid) {
	struct gs_msg_struct *new_msg;
	uuid_t msguuid;
	new_msg = malloc(sizeof(struct gs_msg_struct));
	new_msg->magic1 = MAGIC1;
	new_msg->magic2 = MAGIC2;
	new_msg->msgtype = mtype;
	uuid_generate(msguuid);
	uuid_unparse(msguuid, new_msg->msguuid);
	if (msgreuuid) new_msg->msgreuuid = msgreuuid;
	return new_msg;
}

/* Need:
 *
 * 1.  Definitions of structures to be sending and receiving (probably structs)
 *
 * 2.  Serialize message structures for sending down sockets
 *
 * 3.  actual logic to send msgs, receive responses and unpack them
 *
 * 4.  once communications are established, enough logic to do something basic with
 *     received geometry to validate it, like list all objects.
 */


int
main(int argc, char **argv) {
        /* TODO */

	/* Set up a vanilla socket based connector to the GS - better if we don't assume
	 * libpkg so we can demonstrate reliance on nothing but the protocol docs */

	/* Define a command table so we can do things like type "ping" on the 
	 * server command prompt to send GSPING to the server */

	/* process either file or command line args (or both) for server info */

        /* set up command line environment and report initial results of first attempt
	 * to contact the server.  Accept user input, parse it through the command
	 * table and do whatever the command is. */
}
