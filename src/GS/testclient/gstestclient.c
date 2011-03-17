#include <stdlib.h>
#include <stdio.h>
#include <uuid/uuid.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <inttypes.h>
#include "bu.h"
#include "pkg.h"

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

struct gs_string {
	uint32_t length;
	uint16_t *chararray;
};

struct gs_msg {
	uint16_t msgtype;
	struct gs_string msguuid;
	struct gs_string msgreuuid;
	void *data;
};

struct gsnet_msg {
	uint16_t magic1;
	uint16_t magic2;
	uint32_t msglength;
	struct gs_msg *msg;
};

struct gsnet_msg *create_new_msg(uint16_t mtype, struct gs_string *msgreuuid) {
	struct gsnet_msg *new_msg;
	struct gs_msg *core_msg;
	uuid_t msguuid;
	new_msg = malloc(sizeof(struct gsnet_msg));
	new_msg->magic1 = MAGIC1;
	new_msg->magic2 = MAGIC2;
	core_msg = malloc(sizeof(struct gs_msg));
	new_msg->msg = core_msg;
	core_msg->msgtype = mtype;
	uuid_generate(msguuid);
	core_msg->msguuid.chararray = malloc(sizeof(uuid_t) * 2 + 4);
	uuid_unparse(msguuid, (char *)core_msg->msguuid.chararray);
	core_msg->msguuid.length = strlen((char *)core_msg->msguuid.chararray);
	core_msg->msgreuuid.chararray = malloc(sizeof(uuid_t) * 2 + 4);
	if (msgreuuid) {
		memcpy(msgreuuid->chararray, core_msg->msgreuuid.chararray, sizeof(uuid_t) * 2 + 4);
		core_msg->msgreuuid.length = strlen((char *)core_msg->msgreuuid.chararray);
	}
	return new_msg;
}

void gs_msg_free(struct gs_msg *msg) {
	if (msg->msguuid.chararray) free(msg->msguuid.chararray);
	if (msg->msgreuuid.chararray) free(msg->msgreuuid.chararray);
	if (msg->data) free(msg->data);
	free(msg);
}

void gsnet_msg_free(struct gsnet_msg *netmsg) {
	if (netmsg->msg) gs_msg_free(netmsg->msg);
	free(netmsg);
}

/* Debug print function */
void print_gs_msg(struct gsnet_msg *new_msg) {
	printf("MAGIC1: %p\n", new_msg->magic1);
	printf("MAGIC2: %p\n", new_msg->magic2);
	printf("msgtype: %p\n", new_msg->msg->msgtype);
	printf("msguuid: %s\n", (char *)new_msg->msg->msguuid.chararray);
	if (new_msg->msg->msgreuuid.chararray)
		printf("msgreuuid: %s\n", new_msg->msg->msgreuuid);
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
	struct pkg_conn *connection;
	const char *server;
	int port = 5309;
	char *msg;
	char *nextstr;
	char s_port[32] = {0};
	int bytes_sent = 0;
	int i;
	int gsnetheaderlength = 2 + (sizeof(uuid_t) * 2 + 4) * 2 + 1; /* MsgType + MessageUUID + RegardingMessageUUID + HasRegardingUUID */

	struct gsnet_msg *test_msg;

	if (!argv[1]) bu_exit(1, "Please supply server address\n");
	server = argv[1];
	snprintf(s_port, 31, "%d", port);
	connection = pkg_open(server, s_port, "tcp",  NULL, NULL, NULL, NULL);
	if (connection == PKC_ERROR) {
		bu_exit(1, "Connection to %s, port %d, failed.\n", server, port);
	}

	test_msg = create_new_msg(GSRUALIVE, NULL);
	print_gs_msg(test_msg);
	msg = (char *)bu_malloc(gsnetheaderlength, "msg");
	msg[1] = test_msg->msg->msgtype;
	msg[0] = test_msg->msg->msgtype >> 8;
	memcpy(msg+2, (char *)test_msg->msg->msguuid.chararray, sizeof(uuid_t) * 2 + 4);
	memcpy(msg+2+sizeof(uuid_t) * 2 + 4, (char *)test_msg->msg->msgreuuid.chararray, sizeof(uuid_t) * 2 + 4);
	bytes_sent = pkg_send(5309, msg, strlen(msg), connection);
	gsnet_msg_free(test_msg);

	/* TODO */


	/* Define a command table so we can do things like type "ping" on the 
	 * server command prompt to send GSPING to the server */

	/* process either file or command line args (or both) for server info */

        /* set up command line environment and report initial results of first attempt
	 * to contact the server.  Accept user input, parse it through the command
	 * table and do whatever the command is. */
}
