#include "bu.h"
#include "pkg.h"

/* Define Magic numbers and Message Types for GS Protocol */

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

/* Need:
 *
 * 1.  Definitions of structures to be sending and receiving (probably structs)
 *
 * 2.  Command prompt and basic set of commands to bounce basic requests at
 *     the server - command table and very basic parsing.
 *
 * 3.  actual logic to pack things up, send them, and unpack them (see g_transfer.c)
 *
 * 4.  once communications are established, enough logic to do something basic with
 *     received geometry to validate it, like list all objects.
 */

int
main(int argc, char **argv) {
	struct pkg_conn *connection;
	const char *server;
	int port = 5309;
	char s_port[32] = {0};
	int bytes_sent = 0;
	int pkg_result = 1;

	/* First, make sure we can do SOMETHING - hard code everything for now */
	if (!argv[1]) bu_exit(1, "Please supply server address\n");
	server = argv[1];
	snprintf(s_port, 31, "%d", port);
	connection = pkg_open(server, s_port, "tcp",  NULL, NULL, NULL, NULL);
	if (connection == PKC_ERROR) {
		bu_exit(1, "Connection to %s, port %d, failed.\n", server, port);
	}
        bytes_sent = pkg_send(GSRUALIVE, NULL, 0, connection);
	if (bytes_sent < 0) {
		pkg_close(connection);
		bu_exit(1, "Unable to successfully send GSRUALIVE to %s, port %d\n", server, port);
	} else {
		bu_log("Sent %p to %s\n", GSRUALIVE, server);
	}


        /* TODO */

	/* Define a command table so we can do things like type "ping" on the 
	 * server command prompt to send GSPING to the server */

	/* Figure out how pkg_switch works and what we need it to do if anything
	 * we'll probably need to respond to server reports by doing things like
	 * printing "PONG" if the server sends it back to us (and more sophisticated
	 * things with geometry returns, of course) */
	struct pkg_switch callbacks[] = {
		{0, 0, NULL, NULL},
		{0, 0, NULL, NULL},
		{0, 0, NULL, NULL},
		{0, 0, NULL, NULL},
		{0, 0, NULL, NULL}
	};

	/* process either file or command line args (or both) for server info */

        /* set up command line environment and report initial results of first attempt
	 * to contact the server.  Accept user input, parse it through the command
	 * table and do whatever the command is. */
}
