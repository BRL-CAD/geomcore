/*
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file GSConnection.java
 *
 */
package org.brlcad.geometryservice.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;

public class GSConnection {

	public static final int SOCKET_READBUF_SIZE = 1024 * 32;

	public static GSConnection connectToHost(InetAddress addy, short port, String username, String password) throws IOException {

		/* Make connection */
		Socket sock = new Socket(addy, port);
		GSConnection conn = new GSConnection(sock);

		/* Handshake */

		
		return null;
	}

	private final Socket sock;
	private final byte[] readBuf;

	private GSConnection(Socket sock) {
		this.sock = sock;
		this.readBuf = new byte[SOCKET_READBUF_SIZE];
	}

	public ByteBuffer read() {
		InputStream is = null;
		int bytesRead = 0;

		try {
			is = this.sock.getInputStream();
			bytesRead = is.read(this.readBuf);
			
		} catch (IOException ioe) {
			this.disconnect();
			return null;

		} catch (Exception e) {
			this.disconnect();
			return null;
		}

		if (bytesRead < 0) {
			this.disconnect();
			return null;
		}
		
		if (bytesRead == 0)
			
		
		if (bytesRead < GSConnection .SOCKET_READBUF_SIZE ) {
			
		}
		
		
		return null;
	}

	public void disconnect() {

	}

}
