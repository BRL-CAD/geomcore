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
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.GeometryServiceException;
import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;
import org.brlcad.geometryservice.net.msg.RemoteNodeNameSetMsg;
import org.brlcad.geometryservice.net.msg.SessionInfoMsg;

/**
 * A stand-alone Thread implementation of a connection to a Geometry Service.
 * Provides built in handshaking and authentication with a Geometry Service server
 * as well as a fully functional NetMsg factory deserialization loop.
 *
 */
public class GSConnection extends Thread {

	/**
	 * This connection's initial read buffer's size
	 */
	public static final int CONN_INITIAL_READBUF_SIZE = 1024 * 1024;

	/**
	 * temporary buffer used in read loops when reading from the socket
	 */
	public static final int SOCKET_READBUF_SIZE = 1024 * 8;
	
	/**
	 * Time, in milliseconds, the connection waits to complete the handshake with 
	 * the Geometry Server
	 */
	public static final long MAX_HANDSHAKE_WAIT_TIME_MS = 1000 * 5;

	private AtomicBoolean recvRunStatus = new AtomicBoolean(false);
	private AtomicBoolean recvRunCmd = new AtomicBoolean(false);

	/**
	 * Static factory method that will connect to the provided addy:port and attempt to
	 * log in.
	 *
	 * @param addy
	 * @param port
	 * @param uname
	 * @param passwd
	 * @return
	 * @throws IOException
	 */
	public static GSConnection connectToHost(InetAddress addy, short port,
			String uname, String passwd) throws IOException,
			GeometryServiceException {

		/* Validate args */
		if (addy == null || port < 1)
			throw new GeometryServiceException("NULL address or port is < 1");
		if (uname == null || uname.length() < 1)
			throw new GeometryServiceException("NULL or zero length uname");
		if (passwd == null || passwd.length() < 1)
			throw new GeometryServiceException("NULL or zero length passwd");

		/* Make connection */
		Socket sock = new Socket(addy, port);
		GSConnection conn = new GSConnection(sock);

		/* Handshake */
		if (!GSConnection.handshake(conn, uname)) 
			throw new GeometryServiceException("Handshake Failed.");

		/* Authenticate */
		if (!GSConnection.authenticate(conn, uname, passwd)) 
			throw new GeometryServiceException("Authentication Failed.");

		return conn;
	}

	/**
	 * Calling thread waits (Sleeps) until either a NetMsg arrives from the GS Server or 
	 * @param conn
	 * @return
	 * @throws GeometryServiceException
	 */
	private static AbstractNetMsg waitForMsg(GSConnection conn)
			throws GeometryServiceException {
		AbstractNetMsg newMsg = null;
		int totalRead = 0;
		long startTime = System.currentTimeMillis();

		while (newMsg == null) {
			while (totalRead < 1) {
				totalRead = conn.pullInFromSocket();

				/* if nothing read yet, sleep a bit */
				if (totalRead == 0) {

					if ((System.currentTimeMillis() - startTime) >= MAX_HANDSHAKE_WAIT_TIME_MS)
						throw new GeometryServiceException(
								"Timeout on handshake.  Waited "
										+ MAX_HANDSHAKE_WAIT_TIME_MS + "ms.");

					try {
						Thread.sleep(15);
					} catch (InterruptedException e) {
						throw new GeometryServiceException(
								"Thread Interruption");
					}
					continue;
				}

				if (totalRead < 0)
					throw new GeometryServiceException(
							"Socket closed before handshake complete.");

			} // End while (totalRead < 1) {

			/* Okay, some data showed up, lets make a NetMsg */
			ByteBufferReader reader = new ByteBufferReader(conn.connReadBuf);
			newMsg = conn.tryMakeNetMsg(reader);
			totalRead = 0;		
		}// End while (inMsg == null) {
		return newMsg;
	}

	/**
	 * Private function to handle the specifics 
	 * (including timeout and blocking) of the handshake process with a GeometryServer.
	 * 
	 * @param conn GSConnection to handshake on
	 * @param uname Username to send to the remote host.
	 * @return a boolean value that indicates whether the handshake was a success or not.
	 * @throws GeometryServiceException
	 */
	private static final boolean handshake(GSConnection conn, String uname)
			throws GeometryServiceException {
		String localNodeName = uname + "-" + conn.getNodename();
		conn.send(new RemoteNodeNameSetMsg(localNodeName));

		AbstractNetMsg inMsg = null;
		
		try {
			inMsg = GSConnection.waitForMsg(conn);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			return false;
		}
				
		if (inMsg == null)
			/* handshake failed! */
			return false;

		if ((inMsg instanceof RemoteNodeNameSetMsg) == false)
			/* handshake failed! */
			return false;
		
		RemoteNodeNameSetMsg remMsg = (RemoteNodeNameSetMsg) inMsg;

		if (remMsg.getNodeName().length() == 0)
			/* handshake failed! */
			return false;

		conn.remoteNodename = remMsg.getNodeName();
		return true;
	}

	private static final boolean authenticate(GSConnection conn, String uname,
			String passwd) throws GeometryServiceException {
		NewSessionReqMsg reqMsg = new NewSessionReqMsg(uname, passwd);
		conn.send(reqMsg);

		AbstractNetMsg inMsg = GSConnection.waitForMsg(conn);

		if (inMsg == null)
			/* Auth failed! */
			return false;

		if ((inMsg instanceof SessionInfoMsg) == false)
			/* Auth failed! */
			return false;

		SessionInfoMsg infoMsg = (SessionInfoMsg) inMsg;

		conn.setSessionID(infoMsg.getSessionID());
		return true;
	}

	private final Socket sock;
	/**
	 * Buffer used to move data from the socket to the connectionReadBuf
	 */
	private final byte[] socketReadBuf;

	/** Buffer used to the store all the recv'ed data for this Connection */
	private ByteBuffer connReadBuf;

	private final String localNodename;
	private String remoteNodename;

	private UUID sessionID;

	private GSConnection(Socket sock) {
		this.sock = sock;
		this.connReadBuf = ByteBuffer.allocate(CONN_INITIAL_READBUF_SIZE);
		this.socketReadBuf = new byte[SOCKET_READBUF_SIZE];
		this.localNodename = UUID.randomUUID().toString();
		this.remoteNodename = "NOT_SET_YET";
	}

	public void disconnect() {

	}

	public synchronized ArrayList<AbstractNetMsg> recv() {
		/* Assume that connReadBuf has position == 0 at this point */

		int totalRead = this.pullInFromSocket();

		if (totalRead < 0) /* Error */
			return null;

		/* Now try to make netMsgs */
		ArrayList<AbstractNetMsg> list = new ArrayList<AbstractNetMsg>();

		if (totalRead == 0)
			return list;

		AbstractNetMsg msg = null;

		ByteBufferReader reader = new ByteBufferReader(this.connReadBuf);
		try {
			do {
				msg = this.tryMakeNetMsg(reader);

				if (msg != null)
					list.add(msg);

			} while (msg != null);

		} catch (BufferUnderflowException bufe) {
			// exhausted the buffer!
			GSStatics.stdErr.println(bufe.getMessage());
		}

		this.connReadBuf.compact();
		return list;
	}

	private final int pullInFromSocket() {
		int total = 0;
		/* make sure the internal BB's limit is maxed */
		if (this.connReadBuf.limit() < this.connReadBuf.capacity())
			this.connReadBuf.limit(this.connReadBuf.capacity());

		/* pull in all the data off the socket */
		int bytesReadLast = 0;

		int timeout;

		/* Try to set the read Timeout to 100ms */
		try {
			timeout = this.sock.getSoTimeout();
			if (timeout < 1)
				this.sock.setSoTimeout(100);
		} catch (SocketException e) {
			return -1;
		}

		do {
			bytesReadLast = this.read(this.socketReadBuf);

			if (bytesReadLast == 0)
				break;

			System.out.println("Got " + bytesReadLast + " bytes.");

			if (bytesReadLast < 0)
				return -1;

			total += bytesReadLast;

			/*
			 * make sure incoming data will fit into connection's BB, if not,
			 * expand buffer
			 */
			if ((this.connReadBuf.position() + bytesReadLast) > this.connReadBuf
					.capacity())
				this.connReadBuf = ByteBufferUtils
						.doubleBufCapacity(this.connReadBuf);

			this.connReadBuf.put(this.socketReadBuf, 0, bytesReadLast);

		} while (bytesReadLast > 0);

		try {
			/* Return timeout to zero if applicable */
			if (timeout < 1)
				this.sock.setSoTimeout(0);
		} catch (SocketException e) {
			return -1;
		}
		return total;
	}

	private final AbstractNetMsg tryMakeNetMsg(ByteBufferReader reader) {
		this.connReadBuf.flip();

		AbstractNetMsg msg = null;

		int startPos = 0;
		int endPos = 0;
		int msgLen = 0;
		short gsMsgType = 0;

		do {
			gsMsgType = reader.getShort();			
			msgLen = reader.getInt();

			endPos = reader.position() + msgLen - 6;

			// TODO some logic in here about checking endPos against the
			// buffer's Capacity would be a good thing. Could pre-emtively
			// expand buffer to accept the large msg coming

			/*
			 * Check to see if the connections buffer is smaller than the
			 * message. if so, there is no way the entire message has arrived
			 * yet.
			 */
			if (endPos > this.connReadBuf.limit()) {
				// rewind the position back to the beginning of the pkg
				// header
				this.connReadBuf.position(startPos);
				break;
			}


			try {
				msg = NetMsgFactory.makeMsg(gsMsgType, reader);
			} catch (BufferUnderflowException e) {
				// This shouldn't have happened....
				GSStatics.stdErr.println(e.getMessage());
				return null;
			}

			if (msg == null) {
				// Not implemented yet or bogus type. Move position to end
				// of msg, try again.
				this.connReadBuf.position(endPos);
				break;
			}

		} while (msg == null);

		if (this.connReadBuf.position() > 0)
			this.connReadBuf.compact();

		return msg;
	}

	private final int read(byte[] ba) {
		InputStream is = null;
		int bytesRead = 0;

		try {
			is = this.sock.getInputStream();
			bytesRead = is.read(ba);

		} catch (SocketTimeoutException ste){
			/* No worries here*/
			return 0;

		} catch (IOException ioe) {
			this.disconnect();
			return -1;

		} catch (Exception e) {
			this.disconnect();
			GSStatics.stdErr.println(e.getMessage());
			return -1;
		}

		if (bytesRead < 0) {
			this.disconnect();
			return -1;
		}
		return bytesRead;
	}

	public boolean send(AbstractNetMsg msg) {
		ByteBuffer bb = null;

		for (int i = 0; i < 10; ++i) {
			/* Start at 64k. */
			bb = ByteBuffer.allocate(1024 * 64 * (2 ^ i));
			ByteBufferWriter writer = new ByteBufferWriter(bb);

			try {
				/* serialize gs msg */
				msg.serialize(writer);

			} catch (BufferOverflowException boe) {
				// Buffer wasn't big enough, try again!
				continue;
			} catch (Exception e) {
				// something bad happened!!!
				GSStatics.stdErr.println(e.getMessage());
				return false;
			}

			// If we made it here, then the serialization was a success
			break;
		}

		try {
			this.write(bb);
			return true;
		} catch (Exception e) {
			// Something BAD happened on the socket write.
			GSStatics.stdErr.println(e.getMessage());
			return false;
		}
	}

	private final void write(ByteBuffer bb) throws IOException {
		OutputStream os = this.sock.getOutputStream();

		int amtToWrite = bb.position();
		bb.flip();

		/*
		 * Use a loop because OutputStream.write(byte[]) and
		 * OutputStream.write(byte[], int, int) both use loops calling
		 * OutputStream.write(byte) anyways.
		 */
		String out = "";
		for (int i = 0; i < amtToWrite; ++i) {
			byte b = bb.get();
			os.write(b);
			out += Integer.toString(b & 0xff, 16).toUpperCase();
		}

		System.out.println("Sending Data (" + amtToWrite + " bytes): " + out);
	}

	/**
	 * @return the nodename
	 */
	public final String getNodename() {
		return localNodename;
	}

	/**
	 * @return the localNodename
	 */
	public final String getLocalNodename() {
		return localNodename;
	}

	/**
	 * @return the remoteNodename
	 */
	public final String getRemoteNodename() {
		return remoteNodename;
	}

	/**
	 * @return the sessionID
	 */
	public final UUID getSessionID() {
		return sessionID;
	}

	/**
	 * @param sessionID
	 *            the sessionID to set
	 */
	private final void setSessionID(UUID sessionID) {
		this.sessionID = sessionID;
	}

	@Override
	public void run() {
		System.out.println("GS Connection.run() entered.");

		this.recvRunStatus.set(true);
		this.recvRunCmd.set(true);
		
		while (this.recvRunCmd.get()) {
			try {
				Thread.sleep(100);				
			} catch (InterruptedException e) {
				e.printStackTrace();
				return;
			}
		}

		this.recvRunStatus.set(false);
		System.out.println("GS Connection.run() exiting.");
	}

	public void stopReceiving() {
		this.recvRunCmd.set(false);
	}

	public boolean isReceiving() {
		return this.recvRunStatus.get();
	}

}
