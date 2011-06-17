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
import java.net.BindException;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.NoRouteToHostException;
import java.net.PortUnreachableException;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.GeometryServiceException;
import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.NetMsgTypes;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;
import org.brlcad.geometryservice.net.msg.RemoteNodeNameSetMsg;
import org.brlcad.geometryservice.net.msg.SessionInfoMsg;
import org.brlcad.geometryservice.net.msg.TypeOnlyMsg;

/**
 * A stand-alone Thread implementation of a connection to a Geometry Service.
 * Provides built in handshaking and authentication with a Geometry Service
 * server as well as a fully functional NetMsg factory deserialization loop.
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
	 * Time, in milliseconds, the connection waits to complete the handshake
	 * with the Geometry Server
	 */
	public static final long MAX_HANDSHAKE_WAIT_TIME_MS = 1000 * 5;
	/**
	 * Time, in milliseconds, the connection waits to complete authenticating
	 * with the Geometry Server
	 */
	public static final long MAX_AUTH_WAIT_TIME_MS = 1000 * 5;

	private AtomicBoolean recvRunStatus = new AtomicBoolean(false);
	private AtomicBoolean recvRunCmd = new AtomicBoolean(false);

	/**
	 * Static factory method that will connect to the provided addy:port and
	 * attempt to log in.
	 * 
	 * @param addy
	 * @param port
	 * @param uname
	 * @param passwd
	 * @return
	 * @throws IOException
	 */
	public static GSConnectionManipulator connectToHost(InetAddress addy, short port,
			String uname, String passwd, GSConnectionManipulator manip) {

		if (manip == null)
			manip = new GSConnectionManipulator();
		
		/* Validate args */
		if (addy == null) {
			manip.setRetVal(-1);
			manip.setRetStr("NULL address");
			return manip;
		}
		if (port < 1) {
			manip.setRetVal(-2);
			manip.setRetStr("Port is < 1");
			return manip;
		}
		if (uname == null || uname.length() < 1) {
			manip.setRetVal(-3);
			manip.setRetStr("NULL or zero length uname");
			return manip;
		}
		if (passwd == null || passwd.length() < 1) {
			manip.setRetVal(-4);
			manip.setRetStr("NULL or zero length passwd");
			return manip;
		}

		Socket sock = null;
		GSConnection conn = null;

		try {
			/* Make connection */
			sock = new Socket(addy, port);
			conn = new GSConnection(sock);
			conn.start();

			/* Handshake */
			if (!GSConnection.handshake(conn, uname)) {
				manip.setRetVal(-5);
				manip.setRetStr("Handshake Failed");
				return manip;
			}

			/* Authenticate */
			if (!GSConnection.authenticate(conn, uname, passwd)) {
				manip.setRetVal(-6);
				manip.setRetStr("Authentication Failed");
				return manip;
			}

			if (conn != null) {
				manip.setRetVal(1);
				manip.setConn(conn);
			}
		} catch (BindException e) {
			manip.setRetVal(-7);
			manip.setRetStr("BindException: " + e.getMessage());

		} catch (ConnectException e) {
			manip.setRetVal(-8);
			manip.setRetStr("ConnectException: " + e.getMessage());

		} catch (NoRouteToHostException e) {
			manip.setRetVal(-9);
			manip.setRetStr("NoRouteToHostException: " + e.getMessage());

		} catch (PortUnreachableException e) {
			manip.setRetVal(-10);
			manip.setRetStr("PortUnreachableException: " + e.getMessage());

		} catch (GeometryServiceException e) {
			manip.setRetVal(-11);
			manip.setRetStr("GeometryServiceException: " + e.getMessage());

		} catch (Exception e) {
			manip.setRetVal(-12);
			manip.setRetStr("GSJavaInterface::connectToHost(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());

		}
		return manip;
	}
	
	/**
	 * Private function to handle the specifics (including timeout and blocking)
	 * of the handshake process with a GeometryServer.
	 * 
	 * @param conn
	 *            GSConnection to handshake on
	 * @param uname
	 *            Username to send to the remote host.
	 * @return a boolean value that indicates whether the handshake was a
	 *         success or not.
	 * @throws GeometryServiceException
	 */
	private static final boolean handshake(GSConnection conn, String uname)
			throws GeometryServiceException {
		String localNodeName = uname + "-" + conn.getLocalNodename();

		RemoteNodeNameSetMsg rnnsm = new RemoteNodeNameSetMsg(localNodeName);
		GSNetMsgFutureResponse response = rnnsm.getFutureResponse();
		conn.send(rnnsm, true);

		AbstractNetMsg inMsg = response
				.blockUntilResponse(MAX_HANDSHAKE_WAIT_TIME_MS);

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
		GSNetMsgFutureResponse response = reqMsg.getFutureResponse();
		conn.send(reqMsg, true);

		AbstractNetMsg inMsg = response
				.blockUntilResponse(MAX_AUTH_WAIT_TIME_MS);

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

	private HashMap<String, GSNetMsgFutureResponse> responseMap;

	private GSConnection(Socket sock) {
		this.sock = sock;
		this.connReadBuf = ByteBuffer.allocate(CONN_INITIAL_READBUF_SIZE);
		this.socketReadBuf = new byte[SOCKET_READBUF_SIZE];
		this.localNodename = UUID.randomUUID().toString();
		this.remoteNodename = "NOT_SET_YET";
		this.responseMap = new HashMap<String, GSNetMsgFutureResponse>(50);
	}

	public void disconnect() {
		this.send(new TypeOnlyMsg(NetMsgTypes.DisconnectREQ));
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			GSStatics.stdErr.println("GSConnection::disconnect(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());
		}
		return;
	}

	public synchronized ArrayList<AbstractNetMsg> recv(
			ArrayList<AbstractNetMsg> orphans) {

		/* Assume connReadBuf is positioned at the end of existing 'good' data */
		int totalRead = this.pullInFromSocket();

		if (totalRead < 0) /* Error */
			return null;

		/* Now try to make netMsgs */
		ArrayList<AbstractNetMsg> list = new ArrayList<AbstractNetMsg>();
		/* If nothing read, AND nothing currently buffered, bail out. */
		if (totalRead == 0 && this.connReadBuf.position() < 1)
			return list;

		/*
		 * At this point, connReadBuff should be positioned at the end of the
		 * good data.
		 */
		int retVal = NetMsgFactory
				.parseBufferForNetMsgs(this.connReadBuf, list);

		if (retVal <= 0) {
			GSStatics.stdErr.println("You fed the parser fn something NULL!!!");
			return null;
		}

		/* Now check incoming msgs for response registrations. */
		String reID_str = "";
		UUID reID_UUID = null;
		GSNetMsgFutureResponse res = null;

		for (AbstractNetMsg msg : list) {
			if (msg.hasReUUID() == false) {
				orphans.add(msg);
				continue;
			}

			reID_UUID = msg.getReUUID();
			if (reID_UUID == null) {
				orphans.add(msg);
				continue;
			}

			reID_str = reID_UUID.toString();
			if (reID_str.length() < 1) {
				orphans.add(msg);
				continue;
			}

			res = this.responseMap.get(reID_str);

			if (res == null) {
				orphans.add(msg);
				continue;
			}

			this.responseMap.remove(reID_str);
			res.setResponseMsg(msg);
		}

		return orphans;
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
			GSStatics.stdErr.println("GSConnection::pullInFromSocket(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());
			return -1;
		}

		do {
			bytesReadLast = this.read(this.socketReadBuf);

			if (bytesReadLast == 0)
				break;

			// System.out.println("Got " + bytesReadLast + " bytes.");

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
			GSStatics.stdErr.println("GSConnection::pullInFromSocket(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());
			return -1;
		}
		return total;
	}

	private final int read(byte[] ba) {
		InputStream is = null;
		int bytesRead = 0;

		try {
			is = this.sock.getInputStream();
			bytesRead = is.read(ba);

		} catch (SocketTimeoutException ste) {
			/* No worries here */
			return 0;

		} catch (IOException ioe) {
			this.disconnect();
			return -1;

		} catch (Exception e) {
			this.disconnect();
			GSStatics.stdErr.println("GSConnection::read(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());
			return -1;
		}

		if (bytesRead < 0) {
			this.disconnect();
			return -1;
		}
		return bytesRead;
	}

	public boolean send(AbstractNetMsg msg) {
		return this.send(msg, false);
	}

	public boolean send(AbstractNetMsg msg, boolean expectReply) {

		if (expectReply) {
			GSNetMsgFutureResponse res = msg.getFutureResponse();
			String msgID = msg.getMsgUUID().toString();
			// GSStatics.stdOut.println("Mapping for '" + msgID + "'");
			this.responseMap.put(msgID, res);
		}

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
				GSStatics.stdErr
						.println("GSConnection::send()->msg.serialize(): "
								+ e.getClass().getSimpleName() + ":"
								+ e.getMessage());
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
			GSStatics.stdErr.println("GSConnection::send()->this.write(): "
					+ e.getClass().getSimpleName() + ":" + e.getMessage());
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
		// String out = "";
		for (int i = 0; i < amtToWrite; ++i) {
			byte b = bb.get();
			os.write(b);
			// out += Integer.toString(b & 0xff, 16).toUpperCase();
		}
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
		// System.out.println("GS Connection.run() entered.");

		this.recvRunStatus.set(true);
		this.recvRunCmd.set(true);
		ArrayList<AbstractNetMsg> orphans = new ArrayList<AbstractNetMsg>(10);
		ArrayList<AbstractNetMsg> test = null; 
		
		while (this.recvRunCmd.get()) {
			orphans.clear();
			test = this.recv(orphans);
			if (test == null) 
				break;
			
			if (orphans.size() > 0)
				GSStatics.stdErr.println("GSConnection::run() Got "
						+ orphans.size() + " orphaned NetMsgs.");

			try {
				Thread.sleep(100);
			} catch (Exception e) {
				GSStatics.stdErr
						.println("GSConnection::run()" + e.getMessage());
				return;
			}
		}

		this.recvRunStatus.set(false);
		// System.out.println("GS Connection.run() exiting.");
	}

	public void stopReceiving() {

		this.recvRunCmd.set(false);
	}

	public boolean isReceiving() {
		return this.recvRunStatus.get();
	}

	public static class GSConnectionManipulator {
		private GSConnection conn;
		private int retVal = 0;
		private String retStr = "";

		public GSConnection getConn() {
			return conn;
		}

		public void setConn(GSConnection conn) {
			this.conn = conn;
		}

		public boolean isConnNull() {
			return (this.conn == null);
		}

		public int getRetVal() {
			return retVal;
		}

		public void setRetVal(int retVal) {
			this.retVal = retVal;
		}

		public String getRetStr() {
			return retStr;
		}

		public void setRetStr(String retStr) {
			this.retStr = retStr;
		}
	}	
	
}
