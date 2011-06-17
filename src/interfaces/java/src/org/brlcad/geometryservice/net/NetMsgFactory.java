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
/** @file NetMsgFactory.java
 *
 */
package org.brlcad.geometryservice.net;

import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.DirListReqMsg;
import org.brlcad.geometryservice.net.msg.DirListResMsg;
import org.brlcad.geometryservice.net.msg.FailureMsg;
import org.brlcad.geometryservice.net.msg.GeometryChunkMsg;
import org.brlcad.geometryservice.net.msg.GeometryManifestMsg;
import org.brlcad.geometryservice.net.msg.GeometryReqMsg;
import org.brlcad.geometryservice.net.msg.NetMsgChangeTracker;
import org.brlcad.geometryservice.net.msg.NetMsgTypes;
import org.brlcad.geometryservice.net.msg.NewNodeOnNetMsg;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;
import org.brlcad.geometryservice.net.msg.PingMsg;
import org.brlcad.geometryservice.net.msg.PongMsg;
import org.brlcad.geometryservice.net.msg.RemoteNodeNameSetMsg;
import org.brlcad.geometryservice.net.msg.SessionInfoMsg;
import org.brlcad.geometryservice.net.msg.SuccessMsg;
import org.brlcad.geometryservice.net.msg.TypeOnlyMsg;

public final class NetMsgFactory {

	public static final int ERR_BYTEBUFFER_NULL = -1;
	public static final int ERR_ARRAYLIST_NULL = -2;
	
	public static final int PARSE_OK = 0;
	public static final int NO_HEADER = 1;
	public static final int DATA_OK_NOT_ALL_HERE = 2;
	public static final int DATA_OK_WILL_NOT_FIT = 3;
	public static final int PARSE_FATAL_ERROR = -100;
		/**
		 * 
		 * @param bb ByteBuffer in which .position() marks then END of the valid Data.
		 * @param msgs
		 * @return codes:
		 * 	>=0  Number of NetMsgs parsed.
		 *  ERR_BYTEBUFFER_NULL =  null ByteBuffer arg
		 *  ERR_ARRAYLIST_NULL  =  null ArrayList<AbstractNetMsg> arg
		 *  
		 */
	public static int parseBufferForNetMsgs(ByteBuffer bb, ArrayList<AbstractNetMsg> msgs) {
		
		if (bb == null)
			return ERR_BYTEBUFFER_NULL;
		if (msgs == null)
			return ERR_ARRAYLIST_NULL;
		
		int endOfGoodData = bb.position();
		if (endOfGoodData == 0) 
			return 0; /* No data is in buffer, bail out early */
		bb.flip();
		
		NetMsgChangeTracker tracker = NetMsgChangeTracker.getChangeTracker();
		
		do {
			tracker.clear();
			tracker = NetMsgFactory.parseBufferForANetMsg(bb, tracker);
		
			if (tracker.getChangeValue() == PARSE_OK)
				msgs.add(tracker.getMsg());
		
		} while(tracker.getChangeValue() == PARSE_OK);
		
		// if we get here, then parsing stopped for some reason.
		int cv = tracker.getChangeValue();
		if (cv == NO_HEADER) {
			/* No worries here. */
		} else 	if (cv == DATA_OK_NOT_ALL_HERE) {
			/* No worries here. */
		} else 	if (cv == DATA_OK_WILL_NOT_FIT) {
			/* No worries here. */	
		} else 	if (cv == PARSE_FATAL_ERROR) {
			/* No worries here. */
		} else {
			/* Unknown change value! */
			GSStatics.stdErr.println("FATAL ERROR on parse: " + tracker.getChangeString());
		}

		bb.compact();
		
		return msgs.size();
	}
	
	/**
	 * 
	 * @param bb  Bytebuffer who's .position() is at the beginning of the data that is to be parsed.  
	 * 		Upon return, and data between 0 and .position() is guaranteed to be parsed and can be deleted.
	 * @param tracker
	 * @return NetMsgChangeTracker object.  
	 * 	Change codes:
	 * 	 PARSE_OK      = Parse completed successfully, reference to NetMsg contained in 'tracker'
	 * 	 NO_HEADER     = Less than 6 bytes available (2bytes for type, 4 for len)
	 * 	 DATA_OK_NOT_ALL_HERE = Msg has not completely arrived yet.
	 * 	 DATA_OK_WILL_NOT_FIT = Msg has not completely arrived yet and will not fit in the current buffer.
	 * 	 PARSE_FATAL_ERROR    = unknown, but fatal error happened.  ByteBuffer will returned 'fast forwarded' to 
	 * 			 the end of the msg with the error, or to the limit(), whichever is less.
	 */
	public static NetMsgChangeTracker parseBufferForANetMsg(ByteBuffer bb, NetMsgChangeTracker tracker) {
		if (tracker == null)
			tracker = NetMsgChangeTracker.getChangeTracker();
		
		AbstractNetMsg msg = null;
		int startPos = 0;
		int endPos = 0;
		int msgLen = 0;
		short type = 0;
		
		/* Check if there is at least the type and len parameters here */
		if (bb.remaining() < 6) {
			tracker.setChangeValue(NO_HEADER);
			return tracker; /* Bail if < 6 bytes */
		}
		
		/* Make a non-copy, non-endian-flip BBReader */
		ByteBufferReader reader = new ByteBufferReader(bb, false, false);
		
		/* Read params, calc range */
		startPos = bb.position();
		type = reader.getShort();
		msgLen = reader.getInt();
		endPos = startPos + msgLen;
		
		/* Range check */
		if (endPos > bb.limit()) {
			tracker.setChangeValue(DATA_OK_NOT_ALL_HERE);
			bb.position(startPos);
			
			if (endPos > bb.capacity()) {
				tracker.setChangeValue(DATA_OK_WILL_NOT_FIT);
				//TODO do something to premptively resize??
			}
			return tracker;
		}

		try {
			msg = NetMsgFactory.makeMsg(type, reader);
			tracker.setChangeValue(PARSE_OK);
			tracker.setMsg(msg);
			
		} catch (BufferUnderflowException e) {
			/* This shouldn't have happened, but merely indicates that the msg hasn't fully arrived yet. */
			tracker.setChangeValue(DATA_OK_NOT_ALL_HERE);
			bb.position(startPos);

		} catch (Exception e) {
			String err = "NetMsgFactory::makeMsg(): ";
			err += e.getClass().getSimpleName() + ":" + e.getMessage();
			err += "Error occurred at bb.position()="
					+ bb.position();

			/* Fast forward past data if the entire message is here. */
			if (endPos < bb.limit())
				bb.position(endPos);
			else
				bb.position(bb.limit());

			tracker.setChangeValue(PARSE_FATAL_ERROR);
			tracker.setChangeString(err);
		}
				
		return tracker;
	}
	
	
	public static AbstractNetMsg makeMsg(short type, ByteBufferReader reader) {
		switch (type) {
		case NetMsgTypes.RUAlive:
			return new TypeOnlyMsg(type, reader);
		case NetMsgTypes.IMAlive:
			return new TypeOnlyMsg(type, reader);
		case NetMsgTypes.Failure:
			return new FailureMsg(reader);
		case NetMsgTypes.Success:
			return new SuccessMsg(reader);
		case NetMsgTypes.Ping:
			return new PingMsg(reader);
		case NetMsgTypes.Pong:
			return new PongMsg(reader);
		case NetMsgTypes.RemNodeNameSET:
			return new RemoteNodeNameSetMsg(reader);
		case NetMsgTypes.DisconnectREQ:
			return new TypeOnlyMsg(type, reader);
		case NetMsgTypes.NewNodeOnNet:
			return new NewNodeOnNetMsg(reader);
			
		case NetMsgTypes.DirListREQ:
			return new DirListReqMsg(reader);
		case NetMsgTypes.DirListRES:
			return new DirListResMsg(reader);
			
		case NetMsgTypes.NewSessionREQ:
			return new NewSessionReqMsg(reader);
		case NetMsgTypes.SessionInfo:
			return new SessionInfoMsg(reader);
		case NetMsgTypes.GeometryREQ:
			return new GeometryReqMsg(reader);
		case NetMsgTypes.GeometryMANIFEST:
			return new GeometryManifestMsg(reader);
		case NetMsgTypes.GeometryCHUNK:
			return new GeometryChunkMsg(reader);

		default:
			return null;
		}
	}
}
