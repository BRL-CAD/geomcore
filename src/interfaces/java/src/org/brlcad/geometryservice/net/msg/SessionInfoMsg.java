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
/** @file SessionInfo.java
 * 
 */
package org.brlcad.geometryservice.net.msg;

import java.util.UUID;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

/**
 * @author david.h.loman
 * 
 */
public class SessionInfoMsg extends AbstractNetMsg {

	private UUID sessionID;

	/**
	 * @param msgType
	 */
	public SessionInfoMsg(UUID sessionID) {
		super(NetMsgTypes.NewSessionREQ);
		this.sessionID = sessionID;
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public SessionInfoMsg(AbstractNetMsg reMsg, UUID sessionID) {
		super(NetMsgTypes.NewSessionREQ, reMsg);
		this.sessionID = sessionID;
	}

	/**
	 * @param msgType
	 * @param reader
	 */
	public SessionInfoMsg(ByteBufferReader reader) {
		super(NetMsgTypes.NewSessionREQ, reader);
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.sessionID = reader.getUUID();
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.putUUID(this.sessionID);
	}

	/**
	 * @return the sessionID
	 */
	public final UUID getSessionID() {
		return sessionID;
	}

}
