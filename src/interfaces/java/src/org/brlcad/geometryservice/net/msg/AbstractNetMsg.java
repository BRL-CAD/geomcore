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
/** @file AbstractNetMsg.java
 *
 */

package org.brlcad.geometryservice.net.msg;

import java.util.UUID;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;
import org.brlcad.geometryservice.net.GSNetMsgFutureResponse;

public abstract class AbstractNetMsg {

	/* Header */
	protected final short msgType;
	protected UUID msgUUID;
	protected boolean hasReUUID;
	protected UUID reUUID;
	private GSNetMsgFutureResponse futureRes = null;
	
	
	/* Cstr used for instantiating an object manually */
	protected AbstractNetMsg(short msgType) {
		this.msgType = msgType;
		this.msgUUID = UUID.randomUUID();
		this.hasReUUID = false;
		this.reUUID = null;
	}

	/*
	 * Cstr used for instantiating an object manually, but replying regarding
	 * another msg.
	 */
	protected AbstractNetMsg(short msgType, AbstractNetMsg reMsg) {
		this.msgType = msgType;
		this.msgUUID = UUID.randomUUID();
		this.hasReUUID = true;
		this.reUUID = reMsg.getMsgUUID();
	}

	/* Cstr used for deserializing an object */
	protected AbstractNetMsg(short msgType, ByteBufferReader reader) {
		this.msgType = msgType;
		
		/* Header items */
		this.msgUUID = reader.getUUID();
		this.hasReUUID = reader.getBoolean();

		if (this.hasReUUID)
			this.reUUID = reader.getUUID();

		/* Subclass items */
		this._deserialize(reader);
	}

	/* Force subclasses to implement a means of deserialization */
	protected abstract void _deserialize(ByteBufferReader reader);

	/* method used to serializing object's current state */
	public void serialize(ByteBufferWriter writer) {
		int lenPos = 0;
		
		/* GS Header items */
		writer.putShort(this.msgType);
		lenPos = writer.position();
		writer.putInt(0);
		writer.putUUID(this.msgUUID);
		writer.putBoolean(this.hasReUUID);

		if (this.hasReUUID)
			writer.putUUID(this.reUUID);

		/* Subclass items */
		this._serialize(writer);

		/* Calc and insert msgLen */
		int dataLen = writer.position() - 6;
		writer.putIntAt(dataLen, lenPos);
	}

	/* Force subclasses to implement a means of serialization */
	protected abstract void _serialize(ByteBufferWriter writer);

	/*
	 * Getters
	 */

	/**
	 * @returns UUID - the Message's UUID;
	 */
	public final UUID getMsgUUID() {
		return msgUUID;
	}

	/**
	 *
	 * @return boolean - indicates where there is a reUUID or not.
	 */
	public final boolean hasReUUID() {
		return hasReUUID;
	}

	/**
	 *
	 * @return UUID - the Message's reUUID
	 */
	public final UUID getReUUID() {
		return reUUID;
	}

	/**
	 * @return short - the Message's msgType
	 */
	public final short getMsgType() {
		return msgType;
	}
	
	public final GSNetMsgFutureResponse getFutureResponse() {
		//Lazy init;
		if (this.futureRes == null)
			this.futureRes = new GSNetMsgFutureResponse(this.msgUUID);

		return this.futureRes;
	}

}
