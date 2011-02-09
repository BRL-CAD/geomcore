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

import java.nio.ByteBuffer;
import java.util.UUID;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public abstract class AbstractNetMsg {

	/* Header */
	
	protected short magic01;
	protected short magic02;
	protected int msgLen;

	protected short msgType;
	protected UUID msgUUID;
	protected boolean hasReUUID;
	protected UUID reUUID;
	
	/* Cstr used for instantiating an object manually */
	protected AbstractNetMsg(short msgType) {
		this.magic01 = GSStatics.magic01;
		this.magic02 = GSStatics.magic02;
		this.msgUUID = UUID.randomUUID();
		this.hasReUUID = false;
		this.reUUID = null;
	}
	
	/* Cstr used for instantiating an object manually, but replying regarding another msg. */
	protected AbstractNetMsg(short msgType, AbstractNetMsg reMsg) {
		this(msgType);
		this.hasReUUID = true;
		this.reUUID = reMsg.getMsgUUID();
	}
	
	/* Cstr used for deserializing an object */
	protected AbstractNetMsg(ByteBufferReader reader)
	{
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
	public void serialize(ByteBuffer bb) {
		ByteBufferWriter writer = new ByteBufferWriter(bb);

		/* Header items */
		writer.putUUID(this.msgUUID);
		writer.putBoolean(this.hasReUUID);
		
		if (this.hasReUUID) 
			writer.putUUID(this.reUUID);
		
		/* Subclass items */
		this._serialize(writer);
	}
	
	
	/* Force subclasses to implement a means of serialization */
	protected abstract void _serialize(ByteBufferWriter writer);
	
	
	/*
	 * Getters
	 */
	public final UUID getMsgUUID() {
		return msgUUID;
	}

	public final boolean hasReUUID() {
		return hasReUUID;
	}

	public final UUID getReUUID() {
		return reUUID;
	}
	
}
