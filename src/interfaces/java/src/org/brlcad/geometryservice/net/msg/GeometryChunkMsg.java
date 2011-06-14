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
/** @file RemotepathSetMsg.java
 *
 */
package org.brlcad.geometryservice.net.msg;

import java.nio.ByteBuffer;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class GeometryChunkMsg extends AbstractNetMsg {

	private String path;
	private ByteBuffer data;

	/**
	 * @param reader
	 */
	public GeometryChunkMsg(ByteBufferReader reader) {
		super(NetMsgTypes.GeometryREQ, reader);
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public GeometryChunkMsg(AbstractNetMsg reMsg, String path, ByteBuffer dataIn) {
		super(NetMsgTypes.GeometryREQ, reMsg);
		this.path = path;
		this.data = dataIn.duplicate();
	}

	/**
	 * @param msgType
	 */
	public GeometryChunkMsg(String path, ByteBuffer dataIn) {
		super(NetMsgTypes.GeometryREQ);
		this.path = path;
		this.data = dataIn.duplicate();
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.path = reader.getString();
		int len = reader.getInt();
		this.data = ByteBuffer.allocate(len);
		reader.get(this.data.array());
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.putString(this.path);
		writer.putInt(this.data.position());
		writer.putBB(this.data);
	}

	/**
	 * @return the path
	 */
	public final String getPath() {
		return path;
	}

	public ByteBuffer getData() {
		return data;
	}

}
