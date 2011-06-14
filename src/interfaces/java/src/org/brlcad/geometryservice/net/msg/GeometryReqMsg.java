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

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class GeometryReqMsg extends AbstractNetMsg {

	private String path;
	private boolean recurse;

	/**
	 * @param reader
	 */
	public GeometryReqMsg(ByteBufferReader reader) {
		super(NetMsgTypes.GeometryREQ, reader);
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public GeometryReqMsg(AbstractNetMsg reMsg, String path, boolean recurse) {
		super(NetMsgTypes.GeometryREQ, reMsg);
		this.path = path;
		this.recurse = recurse;
	}

	/**
	 * @param msgType
	 */
	public GeometryReqMsg(String path, boolean recurse) {
		super(NetMsgTypes.GeometryREQ);
		this.path = path;
		this.recurse = recurse;
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.path = reader.getString();
		this.recurse = reader.getBoolean();
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.putString(this.path);
		writer.putBoolean(this.recurse);
	}

	/**
	 * @return the path
	 */
	public final String getPath() {
		return path;
	}

	/**
	 * 
	 * @return boolean indicating that request is to recurse down the dir tree or not.
	 */
	public boolean getRecurse() {
		return recurse;
	}

}
