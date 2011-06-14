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
/** @file RemoteNodeNameSetMsg.java
 *
 */
package org.brlcad.geometryservice.net.msg;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class DirListReqMsg extends AbstractNetMsg {

	private String path;

	/**
	 * @param reader
	 */
	public DirListReqMsg(ByteBufferReader reader) {
		super(NetMsgTypes.DirListREQ, reader);
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public DirListReqMsg(AbstractNetMsg reMsg, String path) {
		super(NetMsgTypes.DirListREQ, reMsg);
		this.path = path;
	}

	/**
	 * @param msgType
	 */
	public DirListReqMsg(String path) {
		super(NetMsgTypes.DirListREQ);
		this.path = path;
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.path = reader.getString();
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.putString(this.path);
	}

	/**
	 * @return the path
	 */
	public final String getPath() {
		return path;
	}

}
