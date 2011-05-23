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
/** @file NewSessionReqMsg.java
 *
 */
package org.brlcad.geometryservice.net.msg;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

/**
 * @author david.h.loman
 *
 */
public class NewSessionReqMsg extends AbstractNetMsg {

	private String uname;
	private String passwd;

	/**
	 * @param msgType
	 */
	public NewSessionReqMsg(String uname, String passwd) {
		super(NetMsgTypes.NewSessionREQ);
		this.uname = uname;
		this.passwd = passwd;
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public NewSessionReqMsg(AbstractNetMsg reMsg, String uname, String passwd) {
		super(NetMsgTypes.NewSessionREQ, reMsg);
		this.uname = uname;
		this.passwd = passwd;
	}

	/**
	 * @param msgType
	 * @param reader
	 */
	public NewSessionReqMsg(ByteBufferReader reader) {
		super(NetMsgTypes.NewSessionREQ, reader);
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.uname = reader.getString();
		this.passwd = reader.getString();
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.putString(this.uname);
		writer.putString(this.passwd);
	}

	/**
	 * @return the uname
	 */
	public final String getUname() {
		return uname;
	}

	/**
	 * @return the passwd
	 */
	public final String getPasswd() {
		return passwd;
	}
}
