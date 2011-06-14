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
/** @file FailureMsg.java
 * 
 */
package org.brlcad.geometryservice.net.msg;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class SuccessMsg extends AbstractNetMsg {
	private byte successCode;

	/**
	 * @param msgType
	 */
	public SuccessMsg(byte successCode) {
		super(NetMsgTypes.Success);
		this.successCode = successCode;
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public SuccessMsg(AbstractNetMsg reMsg, byte successCode) {
		super(NetMsgTypes.Success, reMsg);
		this.successCode = successCode;
	}

	/**
	 * @param msgType
	 * @param reader
	 */
	public SuccessMsg(ByteBufferReader reader) {
		super(NetMsgTypes.Success, reader);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad
	 * .geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		this.successCode = reader.get();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad
	 * .geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		writer.put(this.successCode);
	}

	public byte getSuccessCode() {
		return successCode;
	}

}
