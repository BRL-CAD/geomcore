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

public class FailureMsg extends AbstractNetMsg {
	private byte failureCode;

	/**
	 * @param msgType
	 */
	public FailureMsg(byte failureCode) {
		super(NetMsgTypes.Failure);
		this.failureCode = failureCode;
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public FailureMsg(AbstractNetMsg reMsg, byte failureCode) {
		super(NetMsgTypes.Failure, reMsg);
		this.failureCode = failureCode;
	}

	/**
	 * @param msgType
	 * @param reader
	 */
	public FailureMsg(ByteBufferReader reader) {
		super(NetMsgTypes.Failure, reader);
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
		this.failureCode = reader.get();
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
		writer.put(this.failureCode);
	}

	public byte getFailureCode() {
		return failureCode;
	}

}
