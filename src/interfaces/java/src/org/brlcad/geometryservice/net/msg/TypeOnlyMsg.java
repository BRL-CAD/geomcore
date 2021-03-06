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

package org.brlcad.geometryservice.net.msg;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class TypeOnlyMsg extends AbstractNetMsg {

	public TypeOnlyMsg(short msgType) {
		super(msgType);
	}

	public TypeOnlyMsg(short msgType, AbstractNetMsg reMsg) {
		super(msgType, reMsg);
	}

	public TypeOnlyMsg(short msgType, ByteBufferReader reader) {
		super(msgType, reader);
	}

	@Override
	protected void _deserialize(ByteBufferReader reader) {
	}

	@Override
	protected void _serialize(ByteBufferWriter writer) {
	}

}
