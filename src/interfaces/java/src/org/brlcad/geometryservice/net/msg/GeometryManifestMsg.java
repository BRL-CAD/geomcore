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

import java.util.ArrayList;

import org.brlcad.geometryservice.net.ByteBufferReader;
import org.brlcad.geometryservice.net.ByteBufferWriter;

public class GeometryManifestMsg extends AbstractNetMsg {

	private ArrayList<String> items;

	/**
	 * @param reader
	 */
	public GeometryManifestMsg(ByteBufferReader reader) {
		super(NetMsgTypes.GeometryMANIFEST, reader);
	}

	/**
	 * @param msgType
	 * @param reMsg
	 */
	public GeometryManifestMsg(AbstractNetMsg reMsg,  ArrayList<String> items) {
		super(NetMsgTypes.GeometryMANIFEST, reMsg);
		this.items = items;
	}

	/**
	 * @param msgType
	 */
	public GeometryManifestMsg( ArrayList<String> items) {
		super(NetMsgTypes.GeometryMANIFEST);
		this.items = items;
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_deserialize(org.brlcad.geometryservice.net.ByteBufferReader)
	 */
	@Override
	protected void _deserialize(ByteBufferReader reader) {
		int size = reader.getInt();
		this.items = new ArrayList<String>(size);
		String s = "";
		
		for (int i = 0 ;i<size;++i) {
			s = reader.getString();
			if (s.length() > 0) 
				this.items.add(s);
		}
	}

	/**
	 * @see org.brlcad.geometryservice.net.msg.AbstractNetMsg#_serialize(org.brlcad.geometryservice.net.ByteBufferWriter)
	 */
	@Override
	protected void _serialize(ByteBufferWriter writer) {
		int size = this.items.size();
		int sizePos = 0;
		int actualCount = 0;
		String s = "";
		
		sizePos = writer.position();
		writer.putInt(size);

		if (size > 0){
			for (int i = 0; i < size; ++i) {
				s = this.items.get(i);
				if (s.length() > 0) {
					writer.putString(s);
					++actualCount;
				}
			}
		
			if (size != actualCount) 
				writer.putIntAt(actualCount, sizePos);
			
		}
	}

	public ArrayList<String> getItems() {
		return items;
	}
	

}
