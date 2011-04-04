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
/** @file SerialTest.java
 * 
 */
package org.brlcad.geometryservice;

import java.nio.ByteBuffer;

import org.brlcad.geometryservice.net.ByteBufferWriter;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;

/**
 * @author dloman
 *
 */
public class SerialTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		NewSessionReqMsg msg = new NewSessionReqMsg("Guest", "Guest");
		
		ByteBuffer bb = ByteBuffer.allocate(1024*1024);
		ByteBufferWriter wr = new ByteBufferWriter(bb);
		
		msg.serialize(wr);
		
		String out = "";
		byte[] ba = bb.array();
		for (int i = 0 ; i < wr.position(); ++i) {
			byte b = ba[i];
			String c = Integer.toString(b & 0xff, 16).toUpperCase();
			if (c.length() == 1)
				c = "0" + c;
			
			out += c;
		}
		
		System.out.println("Data: " + out);

		
	}

}
