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
/** @file ByteBufferUtils.java
 *
 */
package org.brlcad.geometryservice.net;

import java.io.UnsupportedEncodingException;
import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;

public class ByteBufferUtils {
	public static String getString(ByteBuffer bb) throws BufferUnderflowException {
		return getString(bb, false);
	}

	public static String getString(ByteBuffer bb, boolean switchEndian) throws BufferUnderflowException {
		String out = "";
		synchronized (bb) {

			int stringLen = bb.getInt();

			if (switchEndian)
				stringLen = ((Integer.reverseBytes(stringLen)) * 2);
			else
				stringLen *= 2;

			byte[] b = new byte[stringLen];
			for (int i = 0; i < stringLen; i += 2) {
				if (switchEndian) {
					b[i + 1] = bb.get();
					b[i] = bb.get();
				} else {
					b[i] = bb.get();
					b[i + 1] = bb.get();
				}
			}
			try {
				out = new String(b, "UTF-16BE");
			} catch (UnsupportedEncodingException e) {
				// TODO do something here for this error!
			}
		}
		return out;
	}

	public static void putString(ByteBuffer bb, String data) throws BufferOverflowException {
		putString(bb, data, false);
	}

	public static void putString(ByteBuffer bb, String data, boolean switchEndian) throws BufferOverflowException {
		if (data == null) {
			data = new String("");
		}
		char[] chars = data.toCharArray();

		synchronized (bb) {
			// Write length
			if (switchEndian) {
				bb.putInt(Integer.reverseBytes(chars.length));
			} else {
				bb.putInt(chars.length);
			}
			// Write chars
			for (char c : chars) {
				if (switchEndian) {
					bb.putChar(Character.reverseBytes(c));
				} else {
					bb.putChar(c);
				}
			}
		}
	}

}
