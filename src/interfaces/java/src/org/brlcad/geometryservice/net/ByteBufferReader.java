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
/** @file ByteBufferReader.java
 *
 */
package org.brlcad.geometryservice.net;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.UUID;

public class ByteBufferReader {

	private final ByteBuffer bb;
	private final boolean endianFlip;

	/**
	 * Helper class for getting structured information out of a ByteBuffer
	 * easily. ByteBuffer passed in is copied to an internal ByteBuffer.
	 * <B>bbin</B> must have the position attribute at the <i>end</i> of the
	 * data to be copied over, since <b>bbin.flip()</b> is called in this
	 * constructor.
	 *
	 * @param bbin
	 * @param endianFlip
	 */
	public ByteBufferReader(ByteBuffer bbin, boolean endianFlip) {
		super();

		// Copy supplied BB.
		this.bb = ByteBuffer.allocate(bbin.position());
		bbin.flip();
		this.bb.put(bbin);

		// prepare bb for reading
		this.bb.flip();

		this.endianFlip = endianFlip;
	}

	public ByteBufferReader(ByteBuffer bbin) {
		this(bbin, false);
	}

	/*
	 * Getters
	 */

	/**
	 * @return
	 * @see java.nio.ByteBuffer#get()
	 */
	public byte get() {
		return bb.get();
	}

	public void get(byte[] ba) {
		this.bb.get(ba);
		return;
	}

	public boolean getBoolean() {
		return (this.bb.get() > 0);
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getChar()
	 */
	public char getChar() {
		char x = bb.getChar();
		if (this.endianFlip)
			x = Character.reverseBytes(x);
		return x;
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getDouble()
	 */
	public double getDouble() {
		double x = 0;
		if (this.endianFlip) {
			x = bb.order(ByteOrder.LITTLE_ENDIAN).getFloat();
			bb.order(ByteOrder.BIG_ENDIAN);
		} else {
			x = bb.getDouble();
		}
		return x;
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getFloat()
	 */
	public float getFloat() {
		float x = 0;
		if (this.endianFlip) {
			x = bb.order(ByteOrder.LITTLE_ENDIAN).getFloat();
			bb.order(ByteOrder.BIG_ENDIAN);
		} else {
			x = bb.getFloat();
		}
		return x;
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getInt()
	 */
	public int getInt() {
		int x = bb.getInt();
		if (this.endianFlip)
			x = Integer.reverseBytes(x);
		return x;
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getLong()
	 */
	public long getLong() {
		long x = bb.getLong();
		if (this.endianFlip)
			x = Long.reverseBytes(x);
		return x;
	}

	/**
	 * @return
	 * @see java.nio.ByteBuffer#getShort()
	 */
	public short getShort() {
		short x = bb.getShort();
		if (this.endianFlip)
			x = Short.reverseBytes(x);
		return x;
	}

	public final String getString() {
		if (this.endianFlip)
			return ByteBufferUtils.get8BitCharString(this.bb, true);
		else
			return ByteBufferUtils.get8BitCharString(this.bb);

	}

	public final UUID getUUID() {
		String str = this.getString();

		if (str.length() != 36) {
			str = str.substring(0, 36);
		}

		UUID id = UUID.fromString(str);
		return id;
	}

	/*
	 * ByteBuffer delegates
	 */

	/**
	 * @return
	 * @see java.nio.Buffer#hasRemaining()
	 */
	public final boolean hasRemaining() {
		return bb.hasRemaining();
	}

	/**
	 * @return
	 * @see java.nio.Buffer#limit()
	 */
	public final int limit() {
		return bb.limit();
	}

	/**
	 * @return
	 * @see java.nio.Buffer#position()
	 */
	public final int position() {
		return bb.position();
	}

	/**
	 * @return
	 * @see java.nio.Buffer#remaining()
	 */
	public final int remaining() {
		return bb.remaining();
	}

	/*
	 * Status getters
	 */

	protected ByteBuffer getBb() {
		return bb;
	}

	protected boolean isEndianFlip() {
		return endianFlip;
	}

}
