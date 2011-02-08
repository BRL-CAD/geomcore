package org.brlcad.geometryservice.net;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class ByteBufferWriter {

	private final ByteBuffer bb;
	private final boolean endianFlip;

	public ByteBufferWriter(ByteBuffer bb) {
		this(bb, false);
	}

	public ByteBufferWriter(ByteBuffer bb, boolean endianFlip) {
		super();
		this.bb = bb;
		this.endianFlip = endianFlip;
	}

	/*
	 * Putters
	 */

	public void put(byte x) {
		this.bb.put(x);
	}

	public void putChar(char x) {
		if (this.endianFlip)
			x = Character.reverseBytes(x);
		this.bb.putChar(x);
	}

	public void putShort(short x) {
		if (this.endianFlip)
			x = Short.reverseBytes(x);
		this.bb.putShort(x);
	}

	public void putInt(int x) {
		if (this.endianFlip)
			x = Integer.reverseBytes(x);
		this.bb.putInt(x);
	}

	public void putLong(long x) {
		if (this.endianFlip)
			x = Long.reverseBytes(x);
		this.bb.putLong(x);
	}

	public void putFloat(float x) {
		if (this.endianFlip) {
			// TODO verify this?
			bb.order(ByteOrder.LITTLE_ENDIAN).putFloat(x);
			bb.order(ByteOrder.BIG_ENDIAN);
		} else {
			this.bb.putFloat(x);
		}
	}

	public void putDouble(double x) {
		if (this.endianFlip) {
			bb.order(ByteOrder.LITTLE_ENDIAN).putDouble(x);
			bb.order(ByteOrder.BIG_ENDIAN);
		} else {
			this.bb.putDouble(x);
		}
	}

	public void putString(String x) {
		if (this.endianFlip) {
			ByteBufferUtils.putString(this.bb, x, true);
		} else {
			ByteBufferUtils.putString(this.bb, x, false);
		}
		return;
	}

	public void putWriter(ByteBufferWriter writer) {
		ByteBuffer bbin = writer.getBb();
		bbin.flip();
		this.bb.put(bbin);
	}

	public void putBB(ByteBuffer bb) {
		bb.flip();
		this.bb.put(bb);

	}

	public synchronized void putIntAt(int value, int position) {
		// mark end position
		int endPosition = this.position();

		// go back to the desired position
		this.bb.position(position);

		// Write in the value:
		this.putInt(value);

		// now go back to end:
		this.bb.position(endPosition);
	}

	/*
	 * Getters
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

	public ByteBuffer getBb() {
		return this.bb;
	}

	protected boolean isEndianFlip() {
		return this.endianFlip;
	}
}
