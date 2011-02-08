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
