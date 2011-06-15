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
/** @file NetMsgChangeTracker.java
 * 
 */
package org.brlcad.geometryservice.net.msg;

import java.util.ArrayList;

/**
 * Reusable class that groups a NetMsg and a change value together to aid in
 * tracking the last change made to the NetMsg. Has built in pooling.
 */
public class NetMsgChangeTracker {

	/*
	 * Class impl
	 */
	public static final int DEFAULT_CHANGE_VALUE = 0;

	private AbstractNetMsg msg;
	private int changeValue;
	private String changeString;
	
	private NetMsgChangeTracker() {
		this.msg = null;
		this.changeValue = NetMsgChangeTracker.DEFAULT_CHANGE_VALUE;
		this.changeString = "";
	}

	/**
	 * @return the changeString
	 */
	public final String getChangeString() {
		return changeString;
	}

	/**
	 * @param changeString
	 *            the changeString to set
	 */
	public final void setChangeString(String changeString) {
		this.changeString = changeString;
	}

	/**
	 * @param msg
	 *            the msg to set
	 */
	public final void setMsg(AbstractNetMsg msg) {
		this.msg = msg;
	}

	/**
	 * @return the msg
	 */
	public final AbstractNetMsg getMsg() {
		return msg;
	}

	/**
	 * @return the changeValue
	 */
	public final int getChangeValue() {
		return changeValue;
	}

	/**
	 * @param changeValue
	 *            the changeValue to set
	 */
	public final void setChangeValue(int changeValue) {
		this.changeValue = changeValue;
	}


	public final void clear() {
		this.changeValue = NetMsgChangeTracker.DEFAULT_CHANGE_VALUE;
		this.msg = null;
	}

	/*
	 * Pooling/factory impl
	 */

	private static final int DEFAULT_INITIAL_POOL_SIZE = 100;
	private static ArrayList<NetMsgChangeTracker> pool = new ArrayList<NetMsgChangeTracker>(
			DEFAULT_INITIAL_POOL_SIZE);

	public static final NetMsgChangeTracker getChangeTracker() {
		
		NetMsgChangeTracker tracker = null;
		int size = pool.size();
		if (size <= 0)
			tracker = new NetMsgChangeTracker();
		else 
			// pop from end for max performance
			tracker = pool.get(size - 1);	
	
		return tracker;
	}

	public static final void putChangeTracker(NetMsgChangeTracker tracker) {
		tracker.clear();
		pool.add(tracker);
	}
		
	public static final int fillPool() {
		int size = pool.size();
		if (size >= DEFAULT_INITIAL_POOL_SIZE)
			return size;
		
		for (int i = size; i < DEFAULT_INITIAL_POOL_SIZE ; ++i) {
			putChangeTracker(new NetMsgChangeTracker());
		}
		return pool.size();
	}
}
