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
/** @file GSNetMsgFutureResponse.java
 * 
 */
package org.brlcad.geometryservice.net;

import java.util.UUID;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.net.msg.AbstractNetMsg;

public class GSNetMsgFutureResponse {
	private final UUID originalMsgID;
	private AbstractNetMsg responseMsg = null;

	/**
	 * @param originalMsgID
	 */
	public GSNetMsgFutureResponse(UUID originalMsgID) {
		super();
		this.originalMsgID = originalMsgID;
	}

	public boolean hasResponse() {
		return (this.responseMsg != null);
	}

	/**
	 * Blocks until a response to a message arrives.
	 * 
	 * @return If null, then error occurred. Otherwise, the response Msg is
	 *         returned.
	 */
	public AbstractNetMsg blockUntilResponse() {
		return this.blockUntilResponse(-1);
	}

	/**
	 * Blocks until a response to a message arrives or timeout is reached. If
	 * timeOutInMS is <= 0, this call blocks indefinitely.
	 * 
	 * @return If null, then error occurred. Otherwise, the response Msg is
	 *         returned.
	 */
	public AbstractNetMsg blockUntilResponse(long timeOutInMS) {
		long stopT = System.currentTimeMillis() + timeOutInMS;
		while (this.responseMsg == null) {
			if (timeOutInMS > 0 && System.currentTimeMillis() >= stopT)
				break;

			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				GSStatics.stdErr.println("GSNetMsgFutureResponse::blockUntilResponse(): " + e.getClass().getSimpleName() + ":" + e.getMessage());
				return null;
			}
		}
		return this.getResponseMsg();
	}

	protected void setResponseMsg(AbstractNetMsg response) {
		this.responseMsg = response;
	}

	public UUID getOriginalMsgID() {
		return originalMsgID;
	}

	public AbstractNetMsg getResponseMsg() {
		return responseMsg;
	}
}
