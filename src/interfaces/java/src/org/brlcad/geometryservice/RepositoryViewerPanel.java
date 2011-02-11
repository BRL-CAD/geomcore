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
/** @file CmdConsolePanel.java
 * 
 */
package org.brlcad.geometryservice;

import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

/**
 * @author david.h.loman
 * 
 */
public class RepositoryViewerPanel extends JPanel {

	private static final long serialVersionUID = -4138207212648943638L;
	private JTextArea treeview;

	/**
	 * 
	 */
	public RepositoryViewerPanel(ActionListener actListener) {
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 */
	public RepositoryViewerPanel(ActionListener actListener, LayoutManager arg0) {
		super(arg0);
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 */
	public RepositoryViewerPanel(ActionListener actListener, boolean arg0) {
		super(arg0);
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 * @param arg1
	 */
	public RepositoryViewerPanel(ActionListener actListener, LayoutManager arg0, boolean arg1) {
		super(arg0, arg1);
		this.commonCstr(actListener);
	}

	private final void commonCstr(ActionListener actListener) {
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		this.setPreferredSize(new Dimension(300, 560));
		
		Border myBorder = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);

		/*  */
		treeview = new JTextArea();
		treeview.setLineWrap(true);
		treeview.setWrapStyleWord(true);
		treeview.setEditable(false);
		treeview.setBorder(myBorder);
		this.add(treeview);
	}

}
