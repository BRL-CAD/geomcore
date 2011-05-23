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
package org.brlcad.geometryservice.minimalclient;

import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * @author david.h.loman
 *
 */
public class RepositoryViewerPanel extends JPanel {

	private static final long serialVersionUID = -4138207212648943638L;
	private JTree jtree;
	private JScrollPane treeView;

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

		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Tops!!");

		jtree = new JTree(top);
		jtree.setRootVisible(true);
		//jtree.setPreferredSize(new Dimension(300, 560));

		this.treeView = new JScrollPane(jtree);
		treeView.setBorder(myBorder);
		this.add(this.treeView);


	 /* */

		top.add(new DefaultMutableTreeNode("one"));
		top.add(new DefaultMutableTreeNode("two"));
		top.add(new DefaultMutableTreeNode("three"));
		top.add(new DefaultMutableTreeNode("four"));

	}

}
