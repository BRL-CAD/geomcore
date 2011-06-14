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
/** @file MinimalGSClientGUI.java
 *
 */
package org.brlcad.geometryservice.minimalclient;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

import org.brlcad.geometryservice.minimalclient.cmd.CmdManager;

/**
 * @author david.h.loman
 *
 */
public class MinimalGSClientGUI extends JFrame implements ActionListener {

	private static final long serialVersionUID = -839046331894878664L;

	private final MinimalGSClient client;

	/**
	 * @throws HeadlessException
	 */
	public MinimalGSClientGUI( MinimalGSClient client) throws HeadlessException {
		this.commonCstr();
		this.client = client;
	}

	/**
	 * @param arg0
	 */
	public MinimalGSClientGUI( MinimalGSClient client, GraphicsConfiguration arg0) {
		super(arg0);
		this.commonCstr();
		this.client = client;
	}

	/**
	 * @param arg0
	 * @throws HeadlessException
	 */
	public MinimalGSClientGUI( MinimalGSClient client, String arg0) throws HeadlessException {
		super(arg0);
		this.commonCstr();
		this.client = client;
	}

	/**
	 * @param arg0
	 * @param arg1
	 */
	public MinimalGSClientGUI( MinimalGSClient client, String arg0, GraphicsConfiguration arg1) {
		super(arg0, arg1);
		this.commonCstr();
		this.client = client;
	}

	private void commonCstr() {
		this.setSize(new Dimension(800, 600));
		this.setTitle("GeometryService Minimal Client");
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.setLocationRelativeTo(null);
		this.setResizable(false);

		Container contentPanel = this.getContentPane();
		contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.LINE_AXIS));

		JPanel masterPanel = new JPanel();
		masterPanel.setLayout(new BoxLayout(masterPanel, BoxLayout.LINE_AXIS));
		masterPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPanel.add(masterPanel);

		/*  */
		RepositoryViewerPanel repoPanel = new RepositoryViewerPanel(this);
		masterPanel.add(repoPanel);

		/*  */
		JPanel spacerPanel01 = new JPanel();
		spacerPanel01.setPreferredSize(new Dimension(20, 560));
		masterPanel.add(spacerPanel01);

		/*  */
		CmdConsolePanel consolePanel = new CmdConsolePanel(this);
		masterPanel.add(consolePanel);

		consolePanel.giveFocusToCmdLine();

		CmdManager.registerBuiltInCmds(consolePanel);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub

	}

	/**
	 * @return the client
	 */
	public final MinimalGSClient getClient() {
		return client;
	}


}
