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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

import org.brlcad.geometryservice.minimalclient.cmd.CmdManager;

/**
 * @author david.h.loman
 * 
 */
public class CmdConsolePanel extends JPanel implements ActionListener {

	private static final long serialVersionUID = -4138207212648943638L;
	private JTextArea console;
	private JTextField cmdLine;

	private static final String DEFAULT_PROMPT = "gsMinClient> ";
	private String prompt;

	/**
	 * 
	 */
	public CmdConsolePanel(ActionListener actListener) {
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 */
	public CmdConsolePanel(ActionListener actListener, LayoutManager arg0) {
		super(arg0);
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 */
	public CmdConsolePanel(ActionListener actListener, boolean arg0) {
		super(arg0);
		this.commonCstr(actListener);
	}

	/**
	 * @param arg0
	 * @param arg1
	 */
	public CmdConsolePanel(ActionListener actListener, LayoutManager arg0, boolean arg1) {
		super(arg0, arg1);
		this.commonCstr(actListener);
	}

	private final void commonCstr(ActionListener actListener) {
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		Border myBorder = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
		
		/*  */
		console = new JTextArea();
		console.setLineWrap(true);
		console.setWrapStyleWord(true);
		console.setEditable(false);
		console.setBorder(myBorder);

		JScrollPane scroll = new JScrollPane(console);
		this.add(scroll);

		/*  */
		cmdLine = new JTextField();
		cmdLine.setEditable(true);
		cmdLine.setBorder(myBorder);
		cmdLine.setMinimumSize(new Dimension(1, 25));
		cmdLine.setPreferredSize(new Dimension(300, 25));
		cmdLine.setMaximumSize(new Dimension(1024 ^ 2, 25));
		cmdLine.addActionListener(this);
		this.add(cmdLine);

		this.setDefaultPrompt();
		this.cmdLine.setText(this.prompt);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();

		if (source == this.cmdLine) {
			String cmd = this.cmdLine.getText().replace(this.prompt, "");
			CmdManager.parseCmd(cmd, this);
			this.cmdLine.setText(this.prompt);
		}
	}

	public final void giveFocusToCmdLine() {
		this.cmdLine.grabFocus();
	}

	/**
	 * @return the prompt
	 */
	public final String getPrompt() {
		return prompt;
	}

	/**
	 * @param prompt
	 *            the prompt to set
	 */
	public final void setPrompt(String prompt) {
		this.prompt = prompt;
	}

	public final void setDefaultPrompt() {
		this.prompt = CmdConsolePanel.DEFAULT_PROMPT;
	}

	public final void addTextToConsole(String line) {
		String oldText = this.console.getText() + line + "\n";
		this.console.setText(oldText);
	}
}
