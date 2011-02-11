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
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

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
		this.add(console);

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
			String val = this.cmdLine.getText();
			String text = this.console.getText();
			text += val + "\n";
			this.console.setText(text);
			this.cmdLine.setText(this.prompt);
		}
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
}
