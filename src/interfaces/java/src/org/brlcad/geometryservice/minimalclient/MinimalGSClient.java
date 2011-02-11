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
/** @file MinimalGSClient.java
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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

/**
 * @author david.h.loman
 * 
 */
public class MinimalGSClient extends JFrame implements ActionListener {

	private static final long serialVersionUID = -839046331894878664L;

	
	private JTextArea area01 = new JTextArea();
	private JTextArea area02 = new JTextArea();
	private JTextField area03 = new JTextField();
	
	/**
	 * @throws HeadlessException
	 */
	public MinimalGSClient(String labelText) throws HeadlessException {
		this.commonCstr(labelText);
	}

	/**
	 * @param arg0
	 */
	public MinimalGSClient(String labelText, GraphicsConfiguration arg0) {
		super(arg0);
		this.commonCstr(labelText);
	}

	/**
	 * @param arg0
	 * @throws HeadlessException
	 */
	public MinimalGSClient(String labelText, String arg0) throws HeadlessException {
		super(arg0);
		this.commonCstr(labelText);
	}

	/**
	 * @param arg0
	 * @param arg1
	 */
	public MinimalGSClient(String labelText, String arg0, GraphicsConfiguration arg1) {
		super(arg0, arg1);
		this.commonCstr(labelText);
	}

	private void commonCstr(String labelText) {

		Border myBorder = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);

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
		JPanel leftPanel = new JPanel();
		masterPanel.add(leftPanel);

		leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.PAGE_AXIS));
		leftPanel.setPreferredSize(new Dimension(300, 560));

		/*  */
		JPanel spacerPanel01 = new JPanel();
		masterPanel.add(spacerPanel01);

		spacerPanel01.setPreferredSize(new Dimension(20, 560));

		/*  */
		JPanel rightPanel = new JPanel();
		masterPanel.add(rightPanel);

		rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.PAGE_AXIS));

		/*  */
		area01.setLineWrap(true);
		area01.setWrapStyleWord(true);
		area01.setEditable(false);
		area01.setBorder(myBorder);
		leftPanel.add(area01);

		/*  */
		area02.setLineWrap(true);
		area02.setWrapStyleWord(true);
		area02.setEditable(false);
		area02.setBorder(myBorder);
		rightPanel.add(area02);

		/*  */
		area03.setEditable(true);
		area03.setBorder(myBorder);
		area03.setMinimumSize(new Dimension(1, 25));
		area03.setPreferredSize(new Dimension(300, 25));
		area03.setMaximumSize(new Dimension(1024^2, 25));
		area03.addActionListener(this);
		rightPanel.add(area03);

	}


	@Override
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();

		if (source == this.area03)
		{
			String val = this.area03.getText();
			
			JOptionPane.showMessageDialog(null, val, "Message Dialog", JOptionPane.PLAIN_MESSAGE);
			setVisible(true); // show something
		}
	}

}
