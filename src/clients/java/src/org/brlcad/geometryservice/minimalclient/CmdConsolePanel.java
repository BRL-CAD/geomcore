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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.brlcad.geometryservice.GSJavaInterface;
import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.minimalclient.cmd.CmdManager;

/**
 * @author david.h.loman
 *
 */
public class CmdConsolePanel extends JPanel implements ActionListener {

	private static final long serialVersionUID = -4138207212648943638L;
	private JTextPane console;
	private JTextFieldWithHistory cmdLine;
	private JLabel label;
	
	private static final String DEFAULT_PROMPT = "gsMinClient> ";
	private String prompt;

	private final MinimalGSClientGUI parentFrame;

	public CmdConsolePanel(MinimalGSClientGUI parentFrame) {
		this.commonCstr();
		this.parentFrame = parentFrame;
	}


	public CmdConsolePanel(MinimalGSClientGUI parentFrame, LayoutManager arg0) {
		super(arg0);
		this.commonCstr();
		this.parentFrame = parentFrame;
	}


	public CmdConsolePanel(MinimalGSClientGUI parentFrame, boolean arg0) {
		super(arg0);
		this.commonCstr();
		this.parentFrame = parentFrame;
	}

	public CmdConsolePanel(MinimalGSClientGUI parentFrame, LayoutManager arg0, boolean arg1) {
		super(arg0, arg1);
		this.commonCstr();
		this.parentFrame = parentFrame;
	}

	private final void commonCstr() {
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		Border myBorder = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);

		/*  */
		this.console = new JTextPane();
		this.console.setEditable(false);
		this.console.setBorder(myBorder);
		StyledDocument doc = this.console.getStyledDocument();
		this.addStylesToDocument(doc);

		JScrollPane scroll = new JScrollPane(console);
		scroll.setPreferredSize(new Dimension(100,1000));
		this.add(scroll);
		
		/*  */
		this.setDefaultPrompt();
		this.label = new JLabel(this.prompt);


		/*  */
		this.cmdLine = new JTextFieldWithHistory();
		this.cmdLine.setEditable(true);
		this.cmdLine.setBorder(myBorder);
		this.cmdLine.setMinimumSize(new Dimension(1, 25));
		this.cmdLine.setPreferredSize(new Dimension(305, 25));
		this.cmdLine.setMaximumSize(new Dimension(1024 ^ 2, 25));
		this.cmdLine.addActionListener(this);

			
		/*  */
		JPanel cmdLinePanel = new JPanel();
		cmdLinePanel.add(label);
		cmdLinePanel.add(cmdLine);
		
		this.add(cmdLinePanel);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		String act = event.getActionCommand();		

		if (source == this.cmdLine) {
			String cmd = this.cmdLine.getText();
			GSJavaInterface gsji = this.parentFrame.getClient().getGSJavaInterface();
			CmdManager.parseCmd(cmd, this, gsji);
			this.label.setText(this.prompt);

			this.cmdLine.setText(cmd);
			this.cmdLine.clear();
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

	public final void printToConsole(String line, String style) {
		try {
		StyledDocument doc = this.console.getStyledDocument();
		int len = doc.getLength();
			doc.insertString(len, line, doc.getStyle(style));
		} catch (BadLocationException e) {
			GSStatics.stdErr.println(e.getMessage());
		}
	}

	public final void printLnToConsole(String line, String style) {
		this.printToConsole(line + "\n", style);
	}


    private void addStylesToDocument(StyledDocument doc) {
        //Initialize some styles.
        Style def = StyleContext.getDefaultStyleContext().
                        getStyle(StyleContext.DEFAULT_STYLE);
        StyleConstants.setFontFamily(def, "SansSerif");


        Style s = doc.addStyle(STYLE_RED, def);
        StyleConstants.setForeground(s, new Color(0.7f, 0.0f, 0.0f));
        CmdConsolePanel.addSubStyles(s, doc, "red");

        s = doc.addStyle(STYLE_GREEN, def);
        StyleConstants.setForeground(s, new Color(0.0f, 0.7f, 0.0f));
        CmdConsolePanel.addSubStyles(s, doc, "greed");

        s = doc.addStyle(STYLE_BLUE, def);
        StyleConstants.setForeground(s, new Color(0.0f, 0.0f, 0.7f));
        CmdConsolePanel.addSubStyles(s, doc, "blue");
}

    private static void addSubStyles(Style main, StyledDocument doc, String subname) {
        Style s = doc.addStyle(subname + STYLE_SUB_ITALIC, main);
        StyleConstants.setItalic(s, true);

        s = doc.addStyle(subname + STYLE_SUB_BOLD, main);
        StyleConstants.setBold(s, true);

        s = doc.addStyle(subname + STYLE_SUB_UNDERLINE, main);
        StyleConstants.setUnderline(s, true);

        s = doc.addStyle(subname + STYLE_SUB_SMALL, main);
        StyleConstants.setFontSize(s, 10);

        s = doc.addStyle(subname + STYLE_SUB_LARGE, main);
        StyleConstants.setFontSize(s, 16);
    }

    /*
     * Style statics.
     */

    private final static String STYLE_SUB_ITALIC = "-italic";
    private final static String STYLE_SUB_BOLD = "-bold";
    private final static String STYLE_SUB_UNDERLINE = "-underline";
    private final static String STYLE_SUB_SMALL = "-small";
    private final static String STYLE_SUB_LARGE = "-large";

    public final static String STYLE_RED = "red";
    public final static String STYLE_GREEN = "green";
    public final static String STYLE_BLUE = "blue";

    public final static String STYLE_RED_ITALIC = STYLE_RED + STYLE_SUB_ITALIC;
    public final static String STYLE_RED_BOLD = STYLE_RED + STYLE_SUB_BOLD;
    public final static String STYLE_RED_UNDERLINE = STYLE_RED + STYLE_SUB_UNDERLINE;
    public final static String STYLE_RED_SMALL = STYLE_RED + STYLE_SUB_SMALL;
    public final static String STYLE_RED_LARGE = STYLE_RED + STYLE_SUB_LARGE;

    public final static String STYLE_GREEN_ITALIC = STYLE_GREEN + STYLE_SUB_ITALIC;
    public final static String STYLE_GREEN_BOLD = STYLE_GREEN + STYLE_SUB_BOLD;
    public final static String STYLE_GREEN_UNDERLINE = STYLE_GREEN + STYLE_SUB_UNDERLINE;
    public final static String STYLE_GREEN_SMALL = STYLE_GREEN + STYLE_SUB_SMALL;
    public final static String STYLE_GREEN_LARGE = STYLE_GREEN + STYLE_SUB_LARGE;

    public final static String STYLE_BLUE_ITALIC = STYLE_BLUE + STYLE_SUB_ITALIC;
    public final static String STYLE_BLUE_BOLD = STYLE_BLUE + STYLE_SUB_BOLD;
    public final static String STYLE_BLUE_UNDERLINE = STYLE_BLUE + STYLE_SUB_UNDERLINE;
    public final static String STYLE_BLUE_SMALL = STYLE_BLUE + STYLE_SUB_SMALL;
    public final static String STYLE_BLUE_LARGE = STYLE_BLUE + STYLE_SUB_LARGE;
    

    
}
