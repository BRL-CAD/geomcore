package org.brlcad.geometryservice.minimalclient;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.text.Document;

public class JTextFieldWithHistory extends JTextField {
	private static final long serialVersionUID = 2533810103656863448L;
	
	private ArrayList<String> cmds = new ArrayList<String>(100);
	private int index = 0;

	public JTextFieldWithHistory() {
		this.bindKeys();
	}

	public JTextFieldWithHistory(String arg0) {
		super(arg0);
		this.bindKeys();
	}

	public JTextFieldWithHistory(int arg0) {
		super(arg0);
		this.bindKeys();
	}

	public JTextFieldWithHistory(String arg0, int arg1) {
		super(arg0, arg1);
		this.bindKeys();
	}

	public JTextFieldWithHistory(Document arg0, String arg1, int arg2) {
		super(arg0, arg1, arg2);
		this.bindKeys();
	}

	private void bindKeys() {
		UpArrowAction up = new UpArrowAction("UpArrowAction01", this);
		up.mapThisActionToComponent(this);

		DownArrowAction down = new DownArrowAction("DownArrowAction01", this);
		down.mapThisActionToComponent(this);
	}

	public String getHistoryCmdIncr() {
		++this.index;
		return this.getCmdAtIndex();
	}

	public String getHistoryCmdDecr() {
		--this.index;
		return this.getCmdAtIndex();
	}

	private String getCmdAtIndex() {
		int size = this.cmds.size();

		if (size == 0) /* Early Bailout */
			return "";

		if (this.index >= size)
			this.index = 0;

		if (this.index < 0)
			this.index = size - 1;

		return this.cmds.get(this.index);
	}

	public void resetHistoryIndexer() {
		this.index = this.cmds.size() - 1;
	}

	@Override
	public void setText(String t) {
		this.setText(t, true);
	}

	public void setText(String t, boolean includeInHistory) {
		if (t.length() < 1) /* Bail early */
			return;

		/* TODO Validate text here */

		/* Add to history */
		if (includeInHistory) {
			this.cmds.add(t);
			this.resetHistoryIndexer();
		}

		super.setText(t);
	}

	public void clear() {
		super.setText(""); /* this.setText() checks for zero len strings */
	}

	/**
	 * AbstractAction subclass designed to capture and handle the 'up arrow' key
	 * event. Will scroll through the command history and set the TextField to
	 * the appropriate historical command
	 */
	private static class UpArrowAction extends AbstractAction {
		private static final long serialVersionUID = -5442804822938868414L;
		public static final int keyID = KeyEvent.VK_UP;
		public static final String actionStr = "doUPArrowKeyAction";

		public static final KeyStroke getKeyStroke() {
			return KeyStroke.getKeyStroke(UpArrowAction.keyID, 0);
		}

		private JTextFieldWithHistory tf;

		public UpArrowAction(String name, JTextFieldWithHistory tf) {
			super(name);
			this.tf = tf;
			putValue(SHORT_DESCRIPTION, "UP ARROW PRESSED");
			putValue(MNEMONIC_KEY, UpArrowAction.keyID);
		}

		public void mapThisActionToComponent(JComponent c) {
			c.getInputMap().put(UpArrowAction.getKeyStroke(),
					UpArrowAction.actionStr);
			c.getActionMap().put(UpArrowAction.actionStr, this);
		}

		public void actionPerformed(ActionEvent e) {
			this.tf.setText(this.tf.getHistoryCmdDecr(), false);
		}
	}

	/**
	 * AbstractAction subclass designed to capture and handle the 'down arrow'
	 * key event. Will scroll through the command history and set the TextField
	 * to the appropriate historical command
	 */

	private static class DownArrowAction extends AbstractAction {
		private static final long serialVersionUID = -23590062720972088L;
		public static final int keyID = KeyEvent.VK_DOWN;
		public static final String actionStr = "doDOWNArrowKeyAction";

		public static final KeyStroke getKeyStroke() {
			return KeyStroke.getKeyStroke(DownArrowAction.keyID, 0);
		}

		private JTextFieldWithHistory tf;

		public DownArrowAction(String name, JTextFieldWithHistory tf) {
			super(name);
			this.tf = tf;
			putValue(SHORT_DESCRIPTION, "DOWN ARROW PRESSED");
			putValue(MNEMONIC_KEY, DownArrowAction.keyID);
		}

		public void mapThisActionToComponent(JComponent c) {
			c.getInputMap().put(DownArrowAction.getKeyStroke(),
					DownArrowAction.actionStr);
			c.getActionMap().put(DownArrowAction.actionStr, this);
		}

		public void actionPerformed(ActionEvent e) {
			this.tf.setText(this.tf.getHistoryCmdIncr(), false);
		}
	}

}
