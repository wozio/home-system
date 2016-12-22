import com.sun.lwuit.*;
import com.sun.lwuit.events.*;
import com.sun.lwuit.layouts.*;

public class SettingsForm extends Form implements ActionListener {
  private HomeSystemController parent = null;

  private static final int BACK_COMMAND = 1;
  private static final int OK_COMMAND = 2;

  private final Command backCommand = new Command("Back", BACK_COMMAND);
  private final Command okCommand = new Command("OK", OK_COMMAND);

  private final TextField hostField = new TextField("");
  private final Label hostLabel = new Label("Server address:");

  SettingsForm(HomeSystemController parent) {
    this.parent = parent;

    setTitle("Settings");
    setLayout(new BoxLayout(BoxLayout.Y_AXIS));

    addComponent(hostLabel);
    addComponent(hostField);

    addCommand(backCommand);
    addCommand(okCommand);

    addCommandListener(this);
  }

  public void actionPerformed(ActionEvent ae) {
    Command cmd = ae.getCommand();
    switch (cmd.getId()) {
      case BACK_COMMAND:
        parent.setMainForm();
        break;
      case OK_COMMAND:
        Settings.set("host", hostField.getText());
        parent.setMainForm();
        break;
    }
  }

  protected void onShow() {
    String host = Settings.get("host", null);
    if (host.equals("")) {
      host = "localhost";
    }
    hostField.setText(host);
  }
}
