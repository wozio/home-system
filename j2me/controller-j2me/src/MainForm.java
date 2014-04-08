import com.sun.lwuit.*;
import com.sun.lwuit.events.*;
import com.sun.lwuit.layouts.*;
import com.sun.me.web.request.*;
import com.sun.me.web.path.Result;
import com.sun.me.web.path.ResultException;
import javax.microedition.io.HttpConnection;

public class MainForm extends Form implements ActionListener, RequestListener {
  private HomeSystemController parent = null;

  private static final int EXIT_COMMAND = 1;
  private static final int CONNECT_COMMAND = 2;
  private static final int SETTINGS_COMMAND = 3;

  private final Command exitCommand = new Command("Exit", EXIT_COMMAND);
  private final Command connectCommand = new Command("connect", CONNECT_COMMAND);
  private final Command settingsCommand = new Command("Settings", SETTINGS_COMMAND);

  private final Label label = new Label("");
  private final Button button = new Button(connectCommand);

  MainForm(HomeSystemController parent) {
    this.parent = parent;

    setTitle("Home System");
    setLayout(new BoxLayout(BoxLayout.Y_AXIS));

    addComponent(button);
    addComponent(label);

    addCommand(exitCommand);
    addCommand(settingsCommand);

    addCommandListener(this);
  }

  public void actionPerformed(ActionEvent ae) {
    Command cmd = ae.getCommand();
    switch (cmd.getId()) {
      case EXIT_COMMAND:
        parent.exit();
        break;
      case CONNECT_COMMAND:
        label.setText("connecting...");
        try {
          Request.get("http://" + Settings.get("host", null) + ":9999/", null, null, this, new Integer(1));
          
        } catch (Exception e) {
          System.out.println("Error: " + e.getMessage());
          //label.setText("Error: " + e.getMessage());
        }
        repaint();
        break;
      case SETTINGS_COMMAND:
        parent.setSettingsForm();
        break;
    }
  }

  public void readProgress(final Object context, final int bytes, final int total) {
  }

  public void writeProgress(final Object context, final int bytes, final int total) {
  }

  public void done(final Object context, final Response response) {
    try {
      final Result r = validateResponse(response);
      if (r == null) {
        label.setText("error");
        return;
      }
      label.setText(r.getAsString("key1"));
    } catch (Exception e) {
      label.setText("error");
      System.err.println(e.getMessage());
      e.printStackTrace();
    }
  }

  private Result validateResponse(final Response response) {

        final int responseCode = response.getCode();
        if (responseCode == HttpConnection.HTTP_NOT_MODIFIED) {
            return null;
        }

        if (responseCode != HttpConnection.HTTP_OK) {
            System.out.println("Error: HTTP response code " + responseCode);
            return null;
        }

        final Exception rex = response.getException();
        if (rex != null) {
            System.out.println("Error: " + rex.getMessage());
            return null;
        }

        final Result result = response.getResult();

        return result;
    }
}
