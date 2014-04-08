import javax.microedition.midlet.MIDlet;
import com.sun.lwuit.Display;
import com.sun.lwuit.util.Resources;
import com.sun.lwuit.plaf.UIManager;

public class HomeSystemController extends MIDlet {

  public void startApp() {
    //homeSystemController = this;
    try {
      Display.init(this);
      Resources r = Resources.open("/theme.res");
      UIManager.getInstance().setThemeProps(r.getTheme("default"));

      Display.getInstance().callSerially(new Runnable() {
        public void run() {
          if (Settings.get("host", null).equals("")) {
            setSettingsForm();
          }
          setMainForm();
        }
      });
    } catch (Throwable ex) {
      System.out.println(ex.getMessage());
    }
  }

  public void exit() {
    destroyApp(true);
    notifyDestroyed();
  }

  public void setMainForm() {
    final MainForm f = new MainForm(this);
    f.show();
  }

  public void setSettingsForm() {
    final SettingsForm f = new SettingsForm(this);
    f.show();
  }

  public void pauseApp() {
  }

  public void destroyApp(boolean unconditional) {
  }
}
