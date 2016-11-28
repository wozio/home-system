import javax.microedition.rms.*;
import java.io.*;

public class Settings {
  public static String get(String key, Integer id) {
    String value = "";
    try {
      RecordStore rs = RecordStore.openRecordStore("Home System", true);
      RecordEnumeration re = rs.enumerateRecords(null, null, false);
      while (re.hasNextElement()) {
        int rId = re.nextRecordId ();
        // Do something about recordId in rs
        byte[] buf = rs.getRecord(rId);
        ByteArrayInputStream bis = new ByteArrayInputStream(buf);
        DataInputStream dbis = new DataInputStream(bis);

        String rk = dbis.readUTF();
        if (rk.equals(key)) {
          value = dbis.readUTF();
          id = new Integer(rId);
          re.destroy();
          return value;
        }
      }
      rs.closeRecordStore();
      re.destroy();
    } catch (Exception e) {}
    return value;
  }

  public static void set(String key, String value) {
    try {
      Integer id = null;
      if (get(key, id).equals(value)) {
        return;
      }
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dbos = new DataOutputStream(bos);
      dbos.writeUTF(key);
      dbos.writeUTF(value);
      RecordStore rs = RecordStore.openRecordStore("Home System", true);
      if (id == null) {
        rs.addRecord(bos.toByteArray(), 0, bos.size());
      }
      else {
        rs.setRecord(id.intValue(), bos.toByteArray(), 0, bos.size());
      }
      rs.closeRecordStore();
    } catch (Exception e) {}
  }
}
