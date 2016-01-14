package xelement;

import static org.junit.Assert.*;
import org.junit.Test;

import org.w3c.dom.Element;

import helper.XmlHelper;

import cx2x.xcodeml.xelement.Xid;

public class XidTest {
  private static final String TEST_TYPE = "F7f9502e03d00";
  private static final String TEST_SCLASS = "ffunc";
  private static final String TEST_NAME = "loop_extract";

  private static final String ALT_TEST_TYPE = "F7f81a04070d0";
  private static final String ALT_TEST_SCLASS = "auto";
  private static final String ALT_TEST_NAME = "func_name";

  private Xid createSimpleXid(){
    String simpleIdElement = "<id type=\"" + TEST_TYPE + "\" sclass=\"" +
      TEST_SCLASS + "\"><name>" + TEST_NAME + "</name></id>";
    return XmlHelper.createXidFromString(simpleIdElement);
  }

  @Test
  public void readElementInformationTest() {
    Xid simpleId = createSimpleXid();
    assertEquals(TEST_NAME, simpleId.getName());
    assertEquals(TEST_TYPE, simpleId.getType());
    assertEquals(TEST_SCLASS, simpleId.getSclass());
  }

  @Test
  public void setElementInformationTest() {
    Xid element = createSimpleXid();
    element.setName(ALT_TEST_NAME);
    element.setType(ALT_TEST_TYPE);
    element.setSclass(ALT_TEST_SCLASS);

    assertEquals(ALT_TEST_NAME, element.getName());
    assertEquals(ALT_TEST_TYPE, element.getType());
    assertEquals(ALT_TEST_SCLASS, element.getSclass());

    Xid clone = element.cloneObject();
    assertEquals(ALT_TEST_NAME, clone.getName());
    assertEquals(ALT_TEST_TYPE, clone.getType());
    assertEquals(ALT_TEST_SCLASS, clone.getSclass());
  }


}
