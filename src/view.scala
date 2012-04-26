package nielinjie.app.blocks
package ui

import org.eclipse.ui.part.ViewPart
import org.eclipse.swt.awt.SWT_AWT
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT
import javax.swing.JScrollPane

import domain._

class MethodsView extends ViewPart {
  val ui = new MethodUI({m:Method=>})
  override def createPartControl(parent: Composite) = {
    val composite = new Composite(parent, SWT.EMBEDDED | SWT.NO_BACKGROUND);
    val frame = SWT_AWT.new_Frame(composite);
    frame.add(new JScrollPane(ui.viewer))
//    frame.add(ui.viewer)
  }
  override def setFocus() = {
  }
  def viewer = ui.viewer
}