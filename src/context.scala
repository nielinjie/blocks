package nielinjie.app.blocks
package domain

import org.eclipse.ui.IEditorActionDelegate
import org.eclipse.jface.viewers.ISelection
import org.eclipse.swt.widgets.Shell
import org.eclipse.jface.text.IDocument
import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.resources.IFile
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jface.action.IAction
import org.eclipse.ui.IEditorPart
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.ui.IFileEditorInput
import org.eclipse.jface.text.TextSelection

trait FindContext {
  self: IEditorActionDelegate =>
  var selection: TextSelection = _
  var targetEditor: ITextEditor = _
  //  var document: IDocument = _
  var file: IFile = _
  var compilationUnit: ICompilationUnit = _
  def selectionChanged(action: IAction, selection: ISelection): Unit = {
    this.selection = selection.asInstanceOf[TextSelection]
  }
  def setActiveEditor(action: IAction, editorPart: IEditorPart): Unit = {

    targetEditor = editorPart.asInstanceOf[ITextEditor]
    //    document = targetEditor.getDocumentProvider.getDocument(targetEditor.getEditorInput())
    compilationUnit = JavaUI.getEditorInputJavaElement(targetEditor.getEditorInput()).asInstanceOf[ICompilationUnit]
    file = try { targetEditor.asInstanceOf[ITextEditor].getEditorInput().asInstanceOf[IFileEditorInput].getFile } catch { case e: Exception => null }

  }
  // def context =  RefactoringContext(compilationUnit, document.asInstanceOf[Document], selection.asInstanceOf[TextSelection], file)
}