package com.r4intellij.run.debug.resolve;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.xdebugger.XDebuggerUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.r4intellij.debugger.data.RLocation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

public class RResolvingSessionImpl implements RResolvingSession {

  @NotNull
  private static final String PSI_FILE_COULD_NOT_BE_LOADED = "PSI file couldn't be loaded [path: %s]";

  @NotNull
  private static final String DOCUMENT_COULD_NOT_BE_LOADED = "Document couldn't be loaded [path: %s]";

  @NotNull
  private final RFunctionDescriptor myRoot;

  @NotNull
  private final VirtualFile myVirtualFile;

  @NotNull
  private final List<RResolvingSessionEntry> myEntries;

  public RResolvingSessionImpl(@NotNull final Project project, @NotNull final VirtualFile virtualFile) throws IOException {
    myVirtualFile = virtualFile;
    myRoot = calculateRoot(project, getPsiFile(project));
    myEntries = new ArrayList<RResolvingSessionEntry>();
  }

  @Override
  @Nullable
  public XSourcePosition resolveNext(@NotNull final RLocation nextLocation) {
    myEntries.add(resolveNextLocation(nextLocation));

    return createCurrentPosition();
  }

  @Override
  @Nullable
  public XSourcePosition resolveCurrent(final int line) {
    updateCurrentLocation(line);

    return createCurrentPosition();
  }

  @Override
  public void dropLast(final int number) {
    final ListIterator<RResolvingSessionEntry> iterator = myEntries.listIterator(myEntries.size());
    iterator.previous();

    for (int i = 0; i < number; i++) {
      iterator.remove();
      iterator.previous();
    }
  }

  @NotNull
  private PsiFile getPsiFile(@NotNull final Project project) throws IOException {
    final PsiFile psiFile = PsiManager.getInstance(project).findFile(myVirtualFile);

    if (psiFile == null) {
      throw new IOException(
        String.format(PSI_FILE_COULD_NOT_BE_LOADED, myVirtualFile.getPath())
      );
    }

    return psiFile;
  }

  @NotNull
  private RFunctionDescriptor calculateRoot(@NotNull final Project project, @NotNull final PsiFile psiFile) throws IOException {
    final RFunctionDefinitionProcessor processor = new RFunctionDefinitionProcessor(getDocument(project, psiFile));

    PsiTreeUtil.processElements(psiFile, processor);

    return processor.getRoot();
  }

  @NotNull
  private Document getDocument(@NotNull final Project project, @NotNull final PsiFile psiFile) throws IOException {
    final Document document = PsiDocumentManager.getInstance(project).getDocument(psiFile);

    if (document == null) {
      throw new IOException(
        String.format(DOCUMENT_COULD_NOT_BE_LOADED, psiFile.getVirtualFile().getPath())
      );
    }

    return document;
  }

  @NotNull
  private RResolvingSessionEntry resolveNextLocation(@NotNull final RLocation nextLocation) {
    final RFunctionDescriptor descriptor = resolveNextFunction(nextLocation.getFunctionName());

    return new RResolvingSessionEntry(
      descriptor,
      resolveLine(descriptor, nextLocation.getLine())
    );
  }

  @Nullable
  private XSourcePosition createCurrentPosition() {
    final RResolvingSessionEntry entry = myEntries.get(myEntries.size() - 1);

    if (entry.myDescriptor == null) {
      return null;
    }

    return XDebuggerUtil.getInstance().createPosition(myVirtualFile, entry.myLine);
  }

  private void updateCurrentLocation(final int line) {
    final int lastIndex = myEntries.size() - 1;
    final RFunctionDescriptor descriptor = myEntries.get(lastIndex).myDescriptor;

    myEntries.set(
      lastIndex,
      new RResolvingSessionEntry(
        descriptor,
        resolveLine(descriptor, line)
      )
    );
  }

  @Nullable
  private RFunctionDescriptor resolveNextFunction(@NotNull final String nextFunctionName) {
    if (myEntries.isEmpty()) {
      return myRoot.getName().equals(nextFunctionName) ? myRoot : null;
    }
    else {
      return resolveNextFunction(myEntries.listIterator(myEntries.size()), nextFunctionName);
    }
  }

  private int resolveLine(@Nullable final RFunctionDescriptor descriptor, final int line) {
    final boolean isUnbraceFunction = descriptor != null && line == 0;

    return isUnbraceFunction
           ? descriptor.getStartLine()
           : line - 1; // convert 1-based to 0-based
  }

  @Nullable
  private RFunctionDescriptor resolveNextFunction(@NotNull final ListIterator<RResolvingSessionEntry> entries,
                                                  @NotNull final String nextFunctionName) {
    if (!entries.hasPrevious()) {
      return null;
    }

    final RFunctionDescriptor candidate = resolveNextFunction(entries.previous(), nextFunctionName);

    if (candidate != null) {
      return candidate;
    }

    return resolveNextFunction(entries, nextFunctionName);
  }

  @Nullable
  private RFunctionDescriptor resolveNextFunction(@NotNull final RResolvingSessionEntry entry,
                                                  @NotNull final String nextFunctionName) {
    final RFunctionDescriptor currentDescriptor = entry.myDescriptor;

    if (currentDescriptor == null) {
      return null;
    }

    RFunctionDescriptor result = null;

    if (currentDescriptor.getChildren().containsKey(nextFunctionName)) {
      int distance = Integer.MAX_VALUE;

      for (final RFunctionDescriptor candidate : currentDescriptor.getChildren().get(nextFunctionName)) {
        final int currentDistance = currentDescriptor.getStartLine() + entry.myLine - candidate.getStartLine();

        if (currentDistance > 0 && currentDistance < distance) { // candidate is declared before the current line
          result = candidate;
          distance = currentDistance;
        }
      }
    }

    return result;
  }

  private static class RResolvingSessionEntry {

    @Nullable
    private final RFunctionDescriptor myDescriptor;

    private final int myLine;

    public RResolvingSessionEntry(@Nullable final RFunctionDescriptor descriptor, final int line) {
      myDescriptor = descriptor;
      myLine = line;
    }
  }
}
