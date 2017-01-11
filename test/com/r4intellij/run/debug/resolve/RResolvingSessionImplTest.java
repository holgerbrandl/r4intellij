package com.r4intellij.run.debug.resolve;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.PlatformTestCase;
import com.intellij.xdebugger.XSourcePosition;
import com.r4intellij.debugger.data.RLocation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;

import static com.r4intellij.debugger.data.RFunctionConstants.MAIN_FUNCTION_NAME;

public class RResolvingSessionImplTest extends PlatformTestCase {

  public void testNext_CurrentScope() throws IOException {
    final String text = "f <- function(x) {\n" +
                        "    x\n" +
                        "}\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 4)
      ),
      virtualFile,
      3
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 2)
      ),
      virtualFile,
      1
    );
  }

  public void testNext_PreviousScopeAfterCurrent() throws IOException {
    final String text = "f <- function() {\n" +
                        "    g()\n" +
                        "}\n" +
                        "g <- function() {\n" +
                        "    c(1:5)\n" +
                        "}\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 7)
      ),
      virtualFile,
      6
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 2)
      ),
      virtualFile,
      1
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("g", 5)
      ),
      virtualFile,
      4
    );
  }

  public void testNext_PreviousScopeBeforeCurrent() throws IOException {
    final String text = "g <- function() {\n" +
                        "    c(1:5)\n" +
                        "}\n" +
                        "f <- function() {\n" +
                        "    g()\n" +
                        "}\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 7)
      ),
      virtualFile,
      6
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 5)
      ),
      virtualFile,
      4
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("g", 2)
      ),
      virtualFile,
      1
    );
  }

  public void testNext_Overridden() throws IOException {
    final String text = "f <- function() {\n" +
                        "    c(1:5)\n" +
                        "}\n" +
                        "f <- function() {\n" +
                        "    c(1:6)\n" +
                        "}\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 7)
      ),
      virtualFile,
      6
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 5)
      ),
      virtualFile,
      4
    );
  }

  public void testNext_Unknown() throws IOException {
    final String text = "f <- function(x) {\n" +
                        "    x\n" +
                        "}\n" +
                        "g()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 4)
      ),
      virtualFile,
      3
    );

    assertNull(
      resolvingSession.resolveNext(
        new RLocation("g", 2)
      )
    );

    assertNull(
      resolvingSession.resolveNext(
        new RLocation("h", 2)
      )
    );
  }

  public void testNext_FirstIsNotMain() throws IOException {
    final VirtualFile virtualFile = createVirtualFile("");
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    assertNull(
      resolvingSession.resolveNext(
        new RLocation("f", 1)
      )
    );
  }

  public void testNext_Unbrace() throws IOException {
    final String text = "print(\"ok\")\n" +
                        "f <- function() c(1:6)\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 3)
      ),
      virtualFile,
      2
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 0)
      ),
      virtualFile,
      1
    );
  }

  public void testDropLast() throws IOException {
    final String text = "f <- function(x) {\n" +
                        "    x\n" +
                        "}\n" +
                        "f()\n" +
                        "f()";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 4)
      ),
      virtualFile,
      3
    );

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation("f", 2)
      ),
      virtualFile,
      1
    );

    resolvingSession.dropLast(1);

    checkPosition(
      resolvingSession.resolveCurrent(5),
      virtualFile,
      4
    );
  }

  public void testCurrent() throws IOException {
    final String text = "print(\"1\")\n" +
                        "print(\"2\")";

    final VirtualFile virtualFile = createVirtualFile(text);
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 1)
      ),
      virtualFile,
      0
    );

    checkPosition(
      resolvingSession.resolveCurrent(2),
      virtualFile,
      1
    );
  }

  public void testCurrent_Unknown() throws IOException {
    final VirtualFile virtualFile = createVirtualFile("f()");
    final RResolvingSessionImpl resolvingSession = new RResolvingSessionImpl(getProject(), virtualFile);

    checkPosition(
      resolvingSession.resolveNext(
        new RLocation(MAIN_FUNCTION_NAME, 1)
      ),
      virtualFile,
      0
    );

    assertNull(
      resolvingSession.resolveNext(
        new RLocation("f", 1)
      )
    );

    assertNull(
      resolvingSession.resolveCurrent(2)
    );
  }

  @NotNull
  private VirtualFile createVirtualFile(@NotNull final String text) throws IOException {
    final VirtualFile result = getVirtualFile(createTempFile("script.r", text));
    assert result != null;

    return result;
  }

  private void checkPosition(@Nullable final XSourcePosition position, @NotNull final VirtualFile expectedFile, final int expectedLine) {
    assertNotNull(position);
    assertEquals(expectedFile, position.getFile());
    assertEquals(expectedLine, position.getLine());
  }
}