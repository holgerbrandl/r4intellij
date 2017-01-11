package com.r4intellij.run.graphics;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.r4intellij.RHelpersLocator;
import com.r4intellij.debugger.data.RCommands;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;

public final class RGraphicsUtils {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RGraphicsUtils.class);

  @NotNull
  private static final Map<String, RGraphicsState> GRAPHICS_STATES = new HashMap<String, RGraphicsState>();

  @NotNull
  private static final String DEVICE_LIB_FORMAT = "libtherplugin_device%s.%s";

  @NotNull
  private static final String DEVICE_FUNCTION_NAME = SERVICE_FUNCTION_PREFIX + "device_init";

  @NotNull
  private static final String SETUP_DEVICE_COMMAND = RCommands.optionsCommand("device", DEVICE_FUNCTION_NAME);

  @NotNull
  private static final String LIB_IS_NOT_FOUND = "Lib is not found [path: %s]";

  @NotNull
  private static final String LIB_IS_NOT_READABLE = "Lib is not readable [path: %s]";

  @NotNull
  private static final String PROJECT_DIR_IS_NOT_FOUND = "Project dir is not found [path: %s]";

  @NotNull
  private static final String SNAPSHOT_DIR_NAME = "snapshots";

  @NotNull
  private static final String SNAPSHOT_DIR_IS_NOT_FOUND = "Snapshot dir is not found [path: %s]";

  @NotNull
  private static final String SNAPSHOT_DIR_IS_FOUND = "Snapshot dir is found [path: %s]";

  @NotNull
  private static final String SNAPSHOT_DIR_IS_NOT_WRITABLE = "Snapshot dir is not writable [path: %s]";

  @NotNull
  private static final String SNAPSHOT_DIR_HAS_BEEN_CREATED = "Snapshot dir has been created [path: %s]";

  @NotNull
  public static List<String> calculateInitCommands(@NotNull final Project project, final boolean is64Bit) {
    final String libPath = getLibPath(calculateLibName(is64Bit));

    if (libPath != null) {
      final VirtualFile snapshotDir = getSnapshotDir(project);

      if (snapshotDir != null) {
        return Arrays.asList(
          RCommands.loadLibCommand(libPath),
          DEVICE_FUNCTION_NAME + " <- function() { .Call(\"" + DEVICE_FUNCTION_NAME + "\", \"" + snapshotDir.getPath() + "\") }",
          SETUP_DEVICE_COMMAND
        );
      }
    }

    return Collections.emptyList();
  }

  @NotNull
  public static RGraphicsState getGraphicsState(@NotNull final Project project) {
    final VirtualFile snapshotDir = getSnapshotDir(project);

    if (snapshotDir == null) {
      return new REmptyGraphicsState();
    }

    final String snapshotDirPath = snapshotDir.getPath();

    if (!GRAPHICS_STATES.containsKey(snapshotDirPath)) {
      final RGraphicsStateImpl state = new RGraphicsStateImpl(snapshotDir);

      Disposer.register(project, state);
      Disposer.register(
        state,
        new Disposable() {
          @Override
          public void dispose() {
            GRAPHICS_STATES.remove(snapshotDirPath);
          }
        }
      );

      GRAPHICS_STATES.put(snapshotDirPath, state);
    }

    return GRAPHICS_STATES.get(snapshotDirPath);
  }

  @Nullable
  private static String getLibPath(@NotNull final String libName) {
    final File libFile = RHelpersLocator.getHelperFile(libName);
    final String absolutePath = FileUtil.toSystemIndependentName(libFile.getAbsolutePath());

    if (!libFile.exists()) {
      LOGGER.warn(
        String.format(LIB_IS_NOT_FOUND, absolutePath)
      );

      return null;
    }

    if (!libFile.canRead()) {
      LOGGER.warn(
        String.format(LIB_IS_NOT_READABLE, absolutePath)
      );

      return null;
    }

    return absolutePath;
  }

  @NotNull
  private static String calculateLibName(final boolean is64Bit) {
    return String.format(
      DEVICE_LIB_FORMAT,
      is64Bit ? "64" : "32",
      SystemInfo.isWindows ? "dll" : "so"
    );
  }

  @Nullable
  private static VirtualFile getSnapshotDir(@NotNull final Project project) {
    final String projectDirName = Project.DIRECTORY_STORE_FOLDER;
    final VirtualFile dotIdeaDir = project.getBaseDir().findChild(projectDirName);

    if (dotIdeaDir != null) {
      return getSnapshotDir(dotIdeaDir);
    }
    else {
      LOGGER.warn(
        String.format(
          PROJECT_DIR_IS_NOT_FOUND,
          new File(project.getBasePath(), projectDirName).getAbsolutePath()
        )
      );

      return null;
    }
  }

  @Nullable
  private static VirtualFile getSnapshotDir(@NotNull final VirtualFile dotIdeaDir) {
    final VirtualFile snapshotDir = dotIdeaDir.findChild(SNAPSHOT_DIR_NAME);

    if (snapshotDir != null) {
      return checkSnapshotDir(snapshotDir);
    }
    else {
      LOGGER.info(
        String.format(
          SNAPSHOT_DIR_IS_NOT_FOUND,
          new File(dotIdeaDir.getPath(), SNAPSHOT_DIR_NAME).getAbsolutePath()
        )
      );

      return createSnapshotDir(dotIdeaDir);
    }
  }

  @Nullable
  private static VirtualFile checkSnapshotDir(@NotNull final VirtualFile snapshotDir) {
    final String snapshotDirPath = snapshotDir.getPath();

    if (snapshotDir.isWritable()) {
      LOGGER.info(
        String.format(SNAPSHOT_DIR_IS_FOUND, snapshotDirPath)
      );

      return snapshotDir;
    }
    else {
      LOGGER.warn(
        String.format(SNAPSHOT_DIR_IS_NOT_WRITABLE, snapshotDirPath)
      );

      return null;
    }
  }

  @Nullable
  private static VirtualFile createSnapshotDir(@NotNull final VirtualFile dotIdeaDir) {
    final Ref<VirtualFile> resultRef = new Ref<VirtualFile>(null);

    ApplicationManager.getApplication().runWriteAction(
      new Runnable() {
        @Override
        public void run() {
          try {
            resultRef.set(
              dotIdeaDir.createChildDirectory(new RGraphicsUtils(), SNAPSHOT_DIR_NAME)
            );

            LOGGER.info(
              String.format(SNAPSHOT_DIR_HAS_BEEN_CREATED, resultRef.get().getPath())
            );
          }
          catch (final IOException e) {
            LOGGER.error(e);
          }
        }
      }
    );

    return resultRef.get();
  }
}
