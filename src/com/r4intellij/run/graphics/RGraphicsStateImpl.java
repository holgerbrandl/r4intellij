package com.r4intellij.run.graphics;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.vfs.*;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class RGraphicsStateImpl implements RGraphicsState, Disposable {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RGraphicsStateImpl.class);

  @NotNull
  private static final String STARTED_TO_LISTEN_FOR_NEW_SNAPSHOTS = "Started to listen for new snapshots [dir: %s]";

  @NotNull
  private static final String CURRENT_SNAPSHOT_WAS_NOT_SET = "Current snapshot wasn't set [dir: %s]";

  @NotNull
  private static final String SNAPSHOT_IS_NOT_FOUND = "Snapshot is not found [name: %s, dir: %s]";

  @NotNull
  private static final String STATE_HAS_BEEN_RESET = "State has been reset [dir: %s]";

  @NotNull
  private static final String FILE_HAS_BEEN_REMOVED = "File has been removed [name: %s, dir: %s]";

  @NotNull
  private static final String STATE_HAS_BEEN_DISPOSED = "State has been disposed [dir: %s]";

  @NotNull
  private static final String NO_SNAPSHOTS = "No snapshots [dir: %s]";

  @NotNull
  private static final String NO_NEXT_SNAPSHOT_AFTER_CURRENT = "No next snapshot after current [current: %s, dir: %s]";

  @NotNull
  private static final String NO_PREVIOUS_SNAPSHOT_BEFORE_CURRENT = "No previous snapshot before current [current: %s, dir: %s]";

  @NotNull
  private static final String MOVED_FORWARD = "Moved forward [previous: %s, current: %s, dir: %s]";

  @NotNull
  private static final String MOVED_BACKWARD = "Moved backward [previous: %s, current: %s, dir: %s]";

  @NotNull
  private static final String SNAPSHOT_NAME_FORMAT = "snapshot_%d.png";

  @NotNull
  private static final Pattern SNAPSHOT_NAME_PATTERN = Pattern.compile("^snapshot_(\\d+)\\.png$");

  @NotNull
  private static final String SNAPSHOT_HAS_BEEN_ADDED = "Snapshot has been added to the state [name: %s, dir: %s]";

  @NotNull
  private static final String SNAPSHOT_HAS_BEEN_REMOVED = "Snapshot has been removed from the state [name: %s, dir: %s]";

  @NotNull
  private static final String ILLEGAL_SNAPSHOT_NAME = "Illegal snapshot name [name: %s]";

  @NotNull
  private static final String UPDATED_SNAPSHOT = "Updated snapshot [name: %s, dir: %s]";

  @NotNull
  private static final String RENAMED_SNAPSHOT_WILL_BE_REMOVED = "Renamed snapshot will be removed from the state [name: %s, dir: %s]";

  @NotNull
  private static final String MOVED_SNAPSHOT_WILL_BE_REMOVED = "Moved snapshot will be removed from the state [name: %s, dir: %s]";

  @NotNull
  private final TreeSet<Integer> mySnapshotIds;

  @NotNull
  private final VirtualFile mySnapshotDir;

  @NotNull
  private final String mySnapshotDirPath;

  @NotNull
  private final List<Listener> myListeners;

  private int myCurrentId;

  public RGraphicsStateImpl(@NotNull final VirtualFile snapshotDir) {
    mySnapshotIds = new TreeSet<Integer>();
    mySnapshotDir = snapshotDir;
    mySnapshotDirPath = snapshotDir.getPath();
    myListeners = new LinkedList<Listener>();

    myCurrentId = -1;

    VirtualFileManager.getInstance().addVirtualFileListener(
      new RGraphicsListener(),
      this
    );

    LOGGER.info(
      String.format(STARTED_TO_LISTEN_FOR_NEW_SNAPSHOTS, mySnapshotDirPath)
    );
  }

  @Override
  public boolean hasNext() {
    return mySnapshotIds.higher(myCurrentId) != null;
  }

  @Override
  public boolean hasPrevious() {
    return mySnapshotIds.lower(myCurrentId) != null;
  }

  @Override
  public boolean hasCurrent() {
    return myCurrentId != -1;
  }

  @Override
  public void next() {
    advance(true);
  }

  @Override
  public void previous() {
    advance(false);
  }

  @Override
  @NotNull
  public VirtualFile current() throws FileNotFoundException {
    if (!hasCurrent()) {
      throw new NoSuchElementException(
        String.format(CURRENT_SNAPSHOT_WAS_NOT_SET, mySnapshotDirPath)
      );
    }

    final String name = calculateSnapshotName(myCurrentId);
    final VirtualFile result = mySnapshotDir.findChild(name);

    if (result == null) {
      throw new FileNotFoundException(
        String.format(SNAPSHOT_IS_NOT_FOUND, name, mySnapshotDirPath)
      );
    }

    return result;
  }

  @Override
  public int size() {
    return mySnapshotIds.size();
  }

  @Override
  public void refresh(final boolean asynchronous) {
    mySnapshotDir.refresh(asynchronous, true);
  }

  @Override
  public void reset() {
    myCurrentId = -1;
    mySnapshotIds.clear();

    LOGGER.debug(
      String.format(STATE_HAS_BEEN_RESET, mySnapshotDirPath)
    );

    for (final Listener listener : myListeners) {
      listener.onReset();
    }
  }

  @Override
  public void addListener(@NotNull final Listener listener) {
    myListeners.add(listener);
  }

  @Override
  public void removeListener(@NotNull final Listener listener) {
    myListeners.remove(listener);
  }

  @Override
  public void dispose() {
    ApplicationManager.getApplication().runWriteAction(
      new Runnable() {
        @Override
        public void run() {
          for (final VirtualFile file : mySnapshotDir.getChildren()) {
            try {
              file.delete(RGraphicsStateImpl.this);

              LOGGER.debug(
                String.format(FILE_HAS_BEEN_REMOVED, file.getName(), mySnapshotDirPath)
              );
            }
            catch (final IOException e) {
              LOGGER.warn(e);
            }
          }
        }
      }
    );

    LOGGER.info(
      String.format(STATE_HAS_BEEN_DISPOSED, mySnapshotDirPath)
    );
  }

  private void advance(final boolean forward) {
    final Integer newCurrentId = forward ? mySnapshotIds.higher(myCurrentId) : mySnapshotIds.lower(myCurrentId);

    if (newCurrentId == null) {
      if (!hasCurrent()) {
        throw new NoSuchElementException(
          String.format(NO_SNAPSHOTS, mySnapshotDirPath)
        );
      }
      else {
        throw new NoSuchElementException(
          String.format(
            forward ? NO_NEXT_SNAPSHOT_AFTER_CURRENT : NO_PREVIOUS_SNAPSHOT_BEFORE_CURRENT,
            calculateSnapshotName(myCurrentId),
            mySnapshotDirPath
          )
        );
      }
    }

    LOGGER.debug(
      String.format(
        forward ? MOVED_FORWARD : MOVED_BACKWARD,
        calculateSnapshotName(myCurrentId),
        calculateSnapshotName(newCurrentId),
        mySnapshotDirPath
      )
    );

    myCurrentId = newCurrentId;

    for (final Listener listener : myListeners) {
      listener.onCurrentChange();
    }
  }

  @NotNull
  private String calculateSnapshotName(final int snapshotId) {
    return String.format(SNAPSHOT_NAME_FORMAT, snapshotId);
  }

  private boolean isSnapshot(@NotNull final VirtualFile file) {
    return SNAPSHOT_NAME_PATTERN.matcher(file.getName()).matches() && VfsUtilCore.isAncestor(mySnapshotDir, file, false);
  }

  private void add(@NotNull final VirtualFile file) {
    final String name = file.getName();
    final int id = calculateSnapshotId(name);

    if (mySnapshotIds.add(id)) {
      LOGGER.info(
        String.format(SNAPSHOT_HAS_BEEN_ADDED, name, mySnapshotDirPath)
      );

      for (final Listener listener : myListeners) {
        listener.onAdd();
      }
    }
  }

  private void remove(@NotNull final VirtualFile file) {
    final String name = file.getName();
    final int id = calculateSnapshotId(name);

    if (mySnapshotIds.remove(id)) {
      if (id == myCurrentId) {
        if (hasPrevious()) {
          previous();
        }
        else if (hasNext()) {
          next();
        }
        else {
          reset();
        }
      }

      LOGGER.info(
        String.format(SNAPSHOT_HAS_BEEN_REMOVED, name, mySnapshotDirPath)
      );
    }
  }

  private int calculateSnapshotId(@NotNull final String snapshotName) {
    final Matcher matcher = SNAPSHOT_NAME_PATTERN.matcher(snapshotName);

    if (matcher.find()) {
      return Integer.parseInt(matcher.group(1));
    }
    else {
      throw new IllegalArgumentException(
        String.format(ILLEGAL_SNAPSHOT_NAME, snapshotName)
      );
    }
  }

  private class RGraphicsListener extends VirtualFileAdapter {

    @Override
    public void contentsChanged(@NotNull final VirtualFileEvent event) {
      final VirtualFile file = event.getFile();

      if (isSnapshot(file)) {
        final String name = event.getFileName();

        LOGGER.debug(
          String.format(UPDATED_SNAPSHOT, name, mySnapshotDirPath)
        );

        add(file);

        if (myCurrentId == calculateSnapshotId(name)) {
          for (final Listener listener : myListeners) {
            listener.onCurrentChange();
          }
        }
      }
    }

    @Override
    public void fileCreated(@NotNull final VirtualFileEvent event) {
      final VirtualFile file = event.getFile();

      if (isSnapshot(file)) {
        add(file);
      }
    }

    @Override
    public void fileDeleted(@NotNull final VirtualFileEvent event) {
      final VirtualFile file = event.getFile();

      if (isSnapshot(file)) {
        remove(file);
      }
    }

    @Override
    public void fileCopied(@NotNull final VirtualFileCopyEvent event) {
      // ignore
    }

    @Override
    public void beforePropertyChange(@NotNull final VirtualFilePropertyEvent event) {
      final VirtualFile file = event.getFile();

      if (event.getPropertyName().equals(VirtualFile.PROP_NAME) && isSnapshot(file)) {
        LOGGER.warn(
          String.format(
            RENAMED_SNAPSHOT_WILL_BE_REMOVED,
            event.getFileName(),
            mySnapshotDirPath
          )
        );

        remove(file);
      }
    }

    @Override
    public void beforeFileMovement(@NotNull final VirtualFileMoveEvent event) {
      final VirtualFile file = event.getFile();

      if (isSnapshot(file)) {
        LOGGER.warn(
          String.format(
            MOVED_SNAPSHOT_WILL_BE_REMOVED,
            event.getFileName(),
            mySnapshotDirPath
          )
        );

        remove(file);
      }
    }
  }
}
