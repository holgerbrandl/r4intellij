package com.r4intellij.run.graphics;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;

class RGraphicsPanel {

    @NotNull
    private static final Logger LOGGER = Logger.getInstance(RGraphicsPanel.class);

    @NotNull
    private static final String NO_GRAPHICS = "No graphics";

    @NotNull
    private static final String PANEL_HAS_BEEN_RESET = "Panel has been reset";

    @NotNull
    private static final String PANEL_HAS_BEEN_UPDATED = "Panel has been updated";

    @NotNull
    private static final String GRAPHICS_COULD_NOT_BE_LOADED = "Graphics couldn't be loaded";

    @NotNull
    private static final String SNAPSHOT_COULD_NOT_BE_ENCODED = "Snapshot couldn't be encoded [name: %s]";

    @NotNull
    private static final String SNAPSHOT_HAS_BEEN_LOADED = "Snapshot has been loaded [name: %s]";

    @NotNull
    private final RGraphicsState myState;

    @NotNull
    private final JLabel myLabel;

    @NotNull
    private final JPanel myPanel;


    public RGraphicsPanel(@NotNull final RGraphicsState state) {
        myState = state;

        myLabel = new JLabel(NO_GRAPHICS);

        myPanel = new JPanel();
        myPanel.add(myLabel);
    }


    @NotNull
    public JPanel getPanel() {
        return myPanel;
    }


    public void refresh() {
        try {
            myLabel.setText(null);
            myLabel.setIcon(new ImageIcon(loadCurrentGraphics()));

            LOGGER.debug(PANEL_HAS_BEEN_UPDATED);
        } catch (final IOException e) {
            myLabel.setIcon(null);
            myLabel.setText(GRAPHICS_COULD_NOT_BE_LOADED);

            LOGGER.error(e);
        }
    }


    public void reset() {
        myLabel.setIcon(null);
        myLabel.setText(NO_GRAPHICS);

        LOGGER.debug(PANEL_HAS_BEEN_RESET);
    }


    @NotNull
    private BufferedImage loadCurrentGraphics() throws IOException {
        final VirtualFile file = myState.current();
        final InputStream stream = file.getInputStream();

        try {
            final BufferedImage image = ImageIO.read(stream);

            if (image == null) {
                throw new IllegalStateException(
                        String.format(SNAPSHOT_COULD_NOT_BE_ENCODED, file.getName())
                );
            }

            LOGGER.debug(
                    String.format(SNAPSHOT_HAS_BEEN_LOADED, file.getName())
            );

            return image; // TODO [ui][resize]
        } finally {
            try {
                stream.close();
            } catch (final IOException e) {
                LOGGER.warn(e);
            }
        }
    }
}
