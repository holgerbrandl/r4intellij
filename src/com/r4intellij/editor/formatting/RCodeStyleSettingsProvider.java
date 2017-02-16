package com.r4intellij.editor.formatting;

import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.lang.Language;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsProvider;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This class is based on the Groovy and Json formatter implementation.
 *
 * @author Mikhail Golubev
 * @author Holger Brandl
 */
public class RCodeStyleSettingsProvider extends CodeStyleSettingsProvider {
    @NotNull
    @Override
    public Configurable createSettingsPage(CodeStyleSettings settings, CodeStyleSettings originalSettings) {
        return new CodeStyleAbstractConfigurable(settings, originalSettings, "R") {
            @Override
            protected CodeStyleAbstractPanel createPanel(CodeStyleSettings settings) {
                final Language language = RLanguage.getInstance();
                final CodeStyleSettings currentSettings = getCurrentSettings();
                return new TabbedLanguageCodeStylePanel(language, currentSettings, settings) {
                    @Override
                    protected void initTabs(CodeStyleSettings settings) {
                        addIndentOptionsTab(settings);
                        addSpacesTab(settings);
                        addBlankLinesTab(settings);
                        addWrappingAndBracesTab(settings);
//                        addTab(new OtherRCodeStylePanel(settings));
                    }
                };
            }


            @Nullable
            @Override
            public String getHelpTopic() {
                return "reference.settingsdialog.codestyle.r"; //FIXME
            }
        };
    }


    @Nullable
    @Override
    public String getConfigurableDisplayName() {
        return RLanguage.getInstance().getDisplayName();
    }


    @Nullable
    @Override
    public CustomCodeStyleSettings createCustomSettings(CodeStyleSettings settings) {
        return new RCodeStyleSettings(settings);
    }
}
