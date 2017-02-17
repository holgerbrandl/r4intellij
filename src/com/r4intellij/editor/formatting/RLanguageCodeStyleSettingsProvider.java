package com.r4intellij.editor.formatting;

import com.intellij.application.options.IndentOptionsEditor;
import com.intellij.application.options.SmartIndentOptionsEditor;
import com.intellij.lang.Language;
import com.intellij.psi.codeStyle.CodeStyleSettingsCustomizable;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.intellij.psi.codeStyle.CodeStyleSettingsCustomizable.SPACES_OTHER;

/**
 * This class is based on the Groovy and Json formatter implementation.
 *
 * @author Mikhail Golubev
 * @author holgerbrandl
 */
public class RLanguageCodeStyleSettingsProvider extends LanguageCodeStyleSettingsProvider {
    @org.intellij.lang.annotations.Language("TEXT")
    private static final String SAMPLE = "normalMean <- function(samples=10){\n" +
            "  data <- rnorm(samples)\n" +
            "  mean(data)\n" +
            "}\n" +
            "\n" +
            "for(i in 1:10){\n" +
            "    print(i)\n" +
            "}\n" +
            "\n" +
            "#' some roxygen style function documentaiton\n" +
            "#' @param a arggument one \n" +
            "#' @param b arggument b \n" +
            "addTwo <- function(a, b){\n" +
            "  as.numeric(a) + as.numeric(b)\n" +
            "}\n" +
            "\n" +
            "## pipe example\n" +
            "models <- mtcars %>%\n" +
            "   group_by(cyl) %>%\n" +
            "   do(lm = lm(mpg ~ wt, data = .)) %>%\n" +
            "   summarise(rsq = summary(lm)$r.squared) %>%\n" +
            "   head()\n" +
            "   \n" +
            "\n" +
            "ggplot(iris) + geom_point() + ggtitle(\"iris plot\") + facet_grid(~Species) + scale_x_log10()";


    @Override
    public void customizeSettings(@NotNull CodeStyleSettingsCustomizable consumer, @NotNull SettingsType settingsType) {
        if (settingsType == SettingsType.SPACING_SETTINGS) {
            consumer.showStandardOptions("SPACE_WITHIN_BRACKETS",
                    "SPACE_WITHIN_BRACES",
                    "SPACE_AFTER_COMMA",
                    "SPACE_BEFORE_COMMA");
            consumer.renameStandardOption("SPACE_WITHIN_BRACES", "Braces");
            consumer.showCustomOption(RCodeStyleSettings.class, "SPACE_BEFORE_COLON", "Before ':'", SPACES_OTHER);
            consumer.showCustomOption(RCodeStyleSettings.class, "SPACE_AFTER_COLON", "After ':'", SPACES_OTHER);
        } else if (settingsType == SettingsType.BLANK_LINES_SETTINGS) {
            consumer.showStandardOptions("KEEP_BLANK_LINES_IN_CODE");
        } else if (settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS) {
            consumer.showStandardOptions("RIGHT_MARGIN",
                    "WRAP_ON_TYPING",
                    "KEEP_LINE_BREAKS",
                    "WRAP_LONG_LINES");

            consumer.showCustomOption(RCodeStyleSettings.class,
                    "ARRAY_WRAPPING",
                    "Arrays",
                    null,
                    CodeStyleSettingsCustomizable.WRAP_OPTIONS,
                    CodeStyleSettingsCustomizable.WRAP_VALUES);

            consumer.showCustomOption(RCodeStyleSettings.class,
                    "OBJECT_WRAPPING",
                    "Objects",
                    null,
                    CodeStyleSettingsCustomizable.WRAP_OPTIONS,
                    CodeStyleSettingsCustomizable.WRAP_VALUES);

        }
    }


    @NotNull
    @Override
    public Language getLanguage() {
        return RLanguage.getInstance();
    }


    @Nullable
    @Override
    public IndentOptionsEditor getIndentOptionsEditor() {
        return new SmartIndentOptionsEditor();
    }


    @Override
    public String getCodeSample(@NotNull SettingsType settingsType) {
        return SAMPLE;
    }


    @Nullable
    @Override
    public CommonCodeStyleSettings getDefaultCommonSettings() {
        CommonCodeStyleSettings commonSettings = new CommonCodeStyleSettings(RLanguage.getInstance());
        CommonCodeStyleSettings.IndentOptions indentOptions = commonSettings.initIndentOptions();
//        indentOptions.INDENT_SIZE = 2;
        // strip all blank lines by default
        commonSettings.KEEP_BLANK_LINES_IN_CODE = 0;
        return commonSettings;
    }
}
