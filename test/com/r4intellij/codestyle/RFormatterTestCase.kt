package com.r4intellij.codestyle

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.codeStyle.CodeStyleSettings
import com.intellij.psi.codeStyle.CodeStyleSettingsManager
import com.intellij.psi.codeStyle.CommonCodeStyleSettings
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import com.intellij.util.IncorrectOperationException
import com.r4intellij.RFileType
import com.r4intellij.RLanguage
import com.r4intellij.editor.formatting.RCodeStyleSettings
import junit.framework.TestCase
import org.intellij.lang.annotations.Language
import org.jetbrains.annotations.NotNull

/**
 * @author Holger Brandl
 */
abstract class RFormatterTestCase : LightCodeInsightFixtureTestCase() {

    protected var rSettings: CodeStyleSettings? = null

    @Throws(Exception::class)
    override fun setUp() {
        super.setUp()
        setSettings(project)

        commomSettings.CLASS_BRACE_STYLE = CommonCodeStyleSettings.END_OF_LINE
        commomSettings.METHOD_BRACE_STYLE = CommonCodeStyleSettings.END_OF_LINE
        commomSettings.BRACE_STYLE = CommonCodeStyleSettings.END_OF_LINE
        commomSettings.initIndentOptions().INDENT_SIZE = 4
    }

    @Throws(Exception::class)
    override fun tearDown() {
        setSettingsBack()
        super.tearDown()
    }

    protected val commomSettings: CommonCodeStyleSettings
        get() = rSettings!!.getCommonSettings(RLanguage.getInstance())

    protected val rCustomSettings: RCodeStyleSettings
        get() = rSettings!!.getCustomSettings(RCodeStyleSettings::class.java)

    protected fun setSettings(project: Project) {
        TestCase.assertNull(rSettings)
        val settings = CodeStyleSettingsManager.getSettings(project)
        rSettings = settings.clone()

        val gr = rSettings!!.getIndentOptions(RFileType.INSTANCE)

        TestCase.assertNotSame(gr, settings.OTHER_INDENT_OPTIONS)
        gr.INDENT_SIZE = 2
        gr.CONTINUATION_INDENT_SIZE = 4

        gr.TAB_SIZE = 2
        rSettings!!.CLASS_COUNT_TO_USE_IMPORT_ON_DEMAND = 3

        CodeStyleSettingsManager.getInstance(project).setTemporarySettings(rSettings!!)
    }

    protected fun setSettingsBack() {
        val manager = CodeStyleSettingsManager.getInstance(project)
        rSettings!!.getIndentOptions(RFileType.INSTANCE).INDENT_SIZE = 200
        rSettings!!.getIndentOptions(RFileType.INSTANCE).CONTINUATION_INDENT_SIZE = 200
        rSettings!!.getIndentOptions(RFileType.INSTANCE).TAB_SIZE = 200

        rSettings!!.CLASS_COUNT_TO_USE_IMPORT_ON_DEMAND = 5
        manager.dropTemporarySettings()
        rSettings = null
    }

    protected fun checkFormatting(@Language("R") @NotNull fileText: String, @Language("R") @NotNull expected: String) {
        myFixture.configureByText(RFileType.INSTANCE, fileText.trim())
        checkFormatting(expected.trim())
    }

    //    fun doTest() {
    //        def (String before, String after) = TestUtils.readInput(testDataPath + getTestName(true) + ".test")
    //        checkFormatting(before, StringUtil.trimEnd(after, "\n"))
    //    }


    protected fun doFormat(file: PsiFile) {
        CommandProcessor.getInstance().executeCommand(project, {
            ApplicationManager.getApplication().runWriteAction {
                try {
                    val myTextRange = file.textRange
                    CodeStyleManager.getInstance(file.project).reformatText(file, myTextRange.startOffset, myTextRange.endOffset)
                } catch (e: IncorrectOperationException) {
                    LOG.error(e)
                }
            }
        }, null, null)
    }

    private fun checkFormatting(expected: String) {
        doFormat(myFixture.file)
        myFixture.checkResult(expected)
    }

    companion object {

        private val LOG = Logger.getInstance(RFormatterTestCase::class.java)
    }
}
