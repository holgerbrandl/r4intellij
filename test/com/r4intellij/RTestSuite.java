package com.r4intellij;

import com.r4intellij.debugger.RDebuggerStringUtilsTest;
import com.r4intellij.debugger.RDebuggerTest;
import com.r4intellij.debugger.RDebuggerUtilsTest;
import com.r4intellij.debugger.RForcedFunctionDebuggerHandlerTest;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluatorImplTest;
import com.r4intellij.debugger.evaluator.RExpressionHandlerImplTest;
import com.r4intellij.debugger.executor.RExecutionResultCalculatorImplTest;
import com.r4intellij.debugger.executor.RExecutorUtilsTest;
import com.r4intellij.debugger.frame.RValueModifierHandlerImplTest;
import com.r4intellij.debugger.frame.RValueModifierImplTest;
import com.r4intellij.debugger.frame.RVarsLoaderImplTest;
import com.r4intellij.debugger.function.RBraceFunctionDebuggerTest;
import com.r4intellij.debugger.function.RFunctionDebuggerFactoryImplTest;
import com.r4intellij.debugger.function.RTraceAndDebugUtilsTest;
import com.r4intellij.debugger.function.RUnbraceFunctionDebuggerTest;
import com.r4intellij.inspections.RTypeCheckerInspectionTest;
import com.r4intellij.inspections.RUnresolvedReferenceInspectionTest;
import com.r4intellij.inspections.RUnusedParameterInspectionTest;
import com.r4intellij.lexer.RHighlightingLexerTest;
import com.r4intellij.parser.RParsingTest;
import com.r4intellij.rename.RRenameTest;
import com.r4intellij.run.RCommandLineCalculatorTest;
import com.r4intellij.run.ROutputReceiverImplTest;
import com.r4intellij.run.configuration.RRunConfigurationEditorTest;
import com.r4intellij.run.configuration.RRunConfigurationTest;
import com.r4intellij.run.configuration.RRunConfigurationTypeTest;
import com.r4intellij.run.configuration.RRunConfigurationUtilsTest;
import com.r4intellij.run.debug.RLineBreakpointUtilsTest;
import com.r4intellij.run.debug.resolve.RFunctionDefinitionProcessorTest;
import com.r4intellij.run.debug.resolve.RResolvingSessionImplTest;
import com.r4intellij.run.debug.stack.RXPresentationUtilsTest;
import com.r4intellij.run.debug.stack.RXStackFrameTest;
import com.r4intellij.run.debug.stack.RXStackTest;
import com.r4intellij.run.debug.stack.RXSuspendContextTest;
import com.r4intellij.run.graphics.REmptyGraphicsStateTest;
import com.r4intellij.run.graphics.RGraphicsPanelTest;
import com.r4intellij.run.graphics.RGraphicsStateImplTest;
import com.r4intellij.run.run.RRunExecutionResultCalculatorTest;
import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.jetbrains.annotations.NotNull;

public class RTestSuite extends TestCase {

  @NotNull
  public static Test suite() {
    final TestSuite suite = new TestSuite("AllTest");

    suite.addTestSuite(RTypeCheckerInspectionTest.class);
    suite.addTestSuite(RUnresolvedReferenceInspectionTest.class);
    suite.addTestSuite(RUnusedParameterInspectionTest.class);
    suite.addTestSuite(RHighlightingLexerTest.class);
    suite.addTestSuite(RParsingTest.class);
    suite.addTestSuite(RRenameTest.class);

    addDebuggerTests(suite);
    addRunTests(suite);

    return suite;
  }

  private static void addDebuggerTests(@NotNull final TestSuite suite) {
    // evaluator package
    addJUnit4Test(suite, RDebuggerEvaluatorImplTest.class);
    addJUnit4Test(suite, RExpressionHandlerImplTest.class);

    // frame package
    addJUnit4Test(suite, RValueModifierHandlerImplTest.class);
    addJUnit4Test(suite, RValueModifierImplTest.class);
    addJUnit4Test(suite, RVarsLoaderImplTest.class);

    // function package
    addJUnit4Test(suite, RFunctionDebuggerFactoryImplTest.class);
    addJUnit4Test(suite, RBraceFunctionDebuggerTest.class);
    addJUnit4Test(suite, RUnbraceFunctionDebuggerTest.class);
    addJUnit4Test(suite, RTraceAndDebugUtilsTest.class);

    // interpreter package
    addJUnit4Test(suite, RExecutionResultCalculatorImplTest.class);
    addJUnit4Test(suite, RExecutorUtilsTest.class);

    // `main` package
    addJUnit4Test(suite, RDebuggerTest.class);
    addJUnit4Test(suite, RDebuggerStringUtilsTest.class);
    addJUnit4Test(suite, RDebuggerUtilsTest.class);
    addJUnit4Test(suite, RForcedFunctionDebuggerHandlerTest.class);
  }

  private static void addRunTests(@NotNull final TestSuite suite) {
    // configuration package
    suite.addTestSuite(RRunConfigurationTest.class);
    addJUnit4Test(suite, RRunConfigurationEditorTest.class);
    addJUnit4Test(suite, RRunConfigurationTypeTest.class);
    addJUnit4Test(suite, RRunConfigurationUtilsTest.class);

    // debug package
    suite.addTestSuite(RLineBreakpointUtilsTest.class);

    // debug.resolve package
    addJUnit4Test(suite, RFunctionDefinitionProcessorTest.class);
    addJUnit4Test(suite, RResolvingSessionImplTest.class);

    // debug.stack package
    addJUnit4Test(suite, RXPresentationUtilsTest.class);
    addJUnit4Test(suite, RXStackFrameTest.class);
    addJUnit4Test(suite, RXStackTest.class);
    addJUnit4Test(suite, RXSuspendContextTest.class);

    // graphics package
    addJUnit4Test(suite, REmptyGraphicsStateTest.class);
    addJUnit4Test(suite, RGraphicsPanelTest.class);
    suite.addTestSuite(RGraphicsStateImplTest.class);

    // run package
    addJUnit4Test(suite, RRunExecutionResultCalculatorTest.class);

    // `main` package
    addJUnit4Test(suite, RCommandLineCalculatorTest.class);
    addJUnit4Test(suite, ROutputReceiverImplTest.class);
  }

  private static void addJUnit4Test(@NotNull final TestSuite suite, @NotNull final Class<?> cls) {
    suite.addTest(new JUnit4TestAdapter(cls));
  }
}
