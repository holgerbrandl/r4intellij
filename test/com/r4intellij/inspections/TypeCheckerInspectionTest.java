package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;

import static com.r4intellij.inspections.InspectionTestUtilKt.errorMissingArg;
import static com.r4intellij.inspections.UnresolvedReferenceInspection.missingImportMsg;

public class TypeCheckerInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();
    }


    public void testNoWarnings() {
        doTest("NoWarnings.R");
    }


    public void testResolveSignatureFromLibraryMethod() {
        // add library to fixture

        // this is working for groovy
//        void testQualifiedRefToInnerClass() {
//            myFixture.addFileToProject('A.groovy', 'class A {class Bb {}}')
//            final PsiReference ref = configureByText('b.groovy', 'A.B<ref>b b = new A.Bb()')
//            assertNotNull(ref.resolve())
//        }

//        myFixture.addFileToProject("base.R", readFileAsString(getSkeletonPath("utils").toPath()));
        createSkeletonLibrary("utils");

//        Module myModule = myFixture.getModule();
//        PsiTestUtil.addLibrary(myModule, 'lib', tempDir.getFile('').path, [] as String[], [''] as String[])

//         myFixture.addFileToProject("Foo.groovy", """\
//        int a = 42
//        int b = 3 //1
//        """)


        doExprTest(errorMissingArg("x", "head()"));
//        doExprTest("<warning descr=\"argument 'x' is missing, with no default\">head()</warning>");
    }


    public void testMultipleFormalArgMatches() {
        doTest("MultipleFormalArgMatches.R");
    }


    public void testUnusedTripleDotArgument() {
        doTest("UnusedTripleDotArgument.R");
    }


    public void testIgnoreNamedTripleDotArgs() {
        doExprTest("myfun=function(a, ...) a; myfun(23, b=4)"); // b should not be tagged
    }


    public void testPipe() {
        createSkeletonLibrary("datasets", "dplyr");
        doExprTest("require(dplyr); iris %>% count()");
    }


    public void testDiamondReassignment() {
        createSkeletonLibrary("datasets", "dplyr", "magrittr");
        doExprTest("require(dplyr); require(magrittr); iris %<>% mutate(foo=1)");
    }


    public void testNamedArgsInStrangeOrder() {
        createSkeletonLibrary("datasets", "dplyr");
        doExprTest("dplyr::inner_join(by='Species', y=iris, x=iris)");
    }


    public void testSkipArgCheckMissingPipeOp() {
        createSkeletonLibrary("datasets", "utils");

        // actually we would just expect a missing import quickfix for %>% here nothing more
        doExprTest("iris %>% head()");
    }


    /**
     * Ignore arguments that are checked as `missing(varName)` in function body
     */
    public void testIgnoreMissingCheckedInBody() {
        createSkeletonLibrary("datasets", "stringr");

        doExprTest("require(stringr); str_replace_all(c('abc', 'def'), c('[ad]' = '!', '[cf]' = '?'))");

        createSkeletonLibrary("datasets", "knitr", "magrittr");
        doExprTest("require(magrittr); iris %>% knitr::kable(caption=\"Positive additions for tricky authors}\" )");
    }


    public void testIgnoreHybridHandler() {
        createSkeletonLibrary("dplyr", "datasets");

        doExprTest("library(dplyr); mutate(iris, foo=row_number()); filter(iris, percent_rank()>.3); " + errorMissingArg("x", "dense_rank()"));
    }


    public void _testTooManyArgs() { //todo v1.2 enable and fix
        createSkeletonLibrary("readr", "datasets");

        doExprTest("require(readr); write_tsv(iris, foo='myfile.txt')"); // tbd add some warning here
    }


    // todo v1.1 reenable
    public void _testIncorrectRequiredNameArg() {
        createSkeletonLibrary("readr", "datasets");

        // should be called path but is call file. The latter is not a valid arf of write_tsv and should raise an error
        doExprTest("require(readr); " + errorMissingArg("path", "write_tsv(iris, file = 'myfile.txt')"));
    }


    // todo v1.1 reenable
    public void _testDontCheckArgsIfFundefIsMissing() {
        createSkeletonLibrary("datasets");
        myFixture.enableInspections(getInspection(), MissingPackageInspection.class, UnresolvedReferenceInspection.class);

        String warnMissingImport = warnMissingImport("mutate", Arrays.asList("dplyr", "plyr", "SparkR", "plotly"));
        doExprTest(warnMissingImport + "(iris, foo=1)");
    }


    private static String warnMissingImport(String symbol, List<String> foundIn) {
        return "<warning descr=\"" + missingImportMsg(symbol, foundIn) + "\">symbol</warning>";
    }


    public void testOptional() {
        doTest("TestOptional.R");
    }

    // tests below require type system to be active, disabled for now until types are brought back


    public void _testWrongTypeParameter() {
        doTest("WrongTypeParameter.R");
    }


    public void _testRule() {
        doTest("Rule.R");
    }


    public void _testGuessReturnFromBody() {
        doTest("GuessReturnFromBody.R");
    }


    public void _testIfElseType() {
        doTest("IfElseType.R");
    }


    public void _testList() {
        doTest("List.R");
    }


    public void _testBinary() {
        doTest("Binary.R");
    }


    public void _testSlice() {
        doTest("Slice.R");
    }


    public void _testVector() {
        doTest("Vector.R");
    }


    public void _testExpectedCharacterFoundNumeric() {
        doTest();
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return TypeCheckerInspection.class;
    }
}
