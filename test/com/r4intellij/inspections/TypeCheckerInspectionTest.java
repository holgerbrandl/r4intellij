package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

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


        doExprTest("<warning descr=\"argument 'x' is missing, with no default\">head()</warning>");
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

    // todo reenable for v1.1
    public void _testDontCheckArgsIfFundefIsMissing() {
        createSkeletonLibrary("datasets");
        myFixture.enableInspections(getInspection(), MissingPackageInspection.class, UnresolvedReferenceInspection.class);

        doExprTest("<warning descr=\"'mutate' has been detected in a package (dplyr, plyr, SparkR, plotly) which does not seem to be imported yet.\">mutate</warning>(iris, foo=1)");
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
