package com.r4intellij.inspections;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class UnusedParameterInspectionTest extends RInspectionTest {


    public void testUnusedParameterInspection() {
        CodeInsightTestFixture fixture = doTest(getTestName(true) + ".R");
        List<HighlightInfo> highlightInfos = fixture.doHighlighting();

        // make sure that they show up as unused
        assertNotEmpty(highlightInfos);
        assertEquals(highlightInfos.get(0).type.getAttributesKey(), CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES);
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnusedParameterInspection.class;
    }
}
