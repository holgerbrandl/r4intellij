package com.r4intellij.refactoring;


import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.inspections.PlainTextSplitter;
import com.intellij.spellchecker.inspections.Splitter;
import com.intellij.spellchecker.tokenizer.SpellcheckingStrategy;
import com.intellij.spellchecker.tokenizer.TokenConsumer;
import com.intellij.spellchecker.tokenizer.Tokenizer;
import com.r4intellij.psi.api.RStringLiteralExpression;
import org.jetbrains.annotations.NotNull;

/**
 * @author yole, holgerbrandl
 */

public class RSpellCheckerStrategy extends SpellcheckingStrategy {
    private static class StringLiteralTokenizer extends Tokenizer<RStringLiteralExpression> {
        @Override
        public void tokenize(@NotNull RStringLiteralExpression element, TokenConsumer consumer) {
            System.out.println();

            final Splitter splitter = PlainTextSplitter.getInstance();
//            for (RString stringElement : element.getStringElements()) {
//                final List<TextRange> literalPartRanges;
//                if (stringElement.isFormatted()) {
//                    literalPartRanges = ((PyFormattedStringElement) stringElement).getLiteralPartRanges();
//                } else {
//                    literalPartRanges = Collections.singletonList(stringElement.getContentRange());
//                }
//                final PyStringLiteralDecoder decoder = new PyStringLiteralDecoder(stringElement);
//                final boolean containsEscapes = stringElement.textContains('\\');
//                for (TextRange literalPartRange : literalPartRanges) {
//                    final List<TextRange> escapeAwareRanges;
//                    if (stringElement.isRaw() || !containsEscapes) {
//                        escapeAwareRanges = Collections.singletonList(literalPartRange);
//                    } else {
//                        escapeAwareRanges = ContainerUtil.map(decoder.decodeRange(literalPartRange), x -> x.getFirst());
//                    }
//                    for (TextRange escapeAwareRange : escapeAwareRanges) {
//                        final String valueText = escapeAwareRange.substring(stringElement.getText());
//                        consumer.consumeToken(stringElement, valueText, false, escapeAwareRange.getStartOffset(), TextRange.allOf(valueText), splitter);
//                    }
//                }
//            }
        }
    }

    private static class FormatStringTokenizer extends Tokenizer<RStringLiteralExpression> {
        @Override
        public void tokenize(@NotNull RStringLiteralExpression element, TokenConsumer consumer) {
            System.out.println();
//            String stringValue = element.getStringValue();
//            List<PyStringFormatParser.FormatStringChunk> chunks = PyStringFormatParser.parsePercentFormat(stringValue);
//            Splitter splitter = PlainTextSplitter.getInstance();
//            for (PyStringFormatParser.FormatStringChunk chunk : chunks) {
//                if (chunk instanceof PyStringFormatParser.ConstantChunk) {
//                    int startIndex = element.valueOffsetToTextOffset(chunk.getStartIndex());
//                    int endIndex = element.valueOffsetToTextOffset(chunk.getEndIndex());
//                    String text = element.getText().substring(startIndex, endIndex);
//                    consumer.consumeToken(element, text, false, startIndex, TextRange.allOf(text), splitter);
//                }
//            }
        }
    }

    private final StringLiteralTokenizer myStringLiteralTokenizer = new StringLiteralTokenizer();
    private final FormatStringTokenizer myFormatStringTokenizer = new FormatStringTokenizer();

    @NotNull
    @Override
    public Tokenizer getTokenizer(PsiElement element) {
        if (element instanceof RStringLiteralExpression) {
            PsiElement parent = element.getParent();
//            if (parent instanceof PyBinaryExpression) {
//                PyBinaryExpression binaryExpression = (PyBinaryExpression) parent;
//                if (element == binaryExpression.getLeftExpression() && binaryExpression.getOperator() == PyTokenTypes.PERC) {
//                    return myFormatStringTokenizer;
//                }
//            }
            return myStringLiteralTokenizer;
        }
        return super.getTokenizer(element);
    }
}