package com.r4intellij.debugger.executor;

import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import static com.r4intellij.debugger.data.RLanguageConstants.LINE_SEPARATOR;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;

public class RExecutionResultCalculatorImpl implements RExecutionResultCalculator {

  @NotNull
  private static final Pattern START_TRACE_PATTERN = Pattern.compile("^" + TRACING_PREFIX + ".* on entry( )*$");

  @NotNull
  private static final Pattern LINE_BREAK_PATTERN = Pattern.compile("(\r|\n|\r\n)");

  @Override
  public boolean isComplete(@NotNull final CharSequence output) {
    return endsLineBreakAndPlusAndSpace(output) || endsLineBreakAndBrowseAndSpace(output);
  }

  @Override
  @NotNull
  public RExecutionResult calculate(@NotNull final CharSequence output, @NotNull final String error) {
    final String[] lines =
      LINE_BREAK_PATTERN.split(output); // Don't forget that first line is command and the last is invitation for the next one

    return calculateResult(
      lines,
      calculateTypeAndResultLineBounds(lines),
      error
    );
  }

  private static boolean endsLineBreakAndPlusAndSpace(@NotNull final CharSequence output) {
    final int length = output.length();

    return isSubsequence(PLUS_AND_SPACE, output, length - PLUS_AND_SPACE.length()) && // ends with PLUS_AND_SPACE
           StringUtil.isLineBreak(output.charAt(length - PLUS_AND_SPACE.length() - 1)); // line break before PLUS_AND_SPACE
  }

  private static boolean endsLineBreakAndBrowseAndSpace(@NotNull final CharSequence output) {
    final int length = output.length();

    if (isSubsequence(BROWSE_SUFFIX, output, length - BROWSE_SUFFIX.length())) { // ends with BROWSE_SUFFIX
      final int index = readDigitsBackward(output, length - BROWSE_SUFFIX.length() - 1); // read digits before BROWSE_SUFFIX

      return index != -1 && // there are symbols before digits
             index != length - BROWSE_SUFFIX.length() - 1 && // there are any digits before BROWSE_SUFFIX
             isSubsequence(BROWSE_PREFIX, output, index - BROWSE_PREFIX.length() + 1) && // there is BROWSE_PREFIX before digits
             StringUtil.isLineBreak(output.charAt(index - BROWSE_PREFIX.length())); // line break before all mentioned above
    }
    else {
      return false;
    }
  }

  @NotNull
  private static RExecutionResult calculateResult(@NotNull final String[] lines,
                                                  @NotNull final TypeAndResultLineBounds typeAndResultLineBounds,
                                                  @NotNull final String error) {
    final StringBuilder sb = new StringBuilder();

    final TextRange preCalculatedRange =
      (typeAndResultLineBounds.myResultEnd <= typeAndResultLineBounds.myResultBegin) ? TextRange.EMPTY_RANGE : null;

    int resultBegin = 0;
    int resultEnd = 0;

    for (int i = 1; i < lines.length - 1; i++) {
      sb.append(lines[i]);

      if (i != lines.length - 2) {
        sb.append(LINE_SEPARATOR);
      }

      if (preCalculatedRange == null) {
        resultBegin += calculateResultBeginAddition(lines, typeAndResultLineBounds.myResultBegin, i);
        resultEnd += calculateResultEndAddition(lines, typeAndResultLineBounds.myResultEnd, i);
      }
    }

    return new RExecutionResult(
      sb.toString(),
      typeAndResultLineBounds.myType,
      preCalculatedRange == null ? new TextRange(resultBegin, resultEnd) : preCalculatedRange,
      error
    );
  }

  @NotNull
  private static TypeAndResultLineBounds calculateTypeAndResultLineBounds(@NotNull final String[] lines) {
    TypeAndResultLineBounds candidate = tryJustPlusAndSpace(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryJustBrowseAndSpace(lines);

    if (candidate != null) {
      return candidate;
    }

    if (!endsBrowseAndSpace(lines)) {
      throw new IllegalArgumentException("Output is incomplete");
    }

    candidate = tryDebugging(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryContinueTrace(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryExitingFrom(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryDebugAt(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryStartTrace(lines);

    if (candidate != null) {
      return candidate;
    }

    candidate = tryUnbraceDebugAt(lines);

    if (candidate != null) {
      return candidate;
    }

    return new TypeAndResultLineBounds(RESPONSE, 0, lines.length);
  }

  private static boolean isSubsequence(@NotNull final CharSequence sequence,
                                       @NotNull final CharSequence text,
                                       final int beginIndex) {
    if (beginIndex < 0 || beginIndex + sequence.length() > text.length()) {
      return false;
    }

    for (int i = 0; i < sequence.length(); i++) {
      if (sequence.charAt(i) != text.charAt(beginIndex + i)) {
        return false;
      }
    }

    return true;
  }

  private static int calculateResultBeginAddition(@NotNull final String[] lines,
                                                  final int resultLineBegin,
                                                  final int i) {
    int result = 0;

    if (i < resultLineBegin) {
      result += lines[i].length();

      if (i != lines.length - 2) {
        result += LINE_SEPARATOR.length();
      }
    }

    return result;
  }

  private static int calculateResultEndAddition(@NotNull final String[] lines,
                                                final int resultLineEnd,
                                                final int i) {
    int result = 0;

    if (i < resultLineEnd) {
      result += lines[i].length();

      if (i != lines.length - 2 && i != resultLineEnd - 1) {
        result += LINE_SEPARATOR.length();
      }
    }

    return result;
  }

  @Nullable
  private static TypeAndResultLineBounds tryJustPlusAndSpace(@NotNull final String[] lines) {
    if (lines.length == 2 && lines[1].equals(PLUS_AND_SPACE)) {
      return new TypeAndResultLineBounds(PLUS, 1, 1);
    }
    else {
      return null;
    }
  }

  @Nullable
  private static TypeAndResultLineBounds tryJustBrowseAndSpace(@NotNull final String[] lines) {
    if (lines.length == 2 && justBrowseAndSpace(lines[1])) {
      return new TypeAndResultLineBounds(EMPTY, 1, 1);
    }
    else {
      return null;
    }
  }

  private static boolean endsBrowseAndSpace(@NotNull final String[] lines) {
    return lines.length > 1 && justBrowseAndSpace(lines[lines.length - 1]);
  }

  @Nullable
  private static TypeAndResultLineBounds tryDebugging(@NotNull final String[] lines) {
    if (lines.length > 1 && lines[1].startsWith(DEBUGGING_IN_PREFIX)) {
      return new TypeAndResultLineBounds(RExecutionResultType.DEBUGGING_IN, 1, 1);
    }
    else {
      return null;
    }
  }

  @Nullable
  private static TypeAndResultLineBounds tryContinueTrace(@NotNull final String[] lines) {
    final int endOffset = -2; // "debugging in..." line and "debug: {..." line

    for (int i = 1; i < lines.length + endOffset - 1; i++) {
      if (lines[i].startsWith(EXITING_FROM_PREFIX)) {
        for (int j = i + 1; j < lines.length; j++) {
          if (lines[j].startsWith(DEBUGGING_IN_PREFIX)) {
            if (i == 1) {
              // result could be located inside trace information between "exiting from ..." and "debugging in..." lines
              return new TypeAndResultLineBounds(CONTINUE_TRACE, i + 1, j);
            }
            else {
              // result could be located before trace information
              return new TypeAndResultLineBounds(CONTINUE_TRACE, 1, i);
            }
          }
        }

        return null;
      }
    }

    return null;
  }

  @Nullable
  private static TypeAndResultLineBounds tryExitingFrom(@NotNull final String[] lines) {
    final List<Integer> exitingFromIndices = new ArrayList<Integer>();

    for (int i = 1; i < lines.length - 1; i++) {
      if (lines[i].startsWith(EXITING_FROM_PREFIX)) {
        exitingFromIndices.add(i);
      }
    }

    if (exitingFromIndices.isEmpty()) {
      return null;
    }

    final RExecutionResultType type = (exitingFromIndices.size() == 1) ? RExecutionResultType.EXITING_FROM : RECURSIVE_EXITING_FROM;

    final Integer firstExitingFrom = exitingFromIndices.get(0);
    final Integer lastExitingFrom = exitingFromIndices.get(exitingFromIndices.size() - 1);

    if (firstExitingFrom == 1) {
      // result could be located between "exiting from ..." lines
      // or between last "exiting from ..." and "debug at #..." lines
      // or just after last "exiting from ..." line if there is no "debug at #..." line

      final Pair<Integer, Integer> resultBetweenExitingFrom = findResultBetweenExitingFrom(exitingFromIndices);

      if (resultBetweenExitingFrom != null) {
        return new TypeAndResultLineBounds(type, resultBetweenExitingFrom.first, resultBetweenExitingFrom.second);
      }

      final int resultLineBegin = lastExitingFrom + 1;
      final int resultLineEnd = findDebugAt(lines, resultLineBegin);

      return new TypeAndResultLineBounds(type, resultLineBegin, resultLineEnd);
    }
    else {
      // result could be located before trace information
      return new TypeAndResultLineBounds(type, 1, firstExitingFrom);
    }
  }

  @Nullable
  private static TypeAndResultLineBounds tryDebugAt(@NotNull final String[] lines) {
    if (lines.length > 2) {
      final int debugAtLine = findDebugAt(lines, 0);
      final boolean debugAtExists = debugAtLine < lines.length - 1;

      if (debugAtExists) {
        return new TypeAndResultLineBounds(RExecutionResultType.DEBUG_AT, 1, debugAtLine);
      }
      else {
        return null;
      }
    }
    else {
      return null;
    }
  }

  @Nullable
  private static TypeAndResultLineBounds tryStartTrace(@NotNull final String[] lines) {
    if (START_TRACE_PATTERN.matcher(lines[1]).find()) {
      final int unbraceFunctionStartTraceLength = 1 // previous command
                                                  + 1 // "Tracing on ... entry"
                                                  + 1 // "[1] \"...\""
                                                  + 1 // "debug: ..,"
                                                  + 1; // invitation for the next command

      if (lines.length == unbraceFunctionStartTraceLength) {
        return new TypeAndResultLineBounds(START_TRACE_UNBRACE, 1, 1);
      }
      else {
        return new TypeAndResultLineBounds(START_TRACE_BRACE, 1, 1);
      }
    }

    return null;
  }

  @Nullable
  private static TypeAndResultLineBounds tryUnbraceDebugAt(@NotNull final String[] lines) {
    if (lines.length > 2 && lines[lines.length - 2].startsWith(DEBUG_AT_PREFIX)) {
      return new TypeAndResultLineBounds(RExecutionResultType.DEBUG_AT, 1, lines.length - 2);
    }
    else {
      return null;
    }
  }

  private static int readDigitsBackward(@NotNull final CharSequence sequence, final int beginIndex) {
    if (sequence.length() <= beginIndex || beginIndex <= -1) {
      return beginIndex;
    }

    for (int i = beginIndex; i > -1; i--) {
      if (!Character.isDigit(sequence.charAt(i))) {
        return i;
      }
    }

    return -1;
  }

  private static boolean justBrowseAndSpace(@NotNull final String line) {
    return line.startsWith(BROWSE_PREFIX) &&
           line.endsWith(BROWSE_SUFFIX) &&
           isDigits(line, BROWSE_PREFIX.length(), line.length() - BROWSE_SUFFIX.length() - 1);
  }

  @Nullable
  private static Pair<Integer, Integer> findResultBetweenExitingFrom(@NotNull final List<Integer> exitingFromIndices) {
    final ListIterator<Integer> curIterator = exitingFromIndices.listIterator();
    final ListIterator<Integer> nextIterator = exitingFromIndices.listIterator(1);

    while (curIterator.hasNext() && nextIterator.hasNext()) {
      final Integer current = curIterator.next();
      final Integer next = nextIterator.next();

      if (next - current > 1) {
        return Pair.create(current + 1, next);
      }
    }

    return null;
  }

  private static int findDebugAt(@NotNull final String[] lines, final int index) {
    int result = index;

    while (result < lines.length - 1 && !lines[result].startsWith(DEBUG_AT_LINE_PREFIX)) {
      result++;
    }

    return result;
  }

  private static boolean isDigits(@NotNull final CharSequence sequence, final int beginIndex, final int endIndex) { // [l..r]
    return beginIndex >= 0 &&
           endIndex >= beginIndex &&
           endIndex < sequence.length() &&
           readDigitsBackward(sequence, endIndex) == beginIndex - 1;
  }

  private static class TypeAndResultLineBounds {

    @NotNull
    private final RExecutionResultType myType;

    private final int myResultBegin;

    private final int myResultEnd;

    public TypeAndResultLineBounds(@NotNull final RExecutionResultType type, final int resultBegin, final int resultEnd) {
      myType = type;
      myResultBegin = resultBegin;
      myResultEnd = resultEnd;
    }
  }
}
