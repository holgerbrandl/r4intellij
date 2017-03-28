package com.r4intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.inspections.UnquotedArgsRule;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

// for nice example see /Users/brandl/projects/jb/intellij-community/plugins/maven/src/main/java/org/jetbrains/idea/maven/project/MavenProjectSettings.java


/**
 * @author Holger Brandl
 */
@State(name = "RCodeInsightSettings", storages = @Storage("codeInsightSettings.xml"))
public class RCodeInsightSettings implements PersistentStateComponent<RCodeInsightSettings> {


    private List<UnquotedArgsRule> argRules = null;


    public static RCodeInsightSettings getInstance() {
        return ServiceManager.getService(RCodeInsightSettings.class);
    }


    @Override
    public RCodeInsightSettings getState() {
        //noinspection ReturnOfThis
        return this;
    }


    @Override
    public void loadState(RCodeInsightSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    // todo add ui
    public List<UnquotedArgsRule> getWhitelistModel() {
        if (argRules == null) {
            argRules = parseArgRules();
        }

        return argRules;
    }


    private static List<UnquotedArgsRule> parseArgRules() {
        List<String> whiteList = Arrays.asList(
                "base::with.default[expr]",
                "base::subset.default[subset select]",
                "base::subset.data.frame[subset select]",  // not really applicable without type system
                "base::transform.default[...]",
                "base::transform.data.frame[...]", // not really applicable without type system (see com/r4intellij/psi/references/RResolver.java:209

                // dplyr: summarize cases
                "dplyr::count[... wt sort]",
                "dplyr::summarize[...]",

                "dplyr::group_by[...]",

                // dplyr: manipulate cases
                "dplyr::filter[...]",
                "dplyr::distinct[...]",
                "dplyr::top_n[wt]",
                "dplyr::arrange[...]",

                // dplyr: manipulate variables
                "dplyr::select[...]",
                "dplyr::mutate[...]",
                "dplyr::transmute[...]",
                "dplyr::rename[...]",
                "dplyr::count[...]",
                "dplyr::vars[...]",

                // tidyr
                "tidyr::gather[key value ...]",
                "tidyr::spread[key value]", // specific names denote args as in method signature
                "tidyr::unite[...]", // tilde denotes just triple dot args
                "tidyr::separate[col]",
                "tidyr::drop_na[...]",

                //ggplot
                "ggplot2::aes[*]", // star denotes all

                "tidytext::unnest_tokens[output input]",
                "tidytext::cast_dtm[document term value]",
                "tidytext::bind_tf_idf[term_col]",
                "tidytext::cast_sparse[row column value]"
        );

        // todo how to add "%$%" here?

        return whiteList.stream()
                .map(UnquotedArgsRule::fromString)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }
}
