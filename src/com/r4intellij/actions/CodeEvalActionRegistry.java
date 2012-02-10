/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.components.AbstractProjectComponent;
import com.intellij.openapi.project.Project;
import com.r4intellij.settings.EvalActionPref;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;


/**
 * @author brandl
 */
public class CodeEvalActionRegistry extends AbstractProjectComponent {

    public CodeEvalActionRegistry(Project project) {
        super(project);

        DefaultActionGroup actionMenu = (DefaultActionGroup) ActionManager.getInstance().getAction("RCodeSnippetActionMenu");

//        DefaultActionGroup myActionGroup = new DefaultActionGroup("R Code Evaluation", true);
//        ActionManager.getInstance().registerAction("MyMenuID", myActionGroup);
//
//        actionMenu.add(myActionGroup);

//        KeyStroke keyStroke = KeyStroke.getKeyStroke("control shift S");
//        KeyboardShortcut shortcut = new KeyboardShortcut(keyStroke, null);
//
//        CustomShortcutSet shortcuts = new CustomShortcutSet(shortcut);
//        AnAction action = new ConfigurableEvalAction("mein haus", "blabal", shortcuts);
//        myActionGroup.add(action, new Constraints(Anchor.LAST, "after"));
//        ActionManager.getInstance().registerAction("customREval1", action);

        RSettings instance = RSettings.getInstance();
        if (instance != null) {
            for (EvalActionPref actionPref : instance.getEvalActionPrefs()) {
                AnAction action = new ConfigurableEvalAction(actionPref.getName(), actionPref.getCode(), CustomShortcutSet.fromString(actionPref.getDefShortCut()));
                actionMenu.add(action, new Constraints(Anchor.LAST, "after"));
                ActionManager.getInstance().registerAction("customRAction" + instance.getEvalActionPrefs().indexOf(actionPref), action);
            }
        }
    }

    @NonNls
    @NotNull
    public String getComponentName() {
        return "CodeEvalActionRegistry";
    }

}
