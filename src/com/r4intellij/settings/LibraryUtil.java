package com.r4intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.impl.OrderEntryUtil;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.CommonProcessors;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author Holger Brandl
 */
public class LibraryUtil {

    public static final String R_LIBRARY = "R Library";
    public static final String R_SKELETONS = "R Skeletons";
//    public static final String USER_SKELETONS = "R User Skeletons";


    public static void createLibrary(final String libraryName, @NotNull final java.util.List<String> paths, @NotNull final Project project) {
        ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();

        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                // add all paths to library
                LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);
                Library library = model.getLibraryByName(libraryName);

                if (library == null) {
                    library = model.createLibrary(libraryName);
                }

                fillLibrary(library, paths);
                model.commit();

                Library.ModifiableModel libModel = library.getModifiableModel();
                libModel.commit();


                // attach to modules if not yet present
                for (Module module : ModuleManager.getInstance(project).getModules()) {
                    // https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000160370-How-to-list-module-dependencies-
                    List<Library> moduleLibraries = new ArrayList<>();
                    OrderEnumerator.orderEntries(module).forEachLibrary(new CommonProcessors.CollectProcessor<>(moduleLibraries));

                    if (moduleLibraries.stream().anyMatch(it -> Objects.equals(libraryName, it.getName()))) {
                        continue;
                    }

                    final ModifiableRootModel modifiableModel = modelsProvider.getModuleModifiableModel(module);

                    modifiableModel.addLibraryEntry(library);
                    modelsProvider.commitModuleModifiableModel(modifiableModel);
                }

            }
        });
    }


    private static void fillLibrary(@NotNull final Library lib, @NotNull final List<String> paths) {
        Library.ModifiableModel modifiableModel = lib.getModifiableModel();
        for (String root : lib.getUrls(OrderRootType.CLASSES)) {
            modifiableModel.removeRoot(root, OrderRootType.CLASSES);
        }
        for (String dir : paths) {
            final VirtualFile pathEntry = LocalFileSystem.getInstance().findFileByPath(dir);
            if (pathEntry != null) {
                modifiableModel.addRoot(pathEntry, OrderRootType.CLASSES);
            } else {
                modifiableModel.addRoot("file://" + dir, OrderRootType.CLASSES);
            }
        }
        modifiableModel.commit();
    }


    private static void detachLibrary(final Project project, final String libraryName) {
        final ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();

        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                // add all paths to library
                final LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);

                final Library library = model.getLibraryByName(libraryName);
                if (library != null) {

                    final Module[] modules = ModuleManager.getInstance(project).getModules();
                    for (Module module : modules) {
                        final ModifiableRootModel modifiableModel = modelsProvider.getModuleModifiableModel(module);
                        OrderEntry entry = OrderEntryUtil.findLibraryOrderEntry(modifiableModel, libraryName);
                        if (entry != null) {
                            modifiableModel.removeOrderEntry(entry);
                            modelsProvider.commitModuleModifiableModel(modifiableModel);
                        } else {
                            modelsProvider.disposeModuleModifiableModel(modifiableModel);
                        }
                    }
                    model.removeLibrary(library);
                    model.commit();
                }
            }
        });
    }
}
