package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */
@State(name = "RPackageService",
        storages = {
                @Storage(file = "rpackages.xml") // i.e. ~/Library//Caches/IntelliJIdea2016.1/plugins-sandbox/config/options/rpackages.xml
        }
)
public class RPackageService implements PersistentStateComponent<RPackageService> {

    public Set<RPackage> allPackages = ContainerUtil.newConcurrentSet();
//    public Set<RPackage> packages = ContainerUtil.newConcurrentSet();

    public int CRANMirror = 1;

    public List<String> enabledRepositories = Lists.newArrayList();

    public List<String> userRepositories = Lists.newArrayList();


    @Nullable
    @Override
    public RPackageService getState() {
        return this;
    }


    @Override
    public void loadState(RPackageService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    public static RPackageService getInstance() {
        return ServiceManager.getService(RPackageService.class);
    }


    public Set<RPackage> getPackages() {
        if (allPackages.isEmpty()) {
            allPackages.clear();
            allPackages.addAll(LocalRUtil.parseInstalledPackages());
        }

        return getInstance().allPackages;
    }


    @NotNull
    public List<RPackage> getContainingPackages(String functionName) {
        List<RPackage> funPackages = Lists.newArrayList();

        for (RPackage rPackage : getInstance().getPackages()) {
            if (rPackage.hasFunction(functionName)) {
                funPackages.add(rPackage);
            }
        }

        return funPackages;
    }


    @NotNull
    public Collection<RPackage> getDependencies(RPackage somePckge) {
        Collection<RPackage> deps = new HashSet<RPackage>();

        for (String dep : somePckge.getDependencyNames()) {
            RPackage depPckg = getByName(dep);
            if (depPckg == null)
                continue;

            deps.add(depPckg);
        }

        return deps;
    }


    @Nullable
    public RPackage getByName(String packageName) {
        for (RPackage aPackage : getInstance().getPackages()) {
            if (aPackage.getName().equals(packageName)) {
                return aPackage;
            }
        }

        return null;
    }


    public static void setRepositories(@NotNull final List<String> defaultRepositories,
                                       @NotNull final List<String> userRepositories) {
        RPackageService service = getInstance();

        service.enabledRepositories.clear();
        service.enabledRepositories.addAll(defaultRepositories);
        service.userRepositories.clear();
        service.userRepositories.addAll(userRepositories);
    }

}
