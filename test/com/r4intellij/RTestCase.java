package com.r4intellij;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.impl.libraries.ProjectLibraryTable;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.PlatformTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.UsefulTestCase;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.testFramework.fixtures.IdeaProjectTestFixture;
import com.intellij.testFramework.fixtures.IdeaTestFixtureFactory;
import com.intellij.testFramework.fixtures.TestFixtureBuilder;
import com.intellij.testFramework.fixtures.impl.LightTempDirTestFixtureImpl;
import com.intellij.util.ArrayUtil;
import com.r4intellij.packages.PackageServiceUtilKt;
import com.r4intellij.settings.LibraryUtil;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NotNull;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.r4intellij.RFileType.DOT_R_EXTENSION;
import static com.r4intellij.packages.RSkeletonGenerator.DEFAULT_PACKAGES;

public abstract class RTestCase extends UsefulTestCase {

  public static final String TEST_DATA_PATH = new File("testData").getAbsolutePath().replace(File.pathSeparatorChar, '/');
  protected CodeInsightTestFixture myFixture;

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();

    PlatformTestCase.doAutodetectPlatformPrefix();

    IdeaTestFixtureFactory factory = IdeaTestFixtureFactory.getFixtureFactory();
    TestFixtureBuilder<IdeaProjectTestFixture> fixtureBuilder = factory.createLightFixtureBuilder();

    final IdeaProjectTestFixture fixture = fixtureBuilder.getFixture();

    myFixture = IdeaTestFixtureFactory
        .getFixtureFactory()
        .createCodeInsightFixture(fixture, new LightTempDirTestFixtureImpl(true));
    myFixture.setUp();
    myFixture.setTestDataPath(getTestDataPath());


    // inject stub index here for more reproducible testing
//        RIndexCache.getTestInstance();
//        RIndexCache.getInstance().refreshIndex();

    // add base packages for testing
    createSkeletonLibrary(ArrayUtil.toStringArray(DEFAULT_PACKAGES));
  }

  protected String getTestDataPath() {
    return TEST_DATA_PATH;
  }

  @Override
  @After
  public void tearDown() throws Exception {
    myFixture.tearDown();
    super.tearDown();
  }

  protected CodeInsightTestFixture doExprTest(@Language("R") @NotNull String expressionList) {
    myFixture.configureByText("a.R", expressionList);
    configureFixture(myFixture);
    myFixture.testHighlighting(true, false, false);

    return myFixture;
  }

  protected void configureFixture(@NotNull CodeInsightTestFixture myFixture) {
  }

  public void addPckgsToSkeletonLibrary(String... packageNames) {
    addPckgsToSkeletonLibrary(myFixture, packageNames);
  }

  public void createSkeletonLibrary(String... packageNames) {
    createSkeletonLibrary(myFixture, packageNames);
  }

  protected static void addPckgsToSkeletonLibrary(CodeInsightTestFixture myFixture, String... packageNames) {
//        fail("not yet ready because we can not fetch the existing library");


//        Library libraryByName = getSkeletonLibrary(myFixture.getModule().getProject());
    LibraryTable libraryTable = ProjectLibraryTable.getInstance(myFixture.getModule().getProject());
    Library libraryByName = libraryTable.getLibraryByName(LibraryUtil.R_SKELETONS);

    if (libraryByName != null) {
      Stream<String> existingLibFiles = Arrays.stream(libraryByName.getFiles(OrderRootType.CLASSES)).
          map(f -> f.getName().replaceFirst("[.][rR]$", ""));
      packageNames = Stream.concat(existingLibFiles, Arrays.stream(packageNames)).toArray(String[]::new);

    }

    createSkeletonLibrary(myFixture, packageNames);

  }

  public static void createSkeletonLibrary(CodeInsightTestFixture myFixture, String... packageNames) {
    Module myModule = myFixture.getModule();

    LocalFileSystem fileSystem = LocalFileSystem.getInstance();

    List<VirtualFile> skeletons = Arrays.stream(packageNames).map(pckgName -> {
      Path skeletonPath = getSkeletonPath(pckgName).toPath();
      VirtualFile fileByPath = fileSystem.findFileByPath(skeletonPath.toAbsolutePath().toString());
      assert fileByPath != null && fileByPath.exists() : "missing package skeleton: " + pckgName;
      return fileByPath;
    }).collect(Collectors.toList());

//        skeletons.stream().filter(it->!it.exists()).collect(Collectors.toList())

    PsiTestUtil.addProjectLibrary(myModule,
        LibraryUtil.R_SKELETONS,
        ArrayUtil.toObjectArray(skeletons, VirtualFile.class));

    // works but makes testing painfully slow
    PackageServiceUtilKt.rebuildIndex(myFixture.getProject());
  }

  @NotNull
  // todo make generic or add test-skeletons to test-data for better portability
  private static File getSkeletonPath(final String pckgName) {
    String localUserSkeletons = TEST_DATA_PATH + "/r_skeletons";
    return new File(localUserSkeletons, pckgName + DOT_R_EXTENSION);
  }
}

