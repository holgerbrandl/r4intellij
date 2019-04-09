package com.r4intellij.settings

import com.intellij.ide.plugins.PluginManager
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationInfo
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.PermanentInstallationID
import com.intellij.openapi.components.BaseComponent
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.event.EditorFactoryEvent
import com.intellij.openapi.editor.event.EditorFactoryListener
import com.intellij.openapi.extensions.PluginId
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.util.JDOMUtil
import com.intellij.openapi.util.SystemInfo
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.util.io.HttpRequests
import com.r4intellij.RFileType
import org.jdom.JDOMException
import java.net.URLEncoder
import java.util.concurrent.TimeUnit

private const val KEY = "r.last.update.timestamp"
private const val PLUGIN_ID = "R4Intellij"

class RUpdateComponent : BaseComponent, Disposable {

  private val myListener = object : EditorFactoryListener {
    override fun editorReleased(p0: EditorFactoryEvent) {
    }

    override fun editorCreated(event: EditorFactoryEvent) {
      val document = event.editor.document

      val file = FileDocumentManager.getInstance().getFile(document)
      if (file != null && (file.fileType is RFileType)) {
        checkForUpdates()
      }
    }
  }

  override fun initComponent() {
    if (!ApplicationManager.getApplication().isUnitTestMode) {
      EditorFactory.getInstance().addEditorFactoryListener(myListener, this)
    }
  }

  override fun disposeComponent() {

  }

  override fun getComponentName(): String {
    return javaClass.name
  }

  override fun dispose() {
    disposeComponent()
  }

  private fun checkForUpdates() {
    val propertiesComponent = PropertiesComponent.getInstance()
    val lastUpdate = propertiesComponent.getOrInitLong(KEY, 0)
    if (lastUpdate == 0L || System.currentTimeMillis() - lastUpdate > TimeUnit.DAYS.toMillis(1)) {
      ApplicationManager.getApplication().executeOnPooledThread {
        try {
          val buildNumber = ApplicationInfo.getInstance().build.asString()
          val plugin = PluginManager.getPlugin(PluginId.getId(PLUGIN_ID))!!
          val pluginVersion = plugin.version
          val pluginId = plugin.pluginId.idString
          val os = URLEncoder.encode(SystemInfo.OS_NAME + " " + SystemInfo.OS_VERSION, CharsetToolkit.UTF8)
          val uid = PermanentInstallationID.get()
          val url = "https://plugins.jetbrains.com/plugins/list" +
                  "?pluginId=" + pluginId +
                  "&build=" + buildNumber +
                  "&pluginVersion=" + pluginVersion +
                  "&os=" + os +
                  "&uuid=" + uid
          PropertiesComponent.getInstance().setValue(KEY, System.currentTimeMillis().toString())
          HttpRequests.request(url).connect<Any> { request ->
            try {
              JDOMUtil.load(request.reader)
            }
            catch (ignore: JDOMException) {
            }

            null
          }
        }
        catch (ignored: Exception) {
        }
      }
    }
  }
}