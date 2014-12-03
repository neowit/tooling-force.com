package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.utils.FileUtils
import com.sforce.soap.tooling.AuraDefinition

object AuraMember {
    val EXTENSIONS = Set("app", "cmp", "evt", "intf", "js", "css", "auradoc")
    def isSupportedType(file: File): Boolean = {
        //is this file in "aura" folder
        FileUtils.getParentByName(file, Set("aura")) match {
            case Some(x) =>
                val extension = FileUtils.getExtension(file)
                EXTENSIONS.contains(extension)
            case None => false //not in aura folder
        }
    }

    /**
     * @param file - if this file is part of aura bundle then name of the bundle is returned
     * @return
     */
    def getAuraBundleDir(file: File): Option[File] = {
        var currentDir = if (file.isDirectory) file else file.getParentFile
        var dir:Option[File] = None

        while (null != currentDir && "aura" != currentDir.getName) {
            dir = Some(currentDir)
            currentDir = currentDir.getParentFile
        }
        dir
    }

    def getInstance(file: File, session: Session): AuraMember = {
        if (!isSupportedType(file)) {
            throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
        }
        val member = new AuraMember
        member.setSource(file)
        val key = session.getKeyByFile(file)
        session.getData(key).get("Id") match {
            case Some(id) =>
                member.setId(id.asInstanceOf[String])
            case None =>
        }
        member

    }
}
class AuraMember extends AuraDefinition {
    def setSource(file: File) {
        setSource(scala.io.Source.fromFile(file).mkString)
    }

}
