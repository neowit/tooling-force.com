package com.neowit.apex.parser



import scala.collection.mutable
import scala.collection.JavaConversions._

class ApexTree(val tree: mutable.LinkedHashMap[String, Member], val classByClassName: mutable.LinkedHashMap[String, ClassMember]) {

    def this() {
        this(
            new mutable.LinkedHashMap[String, Member](),//identity -> member, e.g. TopLevelClass -> ClassMember
            new mutable.LinkedHashMap[String, ClassMember]() //class-name.toLowerCase => ClassMember
        )

    }

    def getMember(identity: String): Option[Member] = {
        val lowerCaseIdentity = identity.toLowerCase
        if (lowerCaseIdentity.indexOf(".") > 0) {
            //TODO resolve hierarchically
            //resolve OuterClass.InnerClass in two steps
            val identities = lowerCaseIdentity.split("\\.")
            if (identities.length > 1) {
                tree.get(identities.head) match {
                    case Some(outerClassMember) =>
                        outerClassMember.getChild(identities.tail.head)
                    case None => None
                }
            }
            None
            None
        } else {
            tree.get(lowerCaseIdentity)
        }
    }
    def addMember(member: Member): Unit = {
        tree += ((member.getIdentity.toLowerCase, member))
        if (member.isInstanceOf[ClassMember]) {
            classByClassName += (member.getType.toLowerCase -> member.asInstanceOf[ClassMember])
        }
    }

    def getClassMemberByType(classTypeName: String): Option[ClassMember] = {
        val lowerCaseTypeName = classTypeName.toLowerCase
        val typeNames = lowerCaseTypeName.split("\\.")
        if (typeNames.size > 1) {
            //resolve OuterClass.InnerClass in two steps
            classByClassName.get(typeNames.head) match {
                case Some(outerClassMember) =>
                    outerClassMember.getInnerClassByType(typeNames.tail.head)
                case None => None
            }
        } else {
            classByClassName.get(lowerCaseTypeName)
        }
    }
    def dump() {
        for(key <- tree.keySet) {
            println(key + ": " + tree.get(key).get.toString)
        }
    }

    def extend(anotherTree: ApexTree): Unit = {
        tree ++= anotherTree.tree
        classByClassName ++= anotherTree.classByClassName
    }

    override def clone = {
        val _tree = new mutable.LinkedHashMap[String, Member]()//identity -> member, e.g. TopLevelClass -> ClassMember
        val _classByClassName = new mutable.LinkedHashMap[String, ClassMember]() //class-name.toLowerCase => ClassMember
        _tree.putAll(this.tree)
        _classByClassName.putAll(this.classByClassName)
        new ApexTree(_tree, _classByClassName)
    }
}
