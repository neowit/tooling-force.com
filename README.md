# A command line utility for working with salesforce.com Tooling API

Primary goal of tooling-force.com is to provide the means of interacting with
force.com Tooling API from various command line tools and editor plugins like
[vim-force.com](https://github.com/neowit/vim-force.com)


## DESCRIPTION                                             

Current version of this tool is similar in functionality to [Force.com Migration Tool](http://wiki.developerforce.com/page/Force.com_Migration_Tool) with one important difference - it is designed with scripting languages (shell, .bat, etc) in mind. All operations can be done by constructing appropriate command line, no messing with XML is required. Results are returned in easy to parse formats.

## FEATURES

Non project specific:  

- Retrieve - metadata retrieval
- Deploy (all, specific files)
- Run Tests (and report code coverage)
- Describe Metadata - load full metadata description (result is provided in JSON format)
- List Metadata - list members of specific metadata type (e.g. list all Apex Classes)
- ExecuteAnonymous - run arbitrary piece of Apex Code
- ListConflicts - display list of local files which have been updated in SFDC since last deployment

Project specific  
Also supported a "project like" mode - where relevant metadata about deployments and retrievals is cached in a set of local files and makes possible operations like:

- Deploy modified files
- List Modified files
- Conflict checking before deployment (test if file(s) you are trying to deploy have been modified by someone else in SFDC since your last Refresh or Deployment.


## System requirements

[Java](http://java.com/download) JDK/JRE, Version 6.1 or greater

## Installation

Download lastest .jar file from [releases page](https://github.com/scottmotte/tooling-force.com/releases).
Assuming .jar file name is `tooling-force.com-0.1.jar` you can run it like so: 

    java -jar tooling-force.com-0.1.jar
    
This will display main help and list of supported commands/actions. In order to get help for specific command run it like so:  

    java -jar tooling-force.com-0.1.jar --help=<action-name>


## Building tooling-force.com

 - make sure you have [sbt](http://www.scala-sbt.org/) installed.
 - Generate partner-api, apex-api, metadata-api and tooling-api jars using instructions from [Force.com Web Service Connector (WSC)](https://github.com/forcedotcom/wsc)
 - rename and place generated files under lib folder such as the file structure looks like this (assuming API v29.0)

<pre>
	lib/
	----apex-wsdl-29.0.jar
	----partner-wsdl-29.0.jar
	----metadata-wsdl-29.0.jar
	----tooling-wsdl-29.0.jar
	----force-wsc-29.0.0.jar
	project/
	src/
	assembly.sbt
	build.sbt
</pre>
    git clone git@github.com:neowit/tooling-force.com.git
    sbt assembly

##CREDITS                                                     

Author: Andrey Gavrikov 

## Legal stuff

Copyright (c) 2013-2014, Andrey Gavrikov. All rights reserved.

License: LGPL v3 <http://www.gnu.org/licenses/>

Third-Party Licenses:  
* [Force.com Web Service Connector (WSC)](https://github.com/forcedotcom/wsc) - see [LICENSE.md](https://github.com/forcedotcom/wsc/blob/master/LICENSE.md)  
* [Apache Commons-logging](http://commons.apache.org/proper/commons-logging/) - [Apache 2.0 License](http://www.apache.org/licenses/)  

