# A command line utility for working with salesforce.com Tooling API

UPDATE Nov. 2013: Current version (v29) of SFDC Tooling API contains too many
problems and limitations to be usable for the purpose of this project, so there
will be no updates here for a while.   
For now [Force.com Migration Tool](http://wiki.developerforce.com/page/Force.com_Migration_Tool) is still the best all round command line tool.

=========================================
Primary goal of tooling-force.com is to provide the means of interacting with
force.com Tooling API from various command line tools and editor plugins like
[vim-force.com](https://github.com/neowit/vim-force.com)


## DESCRIPTION                                             

Nothing interesting here yet. The tool is at its early stages.

## FEATURES

TBD

## System requirements

[Java](http://java.com/download) JDK/JRE, Version 6.1 or greater

## Installation

TBD

##CREDITS                                                     

Author: Andrey Gavrikov 

## Building tooling-force.com

 - Generate partner-api and tooling-api jars using instructions from [Force.com Web Service Connector (WSC)](https://github.com/forcedotcom/wsc)
 - rename and place generated files under lib folder such as the file structure looks like this (assuming API v29.0)
<pre>
	lib
	--force
	----partner-wsdl
	------29.0
	--------partner-wsdl-29.0.jar
	----tooling-wsdl
	------29.0
	--------tooling-wsdl-29.0.jar
	src
</pre>
    git clone git@github.com:neowit/tooling-force.com.git
    mvn clean package

## Legal stuff

Copyright (c) 2013, Andrey Gavrikov. All rights reserved.

License: LGPL v3 <http://www.gnu.org/licenses/>

Third-Party Licenses:  
* [Force.com Web Service Connector (WSC)](https://github.com/forcedotcom/wsc) - see [LICENSE.md](https://github.com/forcedotcom/wsc/blob/master/LICENSE.md)  
* [Apache Commons-logging](http://commons.apache.org/proper/commons-logging/) - [Apache 2.0 License](http://www.apache.org/licenses/)  

