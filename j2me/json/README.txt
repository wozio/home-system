---------------------------------
    A JSON parser for Java ME
---------------------------------

1. Introduction

   This project contains a JSON parser for Java ME. This is a port of the
   org.json Java library found at json.org to Java ME. It is hosted here
   with permission from json.org.
   
   For more information please see:
     https://meapplicationdevelopers.dev.java.net/mobileajax.html

2. Configurations

   Multiple configuations are available -

   2.1 CLDC 1.0 - This configuration is for devices supporting CLDC 1.0.
       Double and Float are not supported in this configuration and will
       generate an exception if encountered.

   2.2 Consumer configurations - These configurations include only a JSON
       parser - the ability to create JSON from scratch is omitted. Since
       a lot of web services clients do not need to create JSON, using this
       configuration can save space.

   2.3 XML configurations - The org.json.util package contains a small,
       limited XML parser that can be useful in situations where a
       JSR 172-based parser is not available on the device. Note that this
       parser does not support XML namespaces. This parser can be useful when
       it is desirable to switch between XML and JSON formats without
       modifying application code.

3. Required APIs

   JSR 30 - Connected Limited Device Configuration (CLDC) 1.0 or
   JSR 139 - Connected Limited Device Configuration (CLDC) 1.1

4. Tools

   The project can be built easily using the following tools -

   1. Java SE version 6.0 or higher
   2. NetBeans IDE 5.5.1 or higher
   3. NetBeans Mobility Pack 5.5.1 for CLDC or higher

5. Setup

   1. Launch NetBeans

   2. Open the project in NetBeans

   3. Resolve dependencies, if necessary. If the project name is in red,
      resolution is needed. Right-click on the project name and select
      Resolve Reference Problems (towards the bottom of the menu).

   4. Right-click on the project and select Build All Project
      Configurations.
