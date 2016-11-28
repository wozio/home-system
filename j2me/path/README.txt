------------------------------------------
    An Expression Language for Java ME
------------------------------------------

1. Introduction

   This project contains an evaluator for a very small expression language
   to extract primitive types from structured information. This
   implementation is layered over the org.json.me library.

   This expression language allows applications to extract information from
   structured data returned by web services with minimal effort.
   
   For more information please see:
     https://meapplicationdevelopers.dev.java.net/mobileajax.html

2. Implementation

   The expression language works a lot like a very small subset of XPath -
   the expression syntax uses the dot character for sub-elements and
   square brackets for arrays. Some sample expressions are, for example -
   "photos.photo[1].title", "[0].location", "[1].status.text", etc

3. Required APIs

   JSR 30 - Connected Limited Device Configuration (CLDC) 1.0 or higher

4. Tools

   The project can be built easily using the following tools -

   1. Java SE version 6.0 or higher
   2. NetBeans IDE 5.5.1 or higher
   3. NetBeans Mobility Pack 5.5.1 for CLDC or higher

5. Setup

   1. Launch NetBeans.

   2. Open the project in NetBeans.

   3. Resolve dependencies, if necessary. If the project name is in red,
      resolution is needed. Right-click on the project name and select
      Resolve Reference Problems (towards the bottom of the menu).

      Resolve the location of the json project. Click Resolve,
      navigate to the json project directory and select it. This
      should fix the reference problem.

   4. Right-click on the project and select Build All Project
      Configurations.
