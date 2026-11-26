# Upgrading 2.0 to 2.1
  
*  Build or download the new Nitrogen environment for your architecture and
   preferred web server. Unzip to a new directory.

*  Copy your existing `./etc` directory to the new build. This retains your
   configuration settings.

*  Copy your existing `./site` directory to the new build, **except** for the
   files in `./site/static/nitrogen`. You will want to use the new version of
   any files in `./site/static/nitrogen`.

This will update the Erlang version, the dependencies files, and all of the
Javascript used by Nitrogen. 
   
You will also need to update your code if there are any API level changes.
