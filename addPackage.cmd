cd src\app
dotnet add package %*
cd ..\..
rem this is supposed to try a retore to fix intellisense for the new package, may not work
dotnet restore