# Sutil Template for Elmish

The simplest Sutil Elmish application. This gives you a development area and shows how a single page Elmish application is constructed and mounted with minimal styling. See

- src/App/App.fs.
- public/index.html

## Sutil Syntax Samples

[Sutil](https://sutil.dev/)

## Quick Start

```shell
    git clone -s https://github.com/ImaginaryDevelopment/SutilElmishPlay.git
    cd SutilElmishPlay
    dotnet tool restore
    npm install
    npm run start
```

### Build/Publish

```shell
npm run build
```

which will dump into public\bundle.js

`publish.fsx` is for deploying as a static file into another project

### Powershell additions

Some powershell commands may use

```shell
Install-PackageProvider -Name NuGet
```

[NugetProvider](https://stackoverflow.com/questions/51406685/powershell-how-do-i-install-the-nuget-provider-for-powershell-on-a-unconnected)
