# FSharpFormat

F# source code formatter, similar to C# formatter in Visual Studio. It formats spacing and indentations. In oposite to [Fantomas](https://github.com/fsprojects/fantomas) it doesn't rearrange source code by adding or removing newlines.
Also one of goals of the project is not to break an existing code and as a reference test to leave intact [VisualFsharp](https://github.com/Microsoft/visualfsharp) Project after formatting.

## Prerequisites

In order to build or run this tool you will need to have [.NET Core (SDK)](https://dotnet.microsoft.com/download) installed.

## Installation

1.  ```dotnet pack```
2.  ```dotnet tool install --global --add-source ./.nupkg fsfmt```

## Usage

In order get the usage, simply invoke the tool with no arguments:
```
$ fsfmt
Usage: fsfmt [OPTION] SOURCE
Formats the SOURCE. If SOURCE is a folder then formats all .fs files within it.
If there is no defined OUTPUT then writes the formatted source code back to the SOURCE.

Options:
 -r, --recurse     Processes the input folder recursively
     --stdout      Writes the formatted source code to the standard output
 -o, --out <FILE>  Writes the formatted source code into <FILE>
 -h, --help        Displays this information
```

## License

This project is subject to the Apache 2.0 License. A copy of this license can be found in [License file](LICENSE) at the root of this repo.
