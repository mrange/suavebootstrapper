// --------------------------------------------------------------------------------------
// Start up Suave.io
// --------------------------------------------------------------------------------------

#r "../packages/Suave/lib/net40/Suave.dll"
#r "../packages/M3.MiniJson/lib/net40-client/MiniJson.dll"
#r "FSharp.Compiler.Interactive.Settings"

#load "WebPartT.fs"
#load "MyService.fs"

open Microsoft.FSharp.Compiler.Interactive

open Suave
open Suave.Http.Successful
open Suave.Web
open Suave.Types
open System.Net

let port =
  let args = Settings.fsi.CommandLineArgs
  if args.Length < 2 then uint16 8083
  else
    args.[1] |> Sockets.Port.Parse

let serverConfig =
    { defaultConfig with bindings = [ HttpBinding.mk HTTP IPAddress.Loopback port ] }

startWebServer serverConfig MyService.App
