module ViktorVan.Fake.Azure

#if !FAKE
#load ".paket/load/main.group.fsx"
#endif

open System

open Fake.Core
open FSharp.Data
open FSharp.Data.JsonExtensions

module Tools =
    let run cmd args workingDir =
        let arguments =
            args
            |> String.split ' '
            |> Arguments.OfArgs
        Command.RawCommand(cmd, arguments)
        |> CreateProcess.fromCommand
        |> CreateProcess.withWorkingDirectory workingDir
        |> CreateProcess.ensureExitCode
        |> Proc.run
        |> ignore

    let runWithResult cmd args =
        let raiseError (res: ProcessResult<ProcessOutput>) =
            if res.ExitCode <> 0 then failwith res.Result.Error
            else res

        CreateProcess.fromRawCommand cmd args
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> raiseError

let private az args = 
    Tools.runWithResult "az" args

let private func workingDir args = 
    Tools.run "func" args workingDir

let private toJson (result:ProcessResult<ProcessOutput>) =
    result.Result.Output
    |> JsonValue.Parse


type ServicePrincipal = ServicePrincipal of string
type ServicePrincipalSecret = ServicePrincipalSecret of string
type Tenant = Tenant of string
module Tenant =
    let create str =
        if String.IsNullOrWhiteSpace str then invalidArg "TenantId" "Cannot be empty"
        else Tenant str

type Location = 
    | WestEurope
    member this.Value = 
        match this with
        | WestEurope -> "westeurope"

module Location =   
    let parse str =
        match str with
        | "westeurope" -> WestEurope
        | _ -> failwith "invalid Azure location"

type ResourceGroupName = ResourceGroupName of string

type StorageAccountName = private StorageAccountName of string
module StorageAccountName =
    let create (str:string) =
        str.ToLower() |> StorageAccountName

type ConnectionString = private ConnectionString of string
module ConnectionString =
    let create str =
        if String.IsNullOrWhiteSpace str then invalidArg "connectionString" "Connection string cannot be empty"
        elif str.StartsWith("\"") || str.EndsWith("\"") then invalidArg "connectionString" "Not correctly parsed"
        else ConnectionString str
    let value (ConnectionString conn) = conn

type ConnectionString with
    member this.Val = ConnectionString.value this


type InstrumentationKey = InstrumentationKey of string

type AccessKey = AccessKey of string
module AccessKey =
    let create str =
        if String.IsNullOrWhiteSpace str then invalidArg "accessKey" "Access key cannot be empty"
        else AccessKey str

type Credentials = Credentials of Username: string * Password: string
module Credentials =
    let create (user, pw) =
        if String.IsNullOrWhiteSpace user then invalidArg "Username" "Cannot be empty"
        elif String.IsNullOrWhiteSpace pw then invalidArg "Password" "Cannot be empty"
        else Credentials (Username = user, Password = pw)

type ServerName = ServerName of string
type DatabaseName = DatabaseName of string
type BlobName = BlobName of string

type StorageSku = 
    | StandardLRS
    member this.Value = 
        match this with
        | StandardLRS -> "Standard_LRS"
    static member Parse (str:string) =
        match str.ToLower() with
        | "standard_lrs" -> StandardLRS
        | _ -> failwith "invalid Storage SKU"
type FunctionsApp = FunctionsApp of string 
module ResourceGroup =
    let create (location:Location) (ResourceGroupName name) =  
        [ "group"; "create"; "--location"; location.Value; "--name"; name ]
        |> az

module Account =
    let login (ServicePrincipal servicePrincipal) (ServicePrincipalSecret secret) (Tenant tenant) = 
        [ "login"; "--service-principal"; "-u"; servicePrincipal; "-p"; secret; "--tenant"; tenant ] 
        |> az
        |> ignore

    let getCurrentTenant() =
        let getTenantId (json:JsonValue) =  
            json?tenantId.AsString()
            |> Tenant.create

        [ "account"; "show" ]
        |> az
        |> toJson
        |> getTenantId

module AppInsights =
    type Properties = { kind: string }
    let create (ResourceGroupName resourceGroup) appName (location:Location)=
        let getInstrumentationKey (json:JsonValue) =
            json?properties?InstrumentationKey.AsString()
            |> InstrumentationKey



        let properties = """{ "kind" : "api" }"""
        ["resource"; "create"; "--resource-group"; resourceGroup; "--resource-type"; "Microsoft.Insights/components"; "--name"; sprintf "%s-appinsights" appName; "--location"; location.Value; "--properties"; properties]
        |> az
        |> toJson
        |> getInstrumentationKey


module Storage =
    let createAccount (ResourceGroupName resourceGroup) (StorageAccountName name) (location:Location) (sku:StorageSku) =
        let getConnectionString (json:JsonValue) =
            json?connectionString.AsString()
            |> ConnectionString.create

        [ "storage"; "account"; "create"; "--name"; name; "--resource-group"; resourceGroup; "--location"; location.Value; "--sku"; sku.Value; "--kind"; "StorageV2" ]
        |> az
        |> ignore

        let connectionString =
            [ "storage"; "account"; "show-connection-string"; "--resource-group"; resourceGroup; "--name"; name]
            |> az
            |> toJson
            |> getConnectionString

        let getKey1 (json:JsonValue) =
            json.AsArray().[0]?value.AsString()
            |> AccessKey.create

        let accessKey =
            [ "storage"; "account"; "keys"; "list"; "-n"; name ]
            |> az
            |> toJson
            |> getKey1

        connectionString, accessKey

    let enableStaticWebhosting (StorageAccountName storageAccountName) errorDocument indexDocument =
        az [ "extension"; "add"; "--name"; "storage-preview" ]
        |> ignore

        [ "storage"; "blob"; "service-properties"; "update"; "--account-name"; storageAccountName; "--static-website"; "--404-document"; errorDocument; "--index-document"; indexDocument ]
        |> az
        |> ignore

    let uploadBatchBlob sourcePath (ConnectionString storageConnectionString) (BlobName blob)=
        [ "storage"; "blob"; "upload-batch"; "-s"; sourcePath; "-d"; blob; "--connection-string"; storageConnectionString ]
        |> az
        |> ignore

    let deleteAll (ConnectionString storageConnectionString) (BlobName blob) =
        let escaped str = str |> sprintf "'%s'"
        [ "storage"
          "blob"
          "delete-batch"
          "--pattern"
          "'*'"
          "--connection-string"
          (escaped storageConnectionString)
          "--source"
          (escaped blob) ]
        |> az
        |> ignore

module Functions =
    let create (ResourceGroupName resourceGroup) (location: Location) (FunctionsApp name) (StorageAccountName storageAccountName) =
        [ "functionapp"; "create"; "--resource-group"; resourceGroup; "--consumption-plan-location"; location.Value; "--name"; name; "--storage-account"; storageAccountName ]
        |> az
        |> ignore

    let setAppSettings (ResourceGroupName resourceGroup) (FunctionsApp functionsApp) settings =

        let settings =
            settings
            |> Map.toList
            |> List.map (fun (key, value) -> sprintf "%s=%s" key value)

        [ "functionapp"; "config"; "appsettings"; "set"; "--resource-group"; resourceGroup; "--name"; functionsApp; "--settings" ] @ settings
        |> az
        |> ignore

    let start workingDir =
        func workingDir "start"
    
    let private setDotNetRuntime workingDir =
        sprintf "settings add FUNCTIONS_WORKER_RUNTIME dotnet" |> func workingDir

    let deploy workingDir (FunctionsApp functionsAppName) =    
        setDotNetRuntime workingDir
        sprintf "azure functionapp publish %s" functionsAppName
        |> func workingDir

    let setLocalSettings workingDir (ConnectionString storageConnection) =   
        setDotNetRuntime workingDir
        sprintf "settings add AzureWebJobsStorage %s" storageConnection |> func workingDir

module Sql =
    let createDatabase (location:Location) (ResourceGroupName resourceGroup) (ServerName serverName) (DatabaseName databaseName) (Credentials (user, pw)) =

        let createServer() =
            [ "sql"; "server"; "create"; "--name"; serverName; "--location"; location.Value; "-g"; resourceGroup; "-u"; user; "-p"; pw ]
            |> az
            |> ignore

        createServer()

        [ "sql"; "db"; "create"; "-g"; resourceGroup; "-s"; serverName; "-n"; databaseName; "-e"; "Basic" ]
        |> az
        |> ignore

    let createFirewallRule (ResourceGroupName resourceGroup) (ServerName serverName) ruleName ipAddress =
        [ "sql"; "server"; "firewall-rule"; "create"; "-g"; resourceGroup; "-s"; serverName; "-n"; ruleName; "--start-ip-address"; ipAddress; "--end-ip-address"; ipAddress ]
        |> az
        |> ignore

    let allowAccessFromAzureServices resourceGroup serverName =
        createFirewallRule resourceGroup serverName "azure-services" "0.0.0.0"

    let getConnectionString (DatabaseName name) =
        let parseConnectionString (result: ProcessResult<ProcessOutput>) =
            result.Result.Output
            |> (fun (str:string) -> str.Trim().Trim('"'))
            |> ConnectionString.create

        [ "sql"; "db"; "show-connection-string"; "--name"; name; "--client"; "ado.net" ]
        |> az
        |> parseConnectionString