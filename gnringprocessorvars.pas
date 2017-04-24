unit GNRingProcessorVars;

{$mode objfpc}{$H+}

interface

 uses Classes, SysUtils, bitDataSource, DOM, XMLRead, XMLWrite;

 type TGNRingProcessorSettings = record
                                  Database: string;
                                  Server: string;
                                  UID: string;
                                  PWD: string;
                                 end;

 var ds_brm: TBitDataSource;
     Settings: TGNRingProcessorSettings;

 function GetDefaultSettings: TGNRingProcessorSettings;

 function LoadGNRingProcessorSettings(Stream: TStream): TGNRingProcessorSettings; overload;
 function LoadGNRingProcessorSettings(XMLFile: string): TGNRingProcessorSettings; overload;

 procedure InitDS;
 procedure FreeDs;

implementation

function GetDefaultSettings: TGNRingProcessorSettings;
 begin
  FillChar(result, sizeof(result), 0);
 end;

function GetXMLExpression(Node: TDomNode): string;
 begin
  result:= '';

  if node.ChildNodes.Count = 1 then
   if node.ChildNodes.Item[0].NodeName = '#text' then
    result:= node.ChildNodes.Item[0].NodeValue;
 end;

function LoadGNRingProcessorSettings(Stream: TStream): TGNRingProcessorSettings;
 var doc: TXMLDocument;
     i, k: integer;
     node: TDOMNode;
     node_name: string;
 begin
  result:= GetDefaultSettings;

  doc:= TXMLDocument.Create;
  ReadXMLFile(doc, Stream);

  for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do
   begin
    node:= doc.DocumentElement.ChildNodes.Item[i];
    node_name:= UpperCase(node.NodeName);

    if node_name = 'SERVER'   then result.Server:= GetXMLExpression(node);
    if node_name = 'DATABASE' then result.Database:= GetXMLExpression(node);
    if node_name = 'UID'      then result.UID:= GetXMLExpression(node);
    if node_name = 'PWD'      then result.PWD:= GetXMLExpression(node);
   end;

  doc.Free;
 end;

function LoadGNRingProcessorSettings(XMLFile: string): TGNRingProcessorSettings;
 var FS: TFileStream;
 begin
  FS:= TFileStream.Create(XMLFile, fmOpenRead);
  result:= LoadGNRingProcessorSettings(FS);
  FS.Free;
 end;


procedure InitDS;
 begin
  ds_brm := TbitDataSource.Create;
  ds_brm.CreateConnector('brm', 'MSSQL_Connector');
  ds_brm['brm'].DataConnector.Parameters.Append('server=' + Settings.Server);
  ds_brm['brm'].DataConnector.Parameters.Append('database=' + Settings.Database);
  ds_brm['brm'].DataConnector.Parameters.Append('uid=' + Settings.UID);
  ds_brm['brm'].DataConnector.Parameters.Append('pwd=' + Settings.PWD);
  ds_brm['brm'].DataConnector.SetParameters;
 end;

procedure FreeDs;
 begin
  while ds_brm.Count > 0 do ds_brm.FreeConnector(ds_brm[0].ID);
  ds_brm.Free;
 end;

end.

